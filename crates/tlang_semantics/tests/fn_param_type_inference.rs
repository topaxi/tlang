use tlang_ast::node::{FunctionDeclaration, Res, StmtKind, TyKind};
use tlang_defs::DefKind;
use tlang_parser::Parser;
use tlang_semantics::analyzer::SemanticAnalyzer;

mod common;

/// Parse source and run the full semantic analysis (which includes
/// `FnParamTypeInference`). Returns the first `FunctionDeclarations` group.
fn analyze_fn_decls(source: &str) -> Vec<FunctionDeclaration> {
    analyze_fn_decls_with_builtins(source, &[])
}

fn analyze_fn_decls_with_builtins(
    source: &str,
    builtins: &[(&str, DefKind)],
) -> Vec<FunctionDeclaration> {
    let mut parser = Parser::from_source(source);
    let (mut ast, _) = parser.parse().unwrap();
    let mut analyzer = SemanticAnalyzer::default();
    analyzer.add_builtin_symbols(builtins);
    analyzer.analyze(&mut ast).unwrap();

    ast.statements
        .into_iter()
        .find_map(|stmt| {
            if let StmtKind::FunctionDeclarations(decls) = stmt.kind {
                Some(decls)
            } else {
                None
            }
        })
        .expect("no FunctionDeclarations in source")
}

/// Returns the `TyKind` of parameter `param_idx` in declaration `decl_idx`.
fn param_ty_kind(decls: &[FunctionDeclaration], decl_idx: usize, param_idx: usize) -> &TyKind {
    &decls[decl_idx].parameters[param_idx]
        .type_annotation
        .as_ref()
        .unwrap_or_else(|| panic!("no type annotation on decl {decl_idx} param {param_idx}"))
        .kind
}

/// Returns the `Res` for a `TyKind::Path` annotation.
fn param_ty_res(decls: &[FunctionDeclaration], decl_idx: usize, param_idx: usize) -> Res {
    match param_ty_kind(decls, decl_idx, param_idx) {
        TyKind::Path(p) => p.res,
        other => panic!("expected TyKind::Path, got {other:?}"),
    }
}

// ── Existing enum inference (must still work) ─────────────────────────────────

#[test]
fn test_enum_pattern_inference() {
    let decls = analyze_fn_decls(
        "enum Option { Some(x), None }
         fn f(Option::Some(x)) { x }
         fn f(Option::None) { 0 }",
    );
    assert!(matches!(param_ty_kind(&decls, 0, 0), TyKind::Path(p) if p.to_string() == "Option"));
    assert!(matches!(param_ty_kind(&decls, 1, 0), TyKind::Path(p) if p.to_string() == "Option"));
    // res should point to the enum declaration node
    assert!(matches!(param_ty_res(&decls, 0, 0), Res::Def(_)));
    assert!(matches!(param_ty_res(&decls, 1, 0), Res::Def(_)));
    // both overloads resolve to the same declaration node
    assert_eq!(param_ty_res(&decls, 0, 0), param_ty_res(&decls, 1, 0));
}

// ── Literal pattern inference ─────────────────────────────────────────────────

#[test]
fn test_integer_literal_inference() {
    let decls = analyze_fn_decls(
        "fn fib(0) { 0 }
         fn fib(1) { 1 }
         fn fib(n) { 0 }",
    );
    for (i, _) in decls.iter().enumerate() {
        assert!(
            matches!(param_ty_kind(&decls, i, 0), TyKind::Path(p) if p.to_string() == "i64"),
            "decl {i} param 0 should be i64"
        );
        assert_eq!(
            param_ty_res(&decls, i, 0),
            Res::PrimTy,
            "decl {i} param 0 res should be PrimTy"
        );
    }
}

#[test]
fn test_float_literal_inference() {
    let decls = analyze_fn_decls(
        "fn f(0.0) { 0 }
         fn f(x) { x }",
    );
    assert!(matches!(param_ty_kind(&decls, 0, 0), TyKind::Path(p) if p.to_string() == "f64"));
    assert!(matches!(param_ty_kind(&decls, 1, 0), TyKind::Path(p) if p.to_string() == "f64"));
}

#[test]
fn test_string_literal_inference() {
    let decls = analyze_fn_decls(
        r#"fn greet("hello") { 0 }
           fn greet(s) { 0 }"#,
    );
    assert!(matches!(param_ty_kind(&decls, 0, 0), TyKind::Path(p) if p.to_string() == "String"));
    assert!(matches!(param_ty_kind(&decls, 1, 0), TyKind::Path(p) if p.to_string() == "String"));
}

#[test]
fn test_bool_literal_inference() {
    let decls = analyze_fn_decls(
        "fn f(true) { 1 }
         fn f(false) { 0 }",
    );
    assert!(matches!(param_ty_kind(&decls, 0, 0), TyKind::Path(p) if p.to_string() == "bool"));
    assert!(matches!(param_ty_kind(&decls, 1, 0), TyKind::Path(p) if p.to_string() == "bool"));
}

// ── List / Slice pattern inference ───────────────────────────────────────────

#[test]
fn test_empty_list_pattern_inference() {
    let decls = analyze_fn_decls(
        "fn len([]) { 0 }
         fn len([_, ...xs]) { 1 + len(xs) }",
    );
    assert!(matches!(param_ty_kind(&decls, 0, 0), TyKind::Path(p) if p.to_string() == "Slice"));
    assert!(matches!(param_ty_kind(&decls, 1, 0), TyKind::Path(p) if p.to_string() == "Slice"));
}

#[test]
fn test_list_with_wildcard_inference() {
    let decls = analyze_fn_decls(
        "fn map([], _) { [] }
         fn map([x, ...xs], f) { [f(x), ...map(xs, f)] }",
    );
    // First param: [] and [x,...xs] → Slice
    assert!(matches!(param_ty_kind(&decls, 0, 0), TyKind::Path(p) if p.to_string() == "Slice"));
    assert!(matches!(param_ty_kind(&decls, 1, 0), TyKind::Path(p) if p.to_string() == "Slice"));
    // Second param: _ and f — unconstrained, no annotation
    assert!(decls[0].parameters[1].type_annotation.is_none());
    assert!(decls[1].parameters[1].type_annotation.is_none());
}

// ── Union inference ───────────────────────────────────────────────────────────

#[test]
fn test_string_and_slice_union_inference() {
    // reverse_string-style: "" matches String, [x,...xs] matches Slice
    let decls = analyze_fn_decls(
        r#"fn reverse("", acc) { acc }
           fn reverse([x, ...xs], acc) { reverse(xs, acc) }"#,
    );
    // param 0 should be String | Slice union
    for (i, _) in decls.iter().enumerate() {
        match param_ty_kind(&decls, i, 0) {
            TyKind::Union(paths) => {
                let names: Vec<String> = paths.iter().map(|p| p.to_string()).collect();
                assert!(
                    names.contains(&"String".to_string()),
                    "union missing String"
                );
                assert!(names.contains(&"Slice".to_string()), "union missing Slice");
            }
            other => panic!("expected Union, got {other:?}"),
        }
    }
}

#[test]
fn test_mixed_enum_types_produce_union() {
    let decls = analyze_fn_decls(
        "enum Foo { A }
         enum Bar { B }
         fn f(Foo::A) { 1 }
         fn f(Bar::B) { 2 }",
    );
    for (i, _) in decls.iter().enumerate() {
        match param_ty_kind(&decls, i, 0) {
            TyKind::Union(paths) => {
                let names: Vec<String> = paths.iter().map(|p| p.to_string()).collect();
                assert!(names.contains(&"Foo".to_string()), "union missing Foo");
                assert!(names.contains(&"Bar".to_string()), "union missing Bar");
                // Both enum paths in the union should resolve to their declaration nodes
                for path in paths {
                    assert!(
                        matches!(path.res, Res::Def(_)),
                        "union path '{}' res should be Def",
                        path
                    );
                }
            }
            other => panic!("expected Union for decl {i}, got {other:?}"),
        }
    }
}

// ── No-inference cases ────────────────────────────────────────────────────────

#[test]
fn test_no_inference_when_pattern_blocks() {
    // Rest pattern at top level — cannot infer type
    let decls = analyze_fn_decls(
        "fn f(x) { 0 }
         fn f(y) { 1 }",
    );
    assert!(decls[0].parameters[0].type_annotation.is_none());
    assert!(decls[1].parameters[0].type_annotation.is_none());
}

#[test]
fn test_literal_and_enum_mix_produces_union() {
    // i64 literal + enum variant → produces i64 | MyEnum union
    let decls = analyze_fn_decls(
        "enum MyEnum { A }
         fn f(0) { 0 }
         fn f(MyEnum::A) { 1 }",
    );
    for (i, _) in decls.iter().enumerate() {
        match param_ty_kind(&decls, i, 0) {
            TyKind::Union(paths) => {
                let names: Vec<String> = paths.iter().map(|p| p.to_string()).collect();
                assert!(names.contains(&"i64".to_string()), "union missing i64");
                assert!(
                    names.contains(&"MyEnum".to_string()),
                    "union missing MyEnum"
                );
                for path in paths {
                    let expected = if path.to_string() == "i64" {
                        Res::PrimTy
                    } else {
                        // MyEnum should resolve to its declaration node
                        match path.res {
                            Res::Def(_) => path.res,
                            other => panic!("MyEnum path res should be Def, got {other:?}"),
                        }
                    };
                    assert_eq!(path.res, expected, "unexpected res for path '{}'", path);
                }
            }
            other => panic!("expected Union for decl {i}, got {other:?}"),
        }
    }
}

// ── Impl block method type inference ─────────────────────────────────────────

/// Parse source and run semantic analysis. Returns methods for the named
/// protocol method from the first impl block found.
fn analyze_impl_methods(source: &str, method_name: &str) -> Vec<FunctionDeclaration> {
    let mut parser = Parser::from_source(source);
    let (mut ast, _) = parser.parse().unwrap();
    let mut analyzer = SemanticAnalyzer::default();
    analyzer.analyze(&mut ast).unwrap();

    ast.statements
        .into_iter()
        .find_map(|stmt| {
            if let StmtKind::ImplBlock(impl_block) = stmt.kind {
                let methods: Vec<FunctionDeclaration> = impl_block
                    .methods
                    .into_iter()
                    .filter(|m| m.name() == method_name)
                    .collect();
                if !methods.is_empty() {
                    Some(methods)
                } else {
                    None
                }
            } else {
                None
            }
        })
        .expect("no ImplBlock with matching methods in source")
}

#[test]
fn test_impl_block_enum_pattern_inference() {
    let decls = analyze_impl_methods(
        "protocol Greet { fn greet(self) }
         enum Animal { Dog(name), Cat(name) }
         impl Greet for Animal {
           fn greet(Animal::Dog(name)) { name }
           fn greet(Animal::Cat(name)) { name }
         }",
        "greet",
    );
    assert_eq!(decls.len(), 2);
    for (i, _) in decls.iter().enumerate() {
        match param_ty_kind(&decls, i, 0) {
            TyKind::Path(p) => {
                assert_eq!(p.to_string(), "Animal");
                assert!(
                    matches!(p.res, Res::Def(_)),
                    "Animal should resolve to its declaration node"
                );
            }
            other => panic!("expected TyKind::Path(Animal) for decl {i}, got {other:?}"),
        }
    }
}

#[test]
fn test_impl_block_multi_method_inference_independent() {
    let greet_decls = analyze_impl_methods(
        "protocol Greet { fn greet(self) fn shout(self) }
         enum Animal { Dog(name), Cat(name) }
         impl Greet for Animal {
           fn greet(Animal::Dog(name)) { name }
           fn greet(Animal::Cat(name)) { name }
           fn shout(Animal::Dog(name)) { name }
           fn shout(Animal::Cat(name)) { name }
         }",
        "greet",
    );
    assert_eq!(greet_decls.len(), 2);
    for (i, _) in greet_decls.iter().enumerate() {
        match param_ty_kind(&greet_decls, i, 0) {
            TyKind::Path(p) => assert_eq!(p.to_string(), "Animal"),
            other => panic!("expected Animal for greet decl {i}, got {other:?}"),
        }
    }
}
