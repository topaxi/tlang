use insta::assert_snapshot;
use tlang_hir::{ExprKind, PatKind, StmtKind, TyKind};

mod common;

use self::common::{hir_from_str_analyzed, pretty_print};

#[test]
fn test_enum_param_type_inferred_from_variants() {
    let hir = hir_from_str_analyzed(
        r#"
            enum LinkedList {
                Empty,
                Node(int, LinkedList),
            }

            fn sum_list(LinkedList::Empty) { 0 }
            fn sum_list(LinkedList::Node(value, rest)) {
                value + sum_list(rest)
            }
        "#,
    );

    assert_snapshot!(pretty_print(&hir), @r###"
    enum LinkedList {
        Empty
        Node {
            0: int,
            1: LinkedList,
        }
    }
    fn sum_list(linkedlist: LinkedList) -> unknown {
        match linkedlist {
            LinkedList::Empty => {
                0
            },
            LinkedList::Node { 0: value, 1: rest } => {
                (value + sum_list(rest))
            },
        }
    }
    "###);
}

#[test]
fn test_enum_param_type_not_inferred_when_mixed() {
    // If different declarations at the same position use different enum types,
    // a union type should be inferred.
    let hir = hir_from_str_analyzed(
        r#"
            enum Foo { A }
            enum Bar { B }

            fn f(Foo::A) { 1 }
            fn f(Bar::B) { 2 }
        "#,
    );

    let output = pretty_print(&hir);
    assert!(
        output.contains("f(arg0: Foo | Bar)"),
        "Expected union type when enum types differ, got:\n{output}"
    );
}

/// Helper: given a module, find the first function declaration and return
/// the match arms from its body (expects the body to be a single match
/// expression, as produced by multi-clause function lowering).
fn match_arms_of_first_fn(module: &tlang_hir::Module) -> &[tlang_hir::MatchArm] {
    let func = module
        .block
        .stmts
        .iter()
        .find_map(|s| match &s.kind {
            StmtKind::FunctionDeclaration(f) => Some(f.as_ref()),
            _ => None,
        })
        .expect("expected a function declaration");
    let expr = func.body.expr.as_ref().expect("expected a body expression");
    match &expr.kind {
        ExprKind::Match(_, arms) => arms,
        _ => panic!("expected a match expression in function body"),
    }
}

#[test]
fn test_enum_param_type_propagated_to_match_arm_patterns() {
    // Verify that inferred type annotations survive lowering and appear on the
    // match-arm patterns, not just on the merged function parameters.
    let hir = hir_from_str_analyzed(
        r#"
            enum LinkedList {
                Empty,
                Node(int, LinkedList),
            }

            fn sum_list(LinkedList::Empty) { 0 }
            fn sum_list(LinkedList::Node(value, rest)) {
                value + sum_list(rest)
            }
        "#,
    );

    let arms = match_arms_of_first_fn(&hir);
    assert_eq!(arms.len(), 2);

    // Each arm's top-level pattern should carry the inferred LinkedList type.
    for (i, arm) in arms.iter().enumerate() {
        match &arm.pat.ty.kind {
            TyKind::Path(path) => {
                let name = path.segments.last().unwrap().ident.to_string();
                assert_eq!(
                    name, "LinkedList",
                    "arm {i}: expected LinkedList, got {name}"
                );
            }
            other => panic!("arm {i}: expected TyKind::Path, got {other:?}"),
        }
    }
}

#[test]
fn test_literal_param_type_propagated_to_match_arm_patterns() {
    // Multi-clause function with literal patterns should infer builtin types
    // and propagate them to match arm patterns.
    let hir = hir_from_str_analyzed(
        r#"
            fn fib(0) { 0 }
            fn fib(1) { 1 }
            fn fib(n) { fib(n - 1) + fib(n - 2) }
        "#,
    );

    let arms = match_arms_of_first_fn(&hir);
    assert_eq!(arms.len(), 3);

    // Arms with literal/identifier patterns should carry the inferred i64 type.
    for (i, arm) in arms.iter().enumerate() {
        match &arm.pat.ty.kind {
            TyKind::Path(path) => {
                let name = path.segments.last().unwrap().ident.to_string();
                assert_eq!(name, "i64", "arm {i}: expected i64, got {name}");
            }
            other => panic!("arm {i}: expected TyKind::Path, got {other:?}"),
        }
    }
}

#[test]
fn test_multi_param_type_propagated_to_inner_patterns() {
    // For multi-parameter functions, the inner patterns within the list
    // pattern should each carry the inferred type.
    let hir = hir_from_str_analyzed(
        r#"
            fn add(0, b) { b }
            fn add(a, 0) { a }
            fn add(a, b) { a + b }
        "#,
    );

    let arms = match_arms_of_first_fn(&hir);
    assert_eq!(arms.len(), 3);

    // Each arm has a list pattern wrapping two inner patterns.
    for (i, arm) in arms.iter().enumerate() {
        let inner = match &arm.pat.kind {
            PatKind::List(pats) => pats,
            other => panic!("arm {i}: expected PatKind::List, got {other:?}"),
        };
        assert_eq!(inner.len(), 2, "arm {i}: expected 2 inner patterns");
        for (j, pat) in inner.iter().enumerate() {
            match &pat.ty.kind {
                TyKind::Path(path) => {
                    let name = path.segments.last().unwrap().ident.to_string();
                    assert_eq!(name, "i64", "arm {i}, param {j}: expected i64, got {name}");
                }
                other => panic!("arm {i}, param {j}: expected TyKind::Path, got {other:?}"),
            }
        }
    }
}
