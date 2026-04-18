//! High-level symbol query API for hover and goto-definition.
//!
//! This module provides [`resolve_symbol`] which combines the AST node finder
//! and symbol index to resolve the symbol under a cursor position.  Both the
//! LSP server and the WASM playground bindings share this implementation to
//! ensure consistent behaviour.

use tlang_ast::node as ast;
use tlang_defs::DefKind;
use tlang_span::Span;

use crate::find_node;
use crate::symbol_index::SymbolIndex;

/// Information about a symbol resolved from a cursor position.
#[derive(Debug)]
pub struct ResolvedSymbol {
    /// The identifier name as written in source.
    pub name: String,
    /// The span of the identifier under the cursor.
    pub ident_span: Span,
    /// The kind of the resolved definition.
    pub def_kind: DefKind,
    /// The span where the symbol was defined.
    pub def_span: Span,
    /// Whether the symbol is a builtin (no source location to jump to).
    pub builtin: bool,
    /// Optional inferred type string (e.g. `"i64"`, `"Vector"`).
    pub type_info: Option<String>,
}

impl ResolvedSymbol {
    /// Format the hover text for this symbol.
    ///
    /// When type information is available, includes it:
    /// `(variable) v1: Vector` or `(function) add/2`.
    pub fn hover_text(&self) -> String {
        let kind_label = self.def_kind.to_string();
        if let Some(arity) = self.def_kind.arity() {
            if arity == u16::MAX {
                format!("({kind_label}) {name}/*", name = self.name)
            } else {
                format!("({kind_label}) {name}/{arity}", name = self.name)
            }
        } else if let Some(ty) = &self.type_info {
            format!("({kind_label}) {name}: {ty}", name = self.name)
        } else {
            format!("({kind_label}) {name}", name = self.name)
        }
    }
}

/// Resolve the symbol under the cursor at the given **0-based** `(line, column)`.
///
/// The column is automatically adjusted for the lexer's coordinate system
/// (line 0 uses 0-based columns, subsequent lines use 1-based columns).
///
/// Returns `None` when the cursor is not on an identifier or the identifier
/// cannot be resolved in the symbol index.
pub fn resolve_symbol(
    module: &ast::Module,
    index: &SymbolIndex,
    line: u32,
    column_0based: u32,
) -> Option<ResolvedSymbol> {
    // The lexer uses 0-based columns on line 0 but 1-based columns on
    // subsequent lines (current_column resets to 1 after '\n').  Editor
    // positions are always 0-based, so adjust here.
    let lexer_column = if line > 0 {
        column_0based + 1
    } else {
        column_0based
    };

    let found = find_node::find_node_at_position(module, line, lexer_column)?;

    // Look up the symbol in the scope's symbol table.
    let entry = index
        .get_closest_by_name(found.scope_id, &found.name, found.span)
        .or_else(|| {
            // When the cursor is on a field of a field-access expression
            // (e.g. `v1.add`), the bare name `add` may not be in scope.
            // Try to resolve the base variable to its defining type and
            // attempt a qualified lookup like `Vector::add`.
            let base_name = found.field_base.as_deref()?;
            let base_entry = index.get_closest_by_name(found.scope_id, base_name, found.span)?;

            // The base must be a struct or enum to attempt qualified method lookup.
            if !matches!(base_entry.kind, DefKind::Struct | DefKind::Enum) {
                // The base is a variable/parameter — try to find its type
                // by looking for qualified names ending with `::member_name`.
                return index.find_member_by_suffix(found.scope_id, &found.name, found.span);
            }

            // Base is a type name itself (e.g. `Vector.add` or `Option.map`)
            let qualified = format!("{}::{}", base_name, found.name);
            index.get_closest_by_name(found.scope_id, &qualified, found.span)
        })
        .or_else(|| {
            // Fallback for hovering on a member/field name in a declaration
            // context (e.g. `to_string` in `protocol Display { fn to_string(…) }`
            // or `x` in `struct Vector { x: i64 }`).  The bare name is not in the
            // symbol table but the qualified name (`Display::to_string`,
            // `Vector::x`) is.
            //
            // Only attempt this when `find_node` explicitly flagged the identifier
            // as being in a declaration-name context, to avoid mis-resolving
            // undefined locals to an unrelated `Type::member` in scope.
            if found.is_declaration_name {
                index.find_member_by_suffix(found.scope_id, &found.name, found.span)
            } else {
                None
            }
        })?;

    Some(ResolvedSymbol {
        name: found.name,
        ident_span: found.span,
        def_kind: entry.kind,
        def_span: entry.defined_at,
        builtin: entry.builtin,
        type_info: None,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::symbol_index::SymbolIndex;

    fn setup_and_resolve(source: &str, line: u32, column: u32) -> Option<ResolvedSymbol> {
        let result = crate::analyze(source, |_| {});
        let module = result.module.as_ref()?;
        let index = SymbolIndex::from_analyzer(&result.analyzer);
        resolve_symbol(module, &index, line, column)
    }

    #[test]
    fn resolve_variable_reference() {
        let resolved = setup_and_resolve("fn f(x) { x }", 0, 10);
        assert!(resolved.is_some(), "should resolve `x` reference");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "x");
        assert_eq!(resolved.def_kind, DefKind::Parameter);
        assert!(!resolved.builtin);
    }

    #[test]
    fn resolve_function_name() {
        let resolved = setup_and_resolve("fn hello() { 1 }", 0, 3);
        assert!(resolved.is_some(), "should resolve function name");
        assert_eq!(resolved.unwrap().name, "hello");
    }

    #[test]
    fn resolve_returns_none_on_whitespace() {
        let resolved = setup_and_resolve("fn f() { }", 0, 9);
        assert!(resolved.is_none());
    }

    #[test]
    fn resolve_let_binding_shadowing_parameter() {
        // `let a` shadows parameter `a`. Hovering on the let binding name
        // should resolve to the variable, not the shadowed parameter.
        // `a` in `let a = 0` is at col 16 (0-based).
        let resolved = setup_and_resolve("fn foo(a) { let a = 0; a }", 0, 16);
        assert!(resolved.is_some(), "should resolve `a` in let binding");
        let resolved = resolved.unwrap();
        assert_eq!(
            resolved.def_kind,
            DefKind::Variable,
            "should be a variable, not a parameter"
        );
    }

    #[test]
    fn resolve_let_binding_shadowing_parameter_multiline() {
        // Same but across lines. Hovering on the `a` in `let a = 0` should
        // resolve to the variable, not the outer parameter.
        let source = "fn foo(a) {\n  let a = 0;\n  a\n}";
        // `let a = 0;` is on line 1, `a` is at 0-based col 6
        let resolved = setup_and_resolve(source, 1, 6);
        assert!(
            resolved.is_some(),
            "should resolve `a` in let binding on line 1"
        );
        let resolved = resolved.unwrap();
        assert_eq!(
            resolved.def_kind,
            DefKind::Variable,
            "should be a variable, not a parameter"
        );
    }

    #[test]
    fn resolve_multiline_uses_0based_column() {
        // Caller passes 0-based column; resolve_symbol adjusts for lexer.
        let resolved = setup_and_resolve("fn f() {\n  let x = 1;\n  x\n}", 2, 2);
        assert!(
            resolved.is_some(),
            "should resolve `x` on line 2 with 0-based column"
        );
        assert_eq!(resolved.unwrap().name, "x");
    }

    #[test]
    fn hover_text_for_parameter() {
        let resolved = setup_and_resolve("fn f(x) { x }", 0, 10).unwrap();
        assert_eq!(resolved.hover_text(), "(parameter) x");
    }

    #[test]
    fn hover_text_includes_arity_for_function() {
        let resolved =
            setup_and_resolve("fn add(a, b) { a + b }\nlet _ = add(1, 2);", 0, 3).unwrap();
        assert!(
            resolved.hover_text().contains("function"),
            "hover should mention 'function'"
        );
        assert!(
            resolved.hover_text().contains("add"),
            "hover should contain name"
        );
    }

    #[test]
    fn goto_definition_span_for_parameter() {
        let resolved = setup_and_resolve("fn f(x) { x }", 0, 10).unwrap();
        // Definition span should point to the parameter declaration, not the reference.
        assert!(!resolved.builtin);
        // The def_span.start should be at the parameter `x` position (col 5).
        assert_eq!(resolved.def_span.start_lc.column, 5);
    }

    #[test]
    fn resolve_multi_segment_path_vector_new() {
        // `Vector::new` should resolve as a qualified name.
        // tlang uses `fn Vector::new(...)` syntax for static methods
        let source = "struct Vector { x: i64 }\nfn Vector::new(x: i64) -> Vector { Vector { x } }\nlet v = Vector::new(1);";
        // Hover on `Vector` part of `Vector::new` at line 2, col 8 (0-based)
        let resolved = setup_and_resolve(source, 2, 8);
        assert!(resolved.is_some(), "should resolve Vector::new");
        assert_eq!(resolved.unwrap().name, "Vector::new");
    }

    #[test]
    fn resolve_field_expression_method() {
        // `v1.add(v2)` — hovering on `add` should resolve to `Vector::add`.
        // tlang uses `fn Vector.add(self, ...)` for instance methods
        let source = "struct Vector { x: i64 }\nfn Vector.add(self, other) { self }\nlet v1 = Vector { x: 1 };\nv1.add(v1);";
        // `add` on line 3, col 3 (0-based)
        let resolved = setup_and_resolve(source, 3, 3);
        assert!(
            resolved.is_some(),
            "should resolve field method call `v1.add`"
        );
        let resolved = resolved.unwrap();
        // The method resolves to "Vector::add" in the symbol table.
        assert_eq!(resolved.name, "add");
        assert!(
            resolved.def_kind.arity().is_some(),
            "should resolve to a callable"
        );
    }

    #[test]
    fn resolve_return_type_annotation() {
        // Hovering on `Vector` in `-> Vector` should resolve to the struct.
        let source = "struct Vector { x: i64 }\nfn Vector::new(x: i64) -> Vector { Vector { x } }";
        // "-> Vector" on line 1 — the "Vector" type starts at 0-based col 26
        let resolved = setup_and_resolve(source, 1, 26);
        assert!(
            resolved.is_some(),
            "should resolve return type annotation 'Vector'"
        );
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "Vector");
        assert_eq!(resolved.def_kind, DefKind::Struct);
    }

    #[test]
    fn resolve_parameter_type_annotation() {
        // Hovering on `Vector` in `other: Vector` should resolve to the struct.
        let source =
            "struct Vector { x: i64 }\nfn Vector.add(self, other: Vector) -> Vector { self }";
        // `Vector` type in `other: Vector` on line 1 — the "V" starts at 0-based col 27
        let resolved = setup_and_resolve(source, 1, 27);
        assert!(
            resolved.is_some(),
            "should resolve parameter type annotation 'Vector'"
        );
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "Vector");
        assert_eq!(resolved.def_kind, DefKind::Struct);
    }

    #[test]
    fn resolve_primitive_type_annotation_not_found() {
        // Primitive types like `i64` are not in the symbol index,
        // so they don't resolve to a definition.
        let source = "struct Vector { x: i64 }\nfn Vector::new(x: i64) -> Vector { Vector { x } }";
        // `i64` in `x: i64` on line 1 — starts at 0-based col 18
        let resolved = setup_and_resolve(source, 1, 18);
        assert!(
            resolved.is_none(),
            "primitive type 'i64' should not resolve (not a user-defined symbol)"
        );
    }

    #[test]
    fn resolve_field_access_on_variable() {
        // `other.x` should resolve to the struct field `Vector::x`
        let source = "struct Vector { x: i64, y: i64 }\nfn Vector.add(self, other: Vector) -> Vector {\n  let sum_x = other.x;\n  sum_x\n}";
        // `x` in `other.x` on line 2 — 0-based col 20
        let resolved = setup_and_resolve(source, 2, 20);
        assert!(resolved.is_some(), "field 'x' in 'other.x' should resolve");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.def_kind, DefKind::StructField);
    }

    #[test]
    fn resolve_field_access_on_self() {
        // `self.x` should resolve to the struct field `Vector::x`
        let source =
            "struct Vector { x: i64, y: i64 }\nfn Vector.get_x(self) -> i64 {\n  self.x\n}";
        // `x` in `self.x` on line 2 — 0-based col 7
        let resolved = setup_and_resolve(source, 2, 7);
        assert!(resolved.is_some(), "field 'x' in 'self.x' should resolve");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.def_kind, DefKind::StructField);
    }

    #[test]
    fn resolve_method_call_on_self() {
        // `self.add` should resolve to the method `Vector::add`
        let source = "struct Vector { x: i64 }\nfn Vector.add(self, other: Vector) -> Vector { self }\nfn Vector.add_twice(self, other: Vector) -> Vector {\n  self.add(other)\n}";
        // `add` in `self.add` on line 3 — 0-based col 7
        let resolved = setup_and_resolve(source, 3, 7);
        assert!(
            resolved.is_some(),
            "method 'add' in 'self.add' should resolve"
        );
        let resolved = resolved.unwrap();
        assert!(
            resolved.def_kind.arity().is_some(),
            "resolved symbol should be a function/method"
        );
    }

    #[test]
    fn resolve_enum_method_call_on_variable() {
        // `opt.map(f)` should resolve to the method `Option::map`
        let source = "enum Option<T> {\n  Some(T),\n  None,\n}\nfn Option.map(Option::Some(x), f) { Option::Some(f(x)) }\nfn Option.map(Option::None, _) { Option::None }\nlet opt = Option::Some(1);\nopt.map(fn (x) { x + 1 });";
        // `map` in `opt.map` on line 7 — 0-based col 4
        let resolved = setup_and_resolve(source, 7, 4);
        assert!(
            resolved.is_some(),
            "method 'map' in 'opt.map' should resolve"
        );
        let resolved = resolved.unwrap();
        assert!(
            resolved.def_kind.arity().is_some(),
            "resolved enum method should be callable"
        );
    }

    #[test]
    fn resolve_enum_method_call_on_self() {
        // `self.is_some()` inside an enum method should resolve
        let source = "enum Option<T> {\n  Some(T),\n  None,\n}\nfn Option.is_some(Option::Some(_)) { true }\nfn Option.is_some(Option::None) { false }\nfn Option.check(self) {\n  self.is_some()\n}";
        // `is_some` in `self.is_some` on line 7 — 0-based col 7
        let resolved = setup_and_resolve(source, 7, 7);
        assert!(
            resolved.is_some(),
            "method 'is_some' in 'self.is_some' should resolve"
        );
    }

    #[test]
    fn resolve_protocol_declaration_name() {
        // Hovering on the protocol name should resolve to `DefKind::Protocol`.
        let source = "protocol Display {\n    fn to_string(self) { \"\" }\n}";
        // `Display` starts at col 9 on line 0
        let resolved = setup_and_resolve(source, 0, 9);
        assert!(resolved.is_some(), "protocol name 'Display' should resolve");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "Display");
        assert_eq!(
            resolved.def_kind,
            tlang_defs::DefKind::Protocol,
            "should resolve to DefKind::Protocol"
        );
    }

    #[test]
    fn resolve_protocol_method_in_signature() {
        // Hovering on method name inside a protocol declaration.
        let source = "protocol Display {\n    fn to_string(self) { \"\" }\n}";
        // `to_string` starts at col 7 on line 1 (0-based)
        let resolved = setup_and_resolve(source, 1, 7);
        assert!(
            resolved.is_some(),
            "protocol method 'to_string' should resolve"
        );
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "to_string");
        assert!(
            resolved.def_kind.arity().is_some(),
            "protocol method should have an arity"
        );
    }

    #[test]
    fn resolve_protocol_name_in_impl_block() {
        // Hovering on the protocol name in `impl Display for MyType` should resolve.
        let source = "protocol Display {\n    fn to_string(self) { \"\" }\n}\nstruct MyType {}\nimpl Display for MyType {\n    fn to_string(self) { \"\" }\n}";
        // `Display` in `impl Display for MyType` is on line 4, col 5 (0-based)
        let resolved = setup_and_resolve(source, 4, 5);
        assert!(
            resolved.is_some(),
            "protocol name 'Display' in impl block should resolve"
        );
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "Display");
        assert_eq!(
            resolved.def_kind,
            tlang_defs::DefKind::Protocol,
            "should resolve to DefKind::Protocol"
        );
    }

    #[test]
    fn resolve_recursive_call_in_multi_clause_function() {
        // In the second clause of a multi-clause function, the recursive call
        // `map(xs, f)` should resolve to the function itself.
        let source = "fn map([], _) { [] }\nfn map([x, ...xs], f) { [f(x), ...map(xs, f)] }";

        // Try a range of columns to find where 'map' is in the recursive call.
        // The recursive call is in `...map(xs, f)` on line 1.
        let mut found_and_resolved = false;
        for col in 30..45 {
            let resolved = setup_and_resolve(source, 1, col);
            if let Some(ref r) = resolved
                && r.name == "map"
                && r.def_kind.arity().is_some()
            {
                found_and_resolved = true;
                break;
            }
        }

        assert!(
            found_and_resolved,
            "should resolve recursive `map` call in multi-clause function"
        );
    }

    #[test]
    fn resolve_for_loop_binding_definition() {
        let source = r#"enum Tree {
    Empty,
    Node { value: isize, left: Tree, right: Tree },
}

impl Iterable<isize> for Tree {
    fn iter(self) {
        Iterable::iter(self.to_list())
    }
}

fn Tree.to_list(Tree::Empty) { [] }
fn Tree.to_list(Tree::Node { value, left, right }) {
    [...left, value, ...right]
}

let tree = Tree::Empty;
for x in tree {
    x |> log();
};"#;
        let resolved = setup_and_resolve(source, 17, 4);
        assert!(resolved.is_some(), "should resolve loop binding definition");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "x");
        assert_eq!(resolved.def_kind, DefKind::Variable);
    }

    #[test]
    fn resolve_for_loop_binding_reference() {
        let source = r#"enum Tree {
    Empty,
    Node { value: isize, left: Tree, right: Tree },
}

impl Iterable<isize> for Tree {
    fn iter(self) {
        Iterable::iter(self.to_list())
    }
}

fn Tree.to_list(Tree::Empty) { [] }
fn Tree.to_list(Tree::Node { value, left, right }) {
    [...left, value, ...right]
}

let tree = Tree::Empty;
for x in tree {
    x |> log();
};"#;
        let resolved = setup_and_resolve(source, 18, 4);
        assert!(resolved.is_some(), "should resolve loop binding reference");
        let resolved = resolved.unwrap();
        assert_eq!(resolved.name, "x");
        assert_eq!(resolved.def_kind, DefKind::Variable);
    }
}
