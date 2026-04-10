//! Find the innermost identifier (or path) at a given source position.
//!
//! The [`NodeFinder`] walks the parsed AST using the standard visitor pattern
//! and returns information about the symbol under the cursor, including which
//! scope the cursor is in so callers can resolve the symbol in the appropriate
//! [`DefScope`](tlang_defs::DefScope).

use tlang_ast::node::{self, Module};
use tlang_ast::visit::{self, Visitor};
use tlang_span::{LineColumn, NodeId, Span};

/// The result of a successful node-find operation.
#[derive(Debug)]
pub struct FoundNode {
    /// The name of the identifier under the cursor.
    pub name: String,
    /// The span of the identifier under the cursor.
    pub span: Span,
    /// The innermost scope [`NodeId`] that contains the cursor position.
    /// Use this to look up the symbol in the semantic analyzer's symbol tables.
    pub scope_id: NodeId,
    /// When the cursor is on the field of a field access expression (e.g.
    /// `v1.add`), this holds the name of the base expression (e.g. `"v1"`).
    /// Used by the query layer to attempt qualified method lookup.
    pub field_base: Option<String>,
}

/// Walk the AST to find the identifier at the given `(line, column)` position.
///
/// **Coordinate system**: `line` and `column` must be in the **lexer's**
/// coordinate system where line 0 uses 0-based columns but lines after the
/// first use 1-based columns (the lexer resets `current_column` to 1 after
/// each newline).  Callers converting from an editor position (which is
/// typically always 0-based) should add 1 to `character` when `line > 0`.
///
/// Returns `Some(FoundNode)` when the cursor is on a recognized identifier,
/// or `None` when the position is on whitespace / punctuation / literal.
pub fn find_node_at_position(module: &Module, line: u32, column: u32) -> Option<FoundNode> {
    let mut finder = NodeFinder {
        target: LineColumn::new(line, column),
        scope_stack: vec![module.id],
        result: None,
    };
    finder.visit_module(module, &mut ());
    finder.result
}

/// Internal visitor state.
struct NodeFinder {
    target: LineColumn,
    scope_stack: Vec<NodeId>,
    result: Option<FoundNode>,
}

impl NodeFinder {
    fn current_scope(&self) -> NodeId {
        *self.scope_stack.last().expect("scope stack is never empty")
    }

    /// Check whether the target position falls inside `span`.
    fn contains_position(&self, span: &Span) -> bool {
        let pos = self.target;
        // Single-line span.
        if span.start_lc.line == span.end_lc.line {
            return span.start_lc.line == pos.line
                && span.start_lc.column <= pos.column
                && pos.column < span.end_lc.column;
        }
        // Multi-line span.
        if pos.line == span.start_lc.line {
            return pos.column >= span.start_lc.column;
        }
        if pos.line == span.end_lc.line {
            return pos.column < span.end_lc.column;
        }
        pos.line > span.start_lc.line && pos.line < span.end_lc.line
    }

    fn record_ident(&mut self, name: &str, span: &Span) {
        if self.contains_position(span) {
            self.result = Some(FoundNode {
                name: name.to_string(),
                span: *span,
                scope_id: self.current_scope(),
                field_base: None,
            });
        }
    }

    fn record_field_ident(&mut self, name: &str, span: &Span, base_name: String) {
        if self.contains_position(span) {
            self.result = Some(FoundNode {
                name: name.to_string(),
                span: *span,
                scope_id: self.current_scope(),
                field_base: Some(base_name),
            });
        }
    }

    /// Extract a simple name from an expression (for field access base tracking).
    fn expr_name(expr: &node::Expr) -> Option<String> {
        match &expr.kind {
            node::ExprKind::Path(path) if path.segments.len() == 1 => {
                Some(path.segments[0].to_string())
            }
            _ => None,
        }
    }
}

impl<'ast> Visitor<'ast> for NodeFinder {
    type Context = ();

    fn enter_scope(&mut self, node_id: NodeId, _ctx: &mut ()) {
        self.scope_stack.push(node_id);
    }

    fn leave_scope(&mut self, _node_id: NodeId, _ctx: &mut ()) {
        self.scope_stack.pop();
    }

    fn visit_ident(&mut self, ident: &'ast node::Ident, _ctx: &mut ()) {
        self.record_ident(ident.as_str(), &ident.span);
    }

    fn visit_path(&mut self, path: &'ast node::Path, _ctx: &mut ()) {
        if path.segments.len() > 1 {
            // Multi-segment paths (e.g. "Vector::new", "math::sqrt") are
            // registered as qualified names in the symbol table.  Walking
            // individual segments would overwrite the result with a bare
            // segment name like "new" that can't be resolved.
            //
            // Check each segment's span individually so the cursor position
            // correctly selects the full qualified name only when hovering on
            // one of its segments.
            for seg in &path.segments {
                if self.contains_position(&seg.span) {
                    self.record_ident(&path.to_string(), &path.span);
                    return;
                }
            }
        } else {
            // Single-segment paths: the full path name equals the segment
            // name, so no ambiguity.
            self.record_ident(&path.to_string(), &path.span);
        }
    }

    fn visit_module(&mut self, module: &'ast Module, ctx: &mut ()) {
        visit::walk_module(self, module, ctx);
    }

    fn visit_stmt(&mut self, statement: &'ast node::Stmt, ctx: &mut ()) {
        // Only descend into statements whose span contains the cursor.
        if self.contains_position(&statement.span) {
            visit::walk_stmt(self, statement, ctx);
        }
    }

    fn visit_expr(&mut self, expression: &'ast node::Expr, ctx: &mut ()) {
        // Only descend into expressions whose span contains the cursor.
        if self.contains_position(&expression.span) {
            // For field access expressions (e.g. `v1.add`), if the cursor is
            // on the field name, record the base expression name so the query
            // layer can try qualified method lookup (e.g. `Vector::add`).
            if let node::ExprKind::FieldExpression(ref field_expr) = expression.kind
                && self.contains_position(&field_expr.field.span)
                && let Some(base_name) = Self::expr_name(&field_expr.base)
            {
                self.record_field_ident(
                    field_expr.field.as_str(),
                    &field_expr.field.span,
                    base_name,
                );
                // Still walk the base in case the cursor is actually
                // on the base (the field ident record only sticks if
                // the cursor is on the field span).
                self.visit_expr(&field_expr.base, ctx);
                return;
            }
            visit::walk_expr(self, expression, ctx);
        }
    }

    fn visit_ty(&mut self, ty: &'ast node::Ty, _ctx: &mut ()) {
        // Type annotations (e.g. `-> Vector`, `x: i64`) contain paths that
        // should be hoverable / goto-definitionable.  The default `walk_ty`
        // only descends into generic parameters, not the path itself.
        match &ty.kind {
            node::TyKind::Path(path) => {
                if self.contains_position(&ty.span) {
                    self.record_ident(&path.to_string(), &ty.span);
                }
            }
            node::TyKind::Union(paths) => {
                for path in paths {
                    if self.contains_position(&path.span) {
                        self.record_ident(&path.to_string(), &path.span);
                    }
                }
            }
            node::TyKind::Unknown => {}
            node::TyKind::Fn(params, ret) => {
                for param in params {
                    self.visit_ty(&param.ty, &mut ());
                }
                self.visit_ty(ret, &mut ());
            }
        }
        // Still walk generic type parameters.
        for param in &ty.parameters {
            self.visit_ty(param, &mut ());
        }
    }

    fn visit_fn_ret_ty(&mut self, annotation: &'ast node::Ty, ctx: &mut ()) {
        // The default visit_fn_ret_ty calls walk_ty which only recurses into
        // generic parameters.  We must call visit_ty first so the return type
        // path (e.g. `-> Vector`) is recorded by our visit_ty override.
        self.visit_ty(annotation, ctx);
    }

    fn visit_fn_decl(&mut self, declaration: &'ast node::FunctionDeclaration, ctx: &mut ()) {
        // Walk the declaration using the standard walk which handles
        // enter_scope / leave_scope for the function body.
        visit::walk_fn_decl(self, declaration, ctx);
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::analyze;

    fn parse_and_find(source: &str, line: u32, column: u32) -> Option<FoundNode> {
        let result = analyze(source, |_| {});
        result
            .module
            .as_ref()
            .and_then(|m| find_node_at_position(m, line, column))
    }

    #[test]
    fn find_variable_reference() {
        let source = "fn f(x) { x }";
        // `x` in the body is at line 0, col 10
        let found = parse_and_find(source, 0, 10);
        assert!(found.is_some());
        let found = found.unwrap();
        assert_eq!(found.name, "x");
    }

    #[test]
    fn find_function_name() {
        let source = "fn hello() { 1 }";
        // `hello` starts at col 3
        let found = parse_and_find(source, 0, 3);
        assert!(found.is_some());
        let found = found.unwrap();
        assert_eq!(found.name, "hello");
    }

    #[test]
    fn find_parameter_name() {
        let source = "fn f(abc) { abc }";
        // `abc` parameter at col 5
        let found = parse_and_find(source, 0, 5);
        assert!(found.is_some());
        let found = found.unwrap();
        assert_eq!(found.name, "abc");
    }

    #[test]
    fn find_nothing_on_whitespace() {
        let source = "fn f() { }";
        // whitespace at col 9
        let found = parse_and_find(source, 0, 9);
        assert!(found.is_none());
    }

    #[test]
    fn find_let_binding() {
        let source = "fn f() { let x = 1; x }";
        // `x` in the let is at col 13
        let found = parse_and_find(source, 0, 13);
        assert!(found.is_some());
        assert_eq!(found.unwrap().name, "x");
    }

    #[test]
    fn find_multiline() {
        // The lexer resets `current_column` to 1 (not 0) after a newline,
        // so every column on lines after the first is offset by +1
        // compared to a 0-indexed column.  `x` at two-space indent on
        // line 2 is reported at column 3 by the lexer.
        let source = "fn f() {\n  let x = 1;\n  x\n}";
        let found = parse_and_find(source, 2, 3);
        assert!(found.is_some(), "should find 'x' at (2,3)");
        assert_eq!(found.unwrap().name, "x");
    }

    #[test]
    fn find_multi_segment_path() {
        // tlang uses `fn Vector::new(...)` syntax for static methods
        let source = "struct Vector { x: i64 }\nfn Vector::new(x: i64) -> Vector { Vector { x } }\nlet v = Vector::new(1);";

        let result = analyze(source, |_| {});
        assert!(
            result.module.is_some(),
            "parse failed: {:?}",
            result.parse_issues
        );

        // "Vector::new" call on line 2
        // Scan line 2 to verify Vector::new is found
        let found_cols: Vec<(u32, String)> = (0..25)
            .filter_map(|col| parse_and_find(source, 2, col).map(|f| (col, f.name)))
            .collect();
        let vec_new_entries: Vec<_> = found_cols
            .iter()
            .filter(|(_, name)| name == "Vector::new")
            .collect();
        assert!(
            !vec_new_entries.is_empty(),
            "should find 'Vector::new' on line 2.\nAll found: {:?}",
            found_cols
        );
    }

    #[test]
    fn find_second_segment_of_multi_segment_path() {
        // Hovering over `sqrt` in `math::sqrt(x)` should resolve to
        // `math::sqrt`, not return no result.
        let source = "math::sqrt(4);";

        // `math` is at cols 0..4, `::` at 4..6, `sqrt` at cols 6..10
        let found_cols: Vec<(u32, String)> = (0..14)
            .filter_map(|col| parse_and_find(source, 0, col).map(|f| (col, f.name)))
            .collect();

        // Hovering on `math` (col 0..4) should yield `math::sqrt`
        let on_math = found_cols.iter().find(|(col, _)| *col == 0);
        assert!(
            on_math.is_some() && on_math.unwrap().1 == "math::sqrt",
            "hovering on `math` should resolve to `math::sqrt`.\nAll: {found_cols:?}"
        );

        // Hovering on `sqrt` (col 6..10) should also yield `math::sqrt`
        let on_sqrt = found_cols.iter().find(|(col, _)| *col == 6);
        assert!(
            on_sqrt.is_some() && on_sqrt.unwrap().1 == "math::sqrt",
            "hovering on `sqrt` should resolve to `math::sqrt`.\nAll: {found_cols:?}"
        );
    }

    #[test]
    fn find_multi_segment_path_in_pipeline() {
        // The LHS of a pipeline `Vector::new(1, 2) |> ...` should resolve
        // `Vector::new` on hover, just like a standalone call.
        let source = "\
struct Vector { x: i64, y: i64 }
fn Vector::new(x: i64, y: i64) -> Vector { Vector { x, y } }
fn Vector.add(self, other: Vector) -> Vector { Vector { x: self.x + other.x, y: self.y + other.y } }
let v1 = Vector::new(3, 4);

Vector::new(1, 2)
|> v1.add()
|> log();";

        let result = analyze(source, |_| {});
        assert!(
            result.module.is_some(),
            "parse failed: {:?}",
            result.parse_issues
        );

        // Line 3: `let v1 = Vector::new(3, 4);` — first Vector::new
        // Note: lines > 0 have 1-based columns in the lexer
        let found_line3: Vec<(u32, String)> = (0..30)
            .filter_map(|col| parse_and_find(source, 3, col).map(|f| (col, f.name)))
            .collect();
        let first_vec_new: Vec<_> = found_line3
            .iter()
            .filter(|(_, name)| name == "Vector::new")
            .collect();
        assert!(
            !first_vec_new.is_empty(),
            "should find 'Vector::new' on line 3 (let binding).\nAll found: {:?}",
            found_line3
        );

        // Line 5: `Vector::new(1, 2)` — second Vector::new (pipeline LHS)
        let found_line5: Vec<(u32, String)> = (0..20)
            .filter_map(|col| parse_and_find(source, 5, col).map(|f| (col, f.name)))
            .collect();
        let second_vec_new: Vec<_> = found_line5
            .iter()
            .filter(|(_, name)| name == "Vector::new")
            .collect();
        assert!(
            !second_vec_new.is_empty(),
            "should find 'Vector::new' on line 5 (pipeline LHS).\nAll found: {:?}",
            found_line5
        );
    }

    #[test]
    fn find_field_expression_records_base() {
        // `v1.add` should record field_base = "v1" when cursor is on "add"
        let source = "struct Vector { x: i64 }\nfn Vector.add(self, other) { self }\nlet v1 = Vector { x: 1 };\nv1.add(v1);";

        let result = analyze(source, |_| {});
        assert!(
            result.module.is_some(),
            "parse failed: {:?}",
            result.parse_issues
        );

        // Scan line 3 to find `add` with field_base = "v1"
        let found_items: Vec<(u32, String, Option<String>)> = (0..15)
            .filter_map(|col| parse_and_find(source, 3, col).map(|f| (col, f.name, f.field_base)))
            .collect();

        let add_entry = found_items
            .iter()
            .find(|(_, name, fb)| name == "add" && fb.as_deref() == Some("v1"));
        assert!(
            add_entry.is_some(),
            "should find field 'add' with field_base='v1' on line 3.\nAll found: {:?}",
            found_items
        );
    }

    #[test]
    fn find_return_type_annotation() {
        // `-> Vector` in a function signature should be hoverable.
        let source = "struct Vector { x: i64 }\nfn Vector::new(x: i64) -> Vector { Vector { x } }";

        // Line 1: `fn Vector::new(x: i64) -> Vector { ... }`
        // The return type annotation "Vector" is after "-> "
        let found: Vec<(u32, String)> = (0..50)
            .filter_map(|col| parse_and_find(source, 1, col).map(|f| (col, f.name)))
            .collect();

        // There should be a "Vector" entry from the return type annotation
        let return_type_entries: Vec<_> = found
            .iter()
            .filter(|(col, name)| name == "Vector" && *col > 25)
            .collect();
        assert!(
            !return_type_entries.is_empty(),
            "should find 'Vector' return type annotation on line 1.\nAll found: {:?}",
            found
        );
    }

    #[test]
    fn find_parameter_type_annotation() {
        // `: i64` in a parameter should be hoverable.
        let source = "fn add(x: i64, y: i64) -> i64 { x + y }";

        let found: Vec<(u32, String)> = (0..40)
            .filter_map(|col| parse_and_find(source, 0, col).map(|f| (col, f.name)))
            .collect();

        // There should be "i64" entries from parameter type annotations
        let i64_entries: Vec<_> = found.iter().filter(|(_, name)| name == "i64").collect();
        assert!(
            i64_entries.len() >= 2,
            "should find at least 2 'i64' type annotations.\nAll found: {:?}",
            found
        );
    }
}
