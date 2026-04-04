//! Find the innermost identifier (or path) at a given source position.
//!
//! The [`NodeFinder`] walks the parsed AST using the standard visitor pattern
//! and returns information about the symbol under the cursor, including which
//! scope the cursor is in so callers can resolve the symbol in the appropriate
//! [`DefScope`].

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
}

/// Walk the AST to find the identifier at the given `(line, column)` position.
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
            });
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

    fn visit_path(&mut self, path: &'ast node::Path, ctx: &mut ()) {
        // Record the full path name if the cursor is on the whole path span,
        // but also walk individual segments so the most-specific match wins.
        visit::walk_path(self, path, ctx);
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
            visit::walk_expr(self, expression, ctx);
        }
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
    use tlang_analysis::analyze;

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
}
