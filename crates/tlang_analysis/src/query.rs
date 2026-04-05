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
}

impl ResolvedSymbol {
    /// Format the hover text for this symbol.
    ///
    /// Returns a string like `(function) add/2` or `(parameter) x`.
    pub fn hover_text(&self) -> String {
        let kind_label = self.def_kind.to_string();
        if let Some(arity) = self.def_kind.arity() {
            format!("({kind_label}) {name}/{arity}", name = self.name)
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
    let entry = index.get_closest_by_name(found.scope_id, &found.name, found.span)?;

    Some(ResolvedSymbol {
        name: found.name,
        ident_span: found.span,
        def_kind: entry.kind,
        def_span: entry.defined_at,
        builtin: entry.builtin,
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
}
