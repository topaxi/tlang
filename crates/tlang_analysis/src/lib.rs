//! High-level diagnostic analysis pipeline shared between the LSP server
//! and the WASM playground bindings.
//!
//! Both consumers need to:
//! 1. Parse the source.
//! 2. Configure a [`SemanticAnalyzer`] with the appropriate builtin symbols.
//! 3. Run semantic analysis.
//! 4. Collect diagnostics.
//!
//! This crate provides a single canonical implementation of that pipeline so
//! both the LSP server and the WASM bindings stay in sync automatically.
//!
//! ## Entry points
//!
//! | Function | Use case |
//! |---|---|
//! | [`analyze`] | Parse + semantic analysis with a custom analyzer setup |
//! | [`analyze_for_target`] | As above, dispatching on [`CompilationTarget`] |
//! | [`analyze_with_js_symbols`] | As above, pre-configured for the JS backend |
//! | [`analyze_with_interpreter_symbols`] | As above, pre-configured for the interpreter |
//! | [`configure_js_analyzer`] | Configure an existing analyzer with JS stdlib symbols |
//! | [`configure_interpreter_analyzer`] | Configure an existing analyzer with VM symbols |

pub mod find_node;
pub mod inlay_hints;
pub mod query;
pub mod symbol_index;

use tlang_ast::node::Module;
use tlang_diagnostics::Diagnostic;
use tlang_parser::error::ParseIssue;
use tlang_parser::{ParseMeta, Parser};
use tlang_semantics::SemanticAnalyzer;

/// The result of running the full analysis pipeline on a source string.
///
/// On a parse failure `module` and `parse_meta` are `None`; `parse_issues`
/// carries the reported errors.  On success `analyzer` holds the
/// post-analysis [`SemanticAnalyzer`] so callers that need to continue with
/// HIR lowering can reuse it directly.
pub struct AnalysisResult {
    /// The parsed (and semantically analyzed) module, or `None` on parse
    /// failure.
    pub module: Option<Module>,
    /// Parser metadata produced alongside the AST (e.g. constant-pool node
    /// ids required for HIR lowering).  `None` when parsing fails.
    pub parse_meta: Option<ParseMeta>,
    /// The configured and executed [`SemanticAnalyzer`].  Consumers that need
    /// to lower the HIR can query its symbol tables and id allocator.
    pub analyzer: SemanticAnalyzer,
    /// Parse errors, if any.  Non-empty means `module` is `None`.
    pub parse_issues: Vec<ParseIssue>,
}

impl AnalysisResult {
    /// Collect all diagnostics (parse issues converted to [`Diagnostic`]
    /// plus semantic diagnostics).
    pub fn all_diagnostics(&self) -> Vec<Diagnostic> {
        let parse_diags = self.parse_issues.iter().map(Diagnostic::from);
        let semantic_diags = self.analyzer.get_diagnostics().into_iter();
        parse_diags.chain(semantic_diags).collect()
    }

    /// Returns `true` if there are any error-level diagnostics.
    pub fn has_errors(&self) -> bool {
        !self.parse_issues.is_empty() || !self.analyzer.get_errors().is_empty()
    }
}

/// Run the full analysis pipeline on `source`.
///
/// `configure` is called with a freshly constructed [`SemanticAnalyzer`]
/// **before** analysis runs, giving the caller a chance to register builtin
/// symbols (e.g. `analyzer.add_builtin_symbols(…)`).
///
/// ```
/// use tlang_analysis::analyze;
///
/// let result = analyze("fn add(a, b) { a + b }", |_analyzer| {});
/// assert!(result.parse_issues.is_empty());
/// ```
pub fn analyze(source: &str, configure: impl FnOnce(&mut SemanticAnalyzer)) -> AnalysisResult {
    let mut analyzer = SemanticAnalyzer::default();
    configure(&mut analyzer);

    let mut parser = Parser::from_source(source);
    match parser.parse() {
        Ok((mut module, parse_meta)) => {
            let _ = analyzer.analyze(&mut module);
            AnalysisResult {
                module: Some(module),
                parse_meta: Some(parse_meta),
                analyzer,
                parse_issues: vec![],
            }
        }
        Err(parse_error) => {
            let issues = parse_error.issues().to_vec();
            AnalysisResult {
                module: None,
                parse_meta: None,
                analyzer,
                parse_issues: issues,
            }
        }
    }
}

/// The compilation target that controls which builtin symbols are registered
/// during semantic analysis.
///
/// Mirrors the two execution backends supported by tlang:
/// - [`CompilationTarget::Js`] — compiles to JavaScript (default).
/// - [`CompilationTarget::Interpreter`] — runs in the tlang VM.
///
/// The default is [`CompilationTarget::Js`].
#[derive(Debug, Default, Clone, Copy, PartialEq, Eq)]
pub enum CompilationTarget {
    /// Compile to JavaScript.  Registers the JS stdlib builtins.
    #[default]
    Js,
    /// Run in the tlang interpreter VM.  Registers the VM builtin symbols.
    Interpreter,
}

/// Run the full analysis pipeline selecting builtins based on `target`.
pub fn analyze_for_target(source: &str, target: CompilationTarget) -> AnalysisResult {
    match target {
        CompilationTarget::Js => analyze_with_js_symbols(source),
        CompilationTarget::Interpreter => analyze_with_interpreter_symbols(source),
    }
}

/// Configure a [`SemanticAnalyzer`] with the JavaScript standard-library
/// symbols.
///
/// This is the same setup used by [`tlang_lsp_server`] and
/// [`analyze_with_js_symbols`].  Useful when the caller needs to reuse an
/// existing long-lived analyzer (e.g. in the WASM bindings where the same
/// analyzer is used across multiple operations).
pub fn configure_js_analyzer(analyzer: &mut SemanticAnalyzer) {
    analyzer.add_builtin_symbols(tlang_builtins_js::symbols());
}

/// Configure a [`SemanticAnalyzer`] with the interpreter (VM) builtin
/// symbols.
///
/// This is the same setup used by [`analyze_with_interpreter_symbols`] and
/// the interpreter runner in the WASM bindings.
pub fn configure_interpreter_analyzer(analyzer: &mut SemanticAnalyzer) {
    analyzer.add_builtin_symbols(tlang_builtins_vm::symbols());
}

/// Run analysis with the JavaScript standard-library symbols pre-registered.
///
/// This matches the setup used by [`tlang_lsp_server`] and is the correct
/// choice for diagnosing code that will be compiled to JavaScript.
pub fn analyze_with_js_symbols(source: &str) -> AnalysisResult {
    analyze(source, configure_js_analyzer)
}

/// Run analysis with the interpreter (VM) builtin symbols pre-registered.
///
/// This matches the setup used by the WASM bindings when running under the
/// interpreter runner.
pub fn analyze_with_interpreter_symbols(source: &str) -> AnalysisResult {
    analyze(source, configure_interpreter_analyzer)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn valid_source_has_no_issues() {
        let result = analyze("fn add(a, b) { a + b }", |_| {});
        assert!(result.parse_issues.is_empty());
        assert!(result.module.is_some());
        assert!(result.parse_meta.is_some());
    }

    #[test]
    fn parse_error_populates_parse_issues() {
        let result = analyze("fn {", |_| {});
        assert!(!result.parse_issues.is_empty());
        assert!(result.module.is_none());
    }

    #[test]
    fn undefined_variable_produces_semantic_diagnostic() {
        let result = analyze("fn f() { x }", |_| {});
        assert!(result.parse_issues.is_empty());
        let diags = result.analyzer.get_diagnostics();
        assert!(!diags.is_empty(), "expected a diagnostic for undefined `x`");
    }

    #[test]
    fn all_diagnostics_combines_parse_and_semantic() {
        // Parse failure – only parse issues.
        let r1 = analyze("fn {", |_| {});
        assert!(!r1.all_diagnostics().is_empty());

        // Semantic failure – only semantic diagnostics.
        let r2 = analyze("fn f() { x }", |_| {});
        assert!(!r2.all_diagnostics().is_empty());
    }

    #[test]
    fn has_errors_is_false_for_valid_code() {
        let result = analyze("fn f(x) { x + 1 }", |_| {});
        assert!(!result.has_errors());
    }

    #[test]
    fn has_errors_is_true_for_parse_error() {
        let result = analyze("fn {", |_| {});
        assert!(result.has_errors());
    }
}
