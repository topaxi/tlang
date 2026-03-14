use tlang_diagnostics::{render_parse_issues, render_semantic_diagnostics};
use tlang_parser::error::ParseIssue;
use tlang_semantics::diagnostic::Diagnostic;

#[derive(Debug)]
pub enum ParserError {
    #[allow(dead_code)]
    ParseError(Vec<ParseIssue>),
    #[allow(dead_code)]
    DiagnosticError(Vec<Diagnostic>),
}

impl From<tlang_parser::error::ParseError> for ParserError {
    fn from(errors: tlang_parser::error::ParseError) -> Self {
        ParserError::ParseError(errors.issues().to_vec())
    }
}

impl From<Vec<Diagnostic>> for ParserError {
    fn from(errors: Vec<Diagnostic>) -> Self {
        ParserError::DiagnosticError(errors)
    }
}

impl ParserError {
    pub fn render(&self, source_name: &str, source: &str) -> String {
        match self {
            ParserError::ParseError(issues) => render_parse_issues(source_name, source, issues),
            ParserError::DiagnosticError(diagnostics) => {
                render_semantic_diagnostics(source_name, source, diagnostics)
            }
        }
    }
}
