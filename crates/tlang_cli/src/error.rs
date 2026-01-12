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
