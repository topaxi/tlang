use std::error::Error;
use std::fmt::{Display, Formatter};

use tlang_ast::{span::Span, token::Token};

#[derive(Debug, Clone)]
pub struct ParseError {
    issues: Vec<ParseIssue>,
}

impl ParseError {
    pub fn new(issues: Vec<ParseIssue>) -> Self {
        Self { issues }
    }

    pub fn issues(&self) -> &[ParseIssue] {
        &self.issues
    }
}

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.issues
                .iter()
                .map(|issue| issue.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

#[derive(Debug, Clone)]
pub struct ParseIssue {
    pub msg: String,
    pub kind: ParseIssueKind,
    pub span: Span,
}

impl Error for ParseIssue {}

impl Display for ParseIssue {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "ParseError: {} at {}:{}",
            self.msg, self.span.start.line, self.span.start.column
        )
    }
}

#[derive(Debug, Clone)]
pub enum ParseIssueKind {
    UnexpectedToken(Token),
    UnexpectedEof,
}
