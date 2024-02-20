use std::fmt::{Display, Formatter};

use tlang_ast::{span::Span, token::Token};

#[derive(Debug, Clone, PartialEq)]
pub struct ParseError {
    pub msg: String,
    pub kind: ParseErrorKind,
    pub span: Span,
}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "ParseError: {} at {}:{}",
            self.msg, self.span.start.line, self.span.start.column
        )
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum ParseErrorKind {
    UnexpectedToken(Token),
    UnexpectedEof,
}
