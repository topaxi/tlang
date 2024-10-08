use serde::Serialize;

use crate::token::Token;

#[derive(Clone, Copy, Debug, Default, PartialEq, Serialize)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Serialize)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
}

impl Span {
    pub fn new(start: LineColumn, end: LineColumn) -> Self {
        Self { start, end }
    }

    pub fn from_token(token: &Token) -> Self {
        token.span
    }

    pub fn end_by_token(&mut self, end: &Token) {
        self.end = end.span.end;
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}
