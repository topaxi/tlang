use serde::Serialize;

use crate::token::Token;

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

impl Default for LineColumn {
    fn default() -> Self {
        Self { line: 0, column: 0 }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
}

impl Span {
    pub fn new(start: LineColumn, end: LineColumn) -> Self {
        Self { start, end }
    }

    pub fn from_token(token: &Token) -> Self {
        token.span.clone()
    }

    pub fn from_start_token(start: &Token) -> Self {
        Self {
            start: start.span.start.clone(),
            end: LineColumn::default(),
        }
    }

    pub fn end_by_token(&mut self, end: &Token) {
        self.end = end.span.end.clone();
    }
}

impl Default for Span {
    fn default() -> Self {
        Self {
            start: LineColumn::default(),
            end: LineColumn::default(),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Serialize)]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}
