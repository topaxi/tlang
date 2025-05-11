use std::ops::Index;

#[cfg(feature = "serde")]
use serde::Serialize;

#[derive(Clone, Copy, Debug, Default, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct LineColumn {
    pub index: u32,
    pub line: u32,
    pub column: u32,
}

impl std::fmt::Display for LineColumn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
}

impl Span {
    pub fn new(start: LineColumn, end: LineColumn) -> Self {
        Self { start, end }
    }
}

impl Index<Span> for str {
    type Output = str;

    fn index(&self, span: Span) -> &Self::Output {
        let start = span.start.index as usize;
        let end = span.end.index as usize;
        &self[start..end]
    }
}
