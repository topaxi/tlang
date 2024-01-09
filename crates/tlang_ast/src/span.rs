#[derive(Clone, Debug, PartialEq)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

#[derive(Clone, Debug, PartialEq)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
}

impl Span {
    pub fn new(start: LineColumn, end: LineColumn) -> Self {
        Self { start, end }
    }
}
