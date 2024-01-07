#[derive(Debug, PartialEq, Clone)]
pub struct LineColumn {
    pub line: usize,
    pub column: usize,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Span {
    pub start: LineColumn,
    pub end: LineColumn,
}
