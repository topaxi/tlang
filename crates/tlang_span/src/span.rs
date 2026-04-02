#[cfg(feature = "serde")]
use serde::Serialize;

#[derive(Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct LineColumn {
    pub line: u32,
    pub column: u32,
}

impl LineColumn {
    pub fn new(line: u32, column: u32) -> Self {
        Self { line, column }
    }
}

impl From<(u32, u32)> for LineColumn {
    fn from(tuple: (u32, u32)) -> Self {
        Self::new(tuple.0, tuple.1)
    }
}

impl std::fmt::Display for LineColumn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

impl std::fmt::Debug for LineColumn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "LineColumn({}, {})", self.line, self.column)
    }
}

/// A source span carrying both byte offsets (for OXC/source-map compatibility)
/// and pre-computed line/column (for diagnostic display without needing source text).
#[derive(Clone, Copy, Default)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Span {
    /// Byte offset of the start of this span in the source file.
    pub start: u32,
    /// Byte offset of the end (exclusive) of this span in the source file.
    pub end: u32,
    /// Line/column of `start`, for diagnostic display. Computed by the lexer.
    pub start_lc: LineColumn,
    /// Line/column of `end`, for diagnostic display. Computed by the lexer.
    pub end_lc: LineColumn,
}

impl Span {
    pub fn new(start: u32, end: u32, start_lc: LineColumn, end_lc: LineColumn) -> Self {
        Self {
            start,
            end,
            start_lc,
            end_lc,
        }
    }

    /// Construct a span from only line/column info (byte offsets default to 0).
    /// Useful for tests and contexts where byte offsets are not available.
    pub fn lc(start_lc: impl Into<LineColumn>, end_lc: impl Into<LineColumn>) -> Self {
        Self {
            start: 0,
            end: 0,
            start_lc: start_lc.into(),
            end_lc: end_lc.into(),
        }
    }

    /// Construct a span covering from the start of `from` to the end of `to`.
    pub fn from_spans(from: &Span, to: &Span) -> Self {
        Self {
            start: from.start,
            end: to.end,
            start_lc: from.start_lc,
            end_lc: to.end_lc,
        }
    }
}

/// Equality compares only `start_lc` and `end_lc` so that tests can construct
/// spans without knowing byte offsets and still compare equal to lexer-produced spans.
impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        self.start_lc == other.start_lc && self.end_lc == other.end_lc
    }
}

impl Eq for Span {}

impl Ord for Span {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.start.cmp(&other.start)
    }
}

impl PartialOrd for Span {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl std::fmt::Debug for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "Span({}..{}, {:?})", self.start, self.end, self.start_lc)
    }
}

#[derive(Debug, PartialEq, Eq, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Spanned<T> {
    pub node: T,
    pub span: Span,
}
