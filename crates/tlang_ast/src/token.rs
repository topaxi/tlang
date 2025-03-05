use crate::define_keywords;
use crate::span::Span;
use serde::Serialize;

define_keywords! {
    Let => "let",
    Fn => "fn",
    Return => "return",
    Rec => "rec",
    If => "if",
    Else => "else",
    Match => "match",
    Enum => "enum",
    Struct => "struct",
    Not => "not",
    And => "and",
    Or => "or",
    As => "as",

    // TODO: Similar to `self`, we might want to treat this as an identifier.
    Underscore => "_",
    // TODO: It might actually be better to just treat this as an identifier.
    _Self => "self",

    // Reserved keywords, unused at the moment
    For => "for",
    In => "in",
    Loop => "loop",
    Pub => "pub",
    SelfUpper => "Self",
    Use => "use",
    While => "while",
    With => "with"
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize)]
pub enum TokenKind {
    // Tokens for binary operators
    Caret,
    Plus,
    Minus,
    Asterisk,
    AsteriskAsterisk,
    Slash,
    EqualEqual,
    NotEqual,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    DoubleAmpersand,
    DoublePipe,

    // Tokens for special characters
    Ampersand,
    Pipe,
    Colon,
    Comma,
    EqualSign,
    ExclamationMark,
    Percent,
    QuestionMark,
    Semicolon,
    DoubleQuote,
    SingleQuote,
    PathSeparator,
    Hash,

    // Tokens for parentheses
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,

    // Others
    Arrow,
    FatArrow,
    Pipeline,
    Dot,
    DotDot,
    DotDotDot,

    // Tokens for numbers
    Literal(Literal),

    // Token for identifiers
    Identifier,

    // Token for comments
    SingleLineComment,
    MultiLineComment,

    // Keywords
    Keyword(Keyword),

    // Unknown token, unexpected character
    Unknown,

    // Token for end-of-file, also used as an unitialized sentinel token.
    Eof,
}

#[derive(Clone, Copy, Debug, PartialEq, Serialize)]
pub enum Literal {
    Boolean(bool),
    Integer,
    UnsignedInteger,
    Float,
    String,
    Char,
    None,
}

#[derive(Clone, Debug, Serialize)]
pub struct Token {
    pub kind: TokenKind,
    pub token_span: TokenSpan,
    pub span: Span,
}

impl Default for Token {
    fn default() -> Self {
        Self {
            kind: TokenKind::Eof,
            token_span: TokenSpan::default(),
            span: Span::default(),
        }
    }
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn as_keyword(&self) -> Option<Keyword> {
        match self.kind {
            TokenKind::Keyword(keyword) => Some(keyword),
            _ => None,
        }
    }
}

#[derive(Clone, Copy, Debug, Default, PartialEq, Serialize)]
pub struct TokenSpan {
    pub start: u32,
    pub end: u32,
}

impl TokenSpan {
    pub fn new(start: u32, end: u32) -> Self {
        Self { start, end }
    }

    pub fn range(&self) -> std::ops::Range<usize> {
        self.start as usize..self.end as usize
    }
}
