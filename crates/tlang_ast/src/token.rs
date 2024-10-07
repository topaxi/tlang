use crate::span::Span;
use serde::Serialize;

#[derive(Copy, Clone, Debug, PartialEq, Serialize)]
pub enum Keyword {
    Let,
    Fn,
    Return,
    Rec,
    If,
    Else,
    Match,
    Enum,
    Struct,
    Not,
    And,
    Or,
    Underscore,
}

#[derive(Clone, Debug, PartialEq, Serialize)]
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
    NamespaceSeparator,

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
    Identifier(String),

    // Token for comments
    SingleLineComment(String),
    MultiLineComment(String),

    // Keywords
    Keyword(Keyword),

    // Unknown token, unexpected character
    Unknown(String),

    // Token for end-of-file
    Eof,
}

impl TokenKind {
    pub fn get_identifier(&self) -> Option<&String> {
        match self {
            TokenKind::Identifier(identifier) => Some(identifier),
            _ => None,
        }
    }

    pub fn get_keyword(&self) -> Option<&Keyword> {
        match self {
            TokenKind::Keyword(keyword) => Some(keyword),
            _ => None,
        }
    }
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub enum Literal {
    Boolean(bool),
    Integer(i64),
    UnsignedInteger(u64),
    Float(f64),
    String(String),
    Char(String),
}

#[derive(Clone, Debug, PartialEq, Serialize)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn get_comment(&self) -> Option<&String> {
        match &self.kind {
            TokenKind::SingleLineComment(comment) => Some(comment),
            TokenKind::MultiLineComment(comment) => Some(comment),
            _ => None,
        }
    }
}
