use crate::keyword::Keyword;
#[cfg(feature = "serde")]
use serde::Serialize;
use tlang_span::Span;

#[derive(Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
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
    Identifier(String),

    // Token for comments
    SingleLineComment(String),
    MultiLineComment(String),

    // Keywords
    Keyword(Keyword),

    // Unknown token, unexpected character
    Unknown,

    // Token for end-of-file, also used as an unitialized sentinel token.
    Eof,
}

#[derive(Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Literal {
    Boolean(bool),
    Integer(i64),
    UnsignedInteger(u64),
    Float(f64),
    String(Box<str>),
    Char(Box<str>),
    None,
}

impl Clone for Literal {
    fn clone(&self) -> Self {
        // TODO: Would be great if literal string and char just refer to the string in the
        //       source. Or we do some kind of interning at some point.
        match self {
            Literal::String(value) => Literal::String(value.clone()),
            Literal::Char(value) => Literal::Char(value.clone()),
            _ => unsafe { std::ptr::read(self) },
        }
    }
}

impl Literal {
    /// # Panics
    pub fn invert_sign(&self) -> Self {
        match self {
            Literal::UnsignedInteger(value) => Literal::Integer(-(*value as i64)),
            Literal::Integer(value) => Literal::Integer(-value),
            Literal::Float(value) => Literal::Float(-value),
            _ => panic!("Expected integer or float, found {self:?}"),
        }
    }
}

impl From<bool> for Literal {
    fn from(value: bool) -> Self {
        Literal::Boolean(value)
    }
}

#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
}

impl Default for Token {
    fn default() -> Self {
        Self {
            kind: TokenKind::Eof,
            span: Span::default(),
        }
    }
}

impl Token {
    pub fn new(kind: TokenKind, span: Span) -> Self {
        Self { kind, span }
    }

    pub fn get_identifier(&self) -> Option<&String> {
        match &self.kind {
            TokenKind::Identifier(identifier) => Some(identifier),
            _ => None,
        }
    }

    pub fn get_keyword(&self) -> Option<&Keyword> {
        match &self.kind {
            TokenKind::Keyword(keyword) => Some(keyword),
            _ => None,
        }
    }

    pub fn get_comment(&self) -> Option<&String> {
        match &self.kind {
            TokenKind::SingleLineComment(comment) => Some(comment),
            TokenKind::MultiLineComment(comment) => Some(comment),
            _ => None,
        }
    }

    pub fn take_literal(&mut self) -> Option<Literal> {
        match &mut self.kind {
            TokenKind::Literal(literal) => Some(std::mem::replace(literal, Literal::None)),
            _ => None,
        }
    }
}
