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
    // TODO: Similar to `self`, we might want to treat this as an identifier.
    Underscore => "_",
    // TODO: It might actually be better to just treat this as an identifier.
    _Self => "self",

    // Reserved keywords, unused at the moment
    As => "as",
    For => "for",
    In => "in",
    Loop => "loop",
    Pub => "pub",
    SelfUpper => "Self",
    Use => "use",
    While => "while",
    With => "with"
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
    Unknown(String),

    // Token for end-of-file
    Eof,
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

#[derive(Clone, Debug, Serialize)]
pub struct Token {
    pub kind: TokenKind,
    pub span: Span,
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
}
