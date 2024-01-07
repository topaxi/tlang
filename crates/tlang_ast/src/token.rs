use crate::span::Span;

#[derive(Clone, Debug)]
pub struct Token {
    pub value: TokenValue,
    pub span: Option<Span>,
}

impl PartialEq for Token {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl PartialEq<TokenValue> for Token {
    fn eq(&self, other: &TokenValue) -> bool {
        &self.value == other
    }
}

impl PartialEq<TokenValue> for Option<Token> {
    fn eq(&self, other: &TokenValue) -> bool {
        self.as_ref().map_or(false, |t| t == other)
    }
}

impl PartialEq<Option<TokenValue>> for Token {
    fn eq(&self, other: &Option<TokenValue>) -> bool {
        self == other.as_ref().map(|t| t)
    }
}

impl Token {
    pub fn new(value: TokenValue, span: Option<Span>) -> Self {
        Token { value, span }
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum TokenValue {
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
    Let,
    Fn,
    Return,
    Rec,
    If,
    Else,
    Match,
    Enum,
    Struct,

    // Token for end-of-file
    Eof,
}

impl PartialEq<Token> for TokenValue {
    fn eq(&self, other: &Token) -> bool {
        self == &other.value
    }
}

impl PartialEq<Token> for Option<TokenValue> {
    fn eq(&self, other: &Token) -> bool {
        self.as_ref().map_or(false, |t| t == other)
    }
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Boolean(bool),
    Integer(i64),
    UnsignedInteger(u64),
    Float(f64),
    String(String),
    Char(String),
}
