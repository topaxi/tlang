#[derive(Clone, Debug, PartialEq)]
pub enum Token {
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

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Boolean(bool),
    Integer(i64),
    UnsignedInteger(u64),
    Float(f64),
    String(String),
    Char(String),
}
