use crate::keyword::Keyword;
#[cfg(feature = "serde")]
use serde::Serialize;
use tlang_intern::Symbol;
use tlang_span::Span;

#[derive(Copy, Clone, Debug, PartialEq)]
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
    Matches,
    NotMatches,
    GreaterThan,
    GreaterThanOrEqual,
    LessThan,
    LessThanOrEqual,
    DoubleAmpersand,
    DoublePipe,

    // Tokens for special characters
    Ampersand,
    Pipe,
    Tilde,
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

    // Tokens for literals (numbers, booleans, strings, chars)
    Literal(Literal),

    // Token for identifiers — text is recovered from the source via span.
    Identifier,

    // Token for comments — text is recovered from the source via span.
    SingleLineComment,
    MultiLineComment,

    /// A tagged string header: the opening quote character.
    /// The tag identifier text is recovered from the span (everything before
    /// the trailing quote char). The actual string content is read lazily by
    /// the parser via `Lexer::read_tagged_string_parts`.
    TaggedStringStart(char),

    // Keywords
    Keyword(Keyword),

    // Unknown token, unexpected character
    Unknown,

    // Token for end-of-file, also used as an unitialized sentinel token.
    Eof,
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum TaggedStringPart {
    /// A literal text segment of the tagged string.
    Literal(Box<str>),
    /// Raw source text of an interpolation expression (between `{` and `}`).
    /// Parsed into a full expression by the parser.
    /// `line`, `column`, and `byte_offset` are the source position of the
    /// first character of the interpolation body (after the opening `{`).
    Interpolation {
        source: Box<str>,
        line: u32,
        column: u32,
        byte_offset: u32,
    },
}

impl PartialEq for TaggedStringPart {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (TaggedStringPart::Literal(a), TaggedStringPart::Literal(b)) => a == b,
            // Compare only the source content; position fields are for span generation only.
            (
                TaggedStringPart::Interpolation { source: a, .. },
                TaggedStringPart::Interpolation { source: b, .. },
            ) => a == b,
            _ => false,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Literal {
    Boolean(bool),
    Integer(i64),
    UnsignedInteger(u64),
    Float(f64),
    String(Symbol),
    Char(Symbol),
    None,
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

#[derive(Copy, Clone, Debug)]
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

    pub fn get_keyword(&self) -> Option<Keyword> {
        match self.kind {
            TokenKind::Keyword(keyword) => Some(keyword),
            _ => None,
        }
    }

    pub fn literal(&self) -> Option<Literal> {
        match self.kind {
            TokenKind::Literal(literal) => Some(literal),
            _ => None,
        }
    }
}

/// Comment kind for AST-stored comments (owned, no source lifetime).
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum CommentKind {
    SingleLine,
    MultiLine,
}

/// An owned comment extracted from the token stream for storage in AST/HIR nodes.
#[derive(Clone, Debug)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct CommentToken {
    pub kind: CommentKind,
    pub text: Box<str>,
    pub span: Span,
}

// Compile-time assertion that Token is Copy.
const _: fn() = || {
    let t = Token::default();
    let _ = (t, t);
};
