use tlang_ast::{
    span::{LineColumn, Span},
    token::{Literal, Token, TokenValue},
};

#[derive(Debug)]
pub struct Lexer<'src> {
    source: &'src str,
    position: usize,
    current_line: usize,
    current_column: usize,
    current_token: Option<Token>,
}

impl Lexer<'_> {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            source,
            position: 0,
            current_line: 1,
            current_column: 1,
            current_token: None,
        }
    }

    pub fn current_line(&self) -> usize {
        self.current_line
    }

    pub fn current_column(&self) -> usize {
        self.current_column
    }

    pub fn source(&self) -> &str {
        self.source
    }

    fn peek_ahead(&self) -> Option<char> {
        self.source.chars().nth(self.position + 1)
    }

    fn advance(&mut self) {
        if self.position < self.source.len() {
            self.position += 1;

            if let Some(ch) = self.source.chars().nth(self.position - 1) {
                if ch == '\n' {
                    self.current_line += 1;
                    self.current_column = 1;
                } else {
                    self.current_column += 1;
                }
            }
        }
    }

    fn skip_whitespace(&mut self) {
        self.advance_while(char::is_whitespace);
    }

    fn is_digit(ch: char) -> bool {
        ch.is_ascii_digit()
    }

    #[inline(always)]
    fn advance_while(&mut self, predicate: impl Fn(char) -> bool) {
        while let Some(ch) = self.source.chars().nth(self.position) {
            if predicate(ch) {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn read_integer(&mut self) -> i64 {
        let start = self.position;
        self.advance_while(Self::is_digit);
        let slice = &self.source[start..self.position];
        slice.parse().unwrap_or(0)
    }

    fn read_float(&mut self) -> f64 {
        let start = self.position;
        self.advance_while(|ch| Self::is_digit(ch) || ch == '.');
        let slice = &self.source[start..self.position];
        slice.parse().unwrap_or(0.0)
    }

    fn is_alphanumeric(ch: char) -> bool {
        ch.is_ascii_alphanumeric() || ch == '_'
    }

    fn read_identifier(&mut self) -> &str {
        let start = self.position;
        self.advance_while(Self::is_alphanumeric);
        &self.source[start..self.position]
    }

    fn read_string_literal(&mut self, quote: char) -> String {
        let start = self.position;
        self.advance_while(|ch| ch != quote);
        let slice = &self.source[start..self.position];
        self.advance();
        slice.to_owned()
    }

    fn create_token(&self, value: TokenValue) -> Token {
        Token::new(
            value,
            Some(Span {
                start: LineColumn {
                    line: self.current_line,
                    column: self.current_column,
                },
                end: LineColumn {
                    line: self.current_line,
                    column: self.current_column + 1,
                },
            }),
        )
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if self.position >= self.source.len() {
            let token = self.create_token(TokenValue::Eof);
            self.current_token = Some(token.clone());
            return token;
        }

        let ch = self.source.chars().nth(self.position).unwrap();

        let token_value = match ch {
            '+' => {
                self.advance();
                TokenValue::Plus
            }
            '-' => {
                if self.peek_ahead() == Some('>') {
                    self.advance();
                    self.advance();
                    TokenValue::Arrow
                } else {
                    self.advance();
                    TokenValue::Minus
                }
            }
            '*' => {
                if self.peek_ahead() == Some('*') {
                    self.advance();
                    self.advance();
                    TokenValue::AsteriskAsterisk
                } else {
                    self.advance();
                    TokenValue::Asterisk
                }
            }
            '/' => {
                if self.peek_ahead() == Some('/') {
                    self.advance();
                    self.advance();
                    let start = self.position;
                    self.advance_while(|ch| ch != '\n');
                    let slice = &self.source[start..self.position];
                    TokenValue::SingleLineComment(slice.to_owned())
                } else if self.peek_ahead() == Some('*') {
                    self.advance();
                    self.advance();
                    let start = self.position;
                    while !(self.source[self.position..].starts_with("*/")
                        || self.position >= self.source.len())
                    {
                        self.advance();
                    }
                    let slice = &self.source[start..self.position];
                    self.advance();
                    self.advance();
                    TokenValue::MultiLineComment(slice.to_owned())
                } else {
                    self.advance();
                    TokenValue::Slash
                }
            }
            '%' => {
                self.advance();
                TokenValue::Percent
            }
            '^' => {
                self.advance();
                TokenValue::Caret
            }
            '|' => {
                if self.peek_ahead() == Some('|') {
                    self.advance();
                    self.advance();
                    TokenValue::DoublePipe
                } else if self.peek_ahead() == Some('>') {
                    self.advance();
                    self.advance();
                    TokenValue::Pipeline
                } else {
                    self.advance();
                    TokenValue::Pipe
                }
            }
            '&' => {
                if self.peek_ahead() == Some('&') {
                    self.advance();
                    self.advance();
                    TokenValue::DoubleAmpersand
                } else {
                    self.advance();
                    TokenValue::Ampersand
                }
            }
            '<' => {
                if self.peek_ahead() == Some('=') {
                    self.advance();
                    self.advance();
                    TokenValue::LessThanOrEqual
                } else {
                    self.advance();
                    TokenValue::LessThan
                }
            }
            '>' => {
                if self.peek_ahead() == Some('=') {
                    self.advance();
                    self.advance();
                    TokenValue::GreaterThanOrEqual
                } else {
                    self.advance();
                    TokenValue::GreaterThan
                }
            }
            '(' => {
                self.advance();
                TokenValue::LParen
            }
            ')' => {
                self.advance();
                TokenValue::RParen
            }
            '{' => {
                self.advance();
                TokenValue::LBrace
            }
            '}' => {
                self.advance();
                TokenValue::RBrace
            }
            '[' => {
                self.advance();
                TokenValue::LBracket
            }
            ']' => {
                self.advance();
                TokenValue::RBracket
            }
            ',' => {
                self.advance();
                TokenValue::Comma
            }
            '=' => {
                if self.peek_ahead() == Some('=') {
                    self.advance();
                    self.advance();
                    TokenValue::EqualEqual
                } else if self.peek_ahead() == Some('>') {
                    self.advance();
                    self.advance();
                    TokenValue::FatArrow
                } else {
                    self.advance();
                    TokenValue::EqualSign
                }
            }
            '!' => {
                if self.peek_ahead() == Some('=') {
                    self.advance();
                    self.advance();
                    TokenValue::NotEqual
                } else {
                    self.advance();
                    TokenValue::ExclamationMark
                }
            }
            '?' => {
                self.advance();
                TokenValue::QuestionMark
            }
            ':' => {
                if self.peek_ahead() == Some(':') {
                    self.advance();
                    self.advance();
                    TokenValue::NamespaceSeparator
                } else {
                    self.advance();
                    TokenValue::Colon
                }
            }
            ';' => {
                self.advance();
                TokenValue::Semicolon
            }
            '.' => {
                if let Some('.') = self.peek_ahead() {
                    if self.source[self.position + 1..].starts_with("..") {
                        self.advance();
                        self.advance();
                        self.advance();
                        TokenValue::DotDotDot
                    } else {
                        self.advance();
                        self.advance();
                        TokenValue::DotDot
                    }
                } else {
                    self.advance();
                    TokenValue::Dot
                }
            }
            '0'..='9' => {
                let is_float = self.source[self.position..]
                    .chars()
                    .take_while(|&ch| Self::is_digit(ch) || ch == '.')
                    .any(|ch| ch == '.');

                if is_float {
                    TokenValue::Literal(Literal::Float(self.read_float()))
                } else {
                    TokenValue::Literal(Literal::Integer(self.read_integer()))
                }
            }
            '"' | '\'' => {
                let quote = ch;
                self.advance();
                let literal = self.read_string_literal(quote);

                if quote == '"' {
                    TokenValue::Literal(Literal::String(literal))
                } else {
                    TokenValue::Literal(Literal::Char(literal))
                }
            }
            _ if Self::is_alphanumeric(ch) => {
                let identifier = self.read_identifier();

                match identifier {
                    "let" => TokenValue::Let,
                    "fn" => TokenValue::Fn,
                    "rec" => TokenValue::Rec,
                    "return" => TokenValue::Return,
                    "if" => TokenValue::If,
                    "else" => TokenValue::Else,
                    "match" => TokenValue::Match,
                    "enum" => TokenValue::Enum,
                    "struct" => TokenValue::Struct,
                    "true" => TokenValue::Literal(Literal::Boolean(true)),
                    "false" => TokenValue::Literal(Literal::Boolean(false)),
                    _ => TokenValue::Identifier(identifier.to_owned()),
                }
            }
            _ => {
                // TODO: We should probably use a Result type here
                panic!("Unexpected character: {}", ch);
            }
        };

        let token = self.create_token(token_value);
        self.current_token = Some(token.clone());
        token
    }
}
