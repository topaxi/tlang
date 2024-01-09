use std::rc::Rc;

use tlang_ast::token::{Literal, TokenKind};

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    source: Rc<&'src str>,
    position: usize,
    current_line: usize,
    current_column: usize,
    current_token: Option<TokenKind>,
}

impl Lexer<'_> {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            source: source.into(),
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
        self.source.as_ref()
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

    pub fn next_token(&mut self) -> TokenKind {
        self.skip_whitespace();

        if self.position >= self.source.len() {
            return TokenKind::Eof;
        }

        let ch = self.source.chars().nth(self.position).unwrap();

        let token = match ch {
            '+' => {
                self.advance();
                TokenKind::Plus
            }
            '-' => {
                if self.peek_ahead() == Some('>') {
                    self.advance();
                    self.advance();
                    TokenKind::Arrow
                } else {
                    self.advance();
                    TokenKind::Minus
                }
            }
            '*' => {
                if self.peek_ahead() == Some('*') {
                    self.advance();
                    self.advance();
                    TokenKind::AsteriskAsterisk
                } else {
                    self.advance();
                    TokenKind::Asterisk
                }
            }
            '/' => {
                if self.peek_ahead() == Some('/') {
                    self.advance();
                    self.advance();
                    let start = self.position;
                    self.advance_while(|ch| ch != '\n');
                    let slice = &self.source[start..self.position];
                    TokenKind::SingleLineComment(slice.to_owned())
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
                    TokenKind::MultiLineComment(slice.to_owned())
                } else {
                    self.advance();
                    TokenKind::Slash
                }
            }
            '%' => {
                self.advance();
                TokenKind::Percent
            }
            '^' => {
                self.advance();
                TokenKind::Caret
            }
            '|' => {
                if self.peek_ahead() == Some('|') {
                    self.advance();
                    self.advance();
                    TokenKind::DoublePipe
                } else if self.peek_ahead() == Some('>') {
                    self.advance();
                    self.advance();
                    TokenKind::Pipeline
                } else {
                    self.advance();
                    TokenKind::Pipe
                }
            }
            '&' => {
                if self.peek_ahead() == Some('&') {
                    self.advance();
                    self.advance();
                    TokenKind::DoubleAmpersand
                } else {
                    self.advance();
                    TokenKind::Ampersand
                }
            }
            '<' => {
                if self.peek_ahead() == Some('=') {
                    self.advance();
                    self.advance();
                    TokenKind::LessThanOrEqual
                } else {
                    self.advance();
                    TokenKind::LessThan
                }
            }
            '>' => {
                if self.peek_ahead() == Some('=') {
                    self.advance();
                    self.advance();
                    TokenKind::GreaterThanOrEqual
                } else {
                    self.advance();
                    TokenKind::GreaterThan
                }
            }
            '(' => {
                self.advance();
                TokenKind::LParen
            }
            ')' => {
                self.advance();
                TokenKind::RParen
            }
            '{' => {
                self.advance();
                TokenKind::LBrace
            }
            '}' => {
                self.advance();
                TokenKind::RBrace
            }
            '[' => {
                self.advance();
                TokenKind::LBracket
            }
            ']' => {
                self.advance();
                TokenKind::RBracket
            }
            ',' => {
                self.advance();
                TokenKind::Comma
            }
            '=' => {
                if self.peek_ahead() == Some('=') {
                    self.advance();
                    self.advance();
                    TokenKind::EqualEqual
                } else if self.peek_ahead() == Some('>') {
                    self.advance();
                    self.advance();
                    TokenKind::FatArrow
                } else {
                    self.advance();
                    TokenKind::EqualSign
                }
            }
            '!' => {
                if self.peek_ahead() == Some('=') {
                    self.advance();
                    self.advance();
                    TokenKind::NotEqual
                } else {
                    self.advance();
                    TokenKind::ExclamationMark
                }
            }
            '?' => {
                self.advance();
                TokenKind::QuestionMark
            }
            ':' => {
                if self.peek_ahead() == Some(':') {
                    self.advance();
                    self.advance();
                    TokenKind::NamespaceSeparator
                } else {
                    self.advance();
                    TokenKind::Colon
                }
            }
            ';' => {
                self.advance();
                TokenKind::Semicolon
            }
            '.' => {
                if let Some('.') = self.peek_ahead() {
                    if self.source[self.position + 1..].starts_with("..") {
                        self.advance();
                        self.advance();
                        self.advance();
                        TokenKind::DotDotDot
                    } else {
                        self.advance();
                        self.advance();
                        TokenKind::DotDot
                    }
                } else {
                    self.advance();
                    TokenKind::Dot
                }
            }
            '0'..='9' => {
                let is_float = self.source[self.position..]
                    .chars()
                    .take_while(|&ch| Self::is_digit(ch) || ch == '.')
                    .any(|ch| ch == '.');

                if is_float {
                    TokenKind::Literal(Literal::Float(self.read_float()))
                } else {
                    TokenKind::Literal(Literal::Integer(self.read_integer()))
                }
            }
            '"' | '\'' => {
                let quote = ch;
                self.advance();
                let literal = self.read_string_literal(quote);

                if quote == '"' {
                    TokenKind::Literal(Literal::String(literal))
                } else {
                    TokenKind::Literal(Literal::Char(literal))
                }
            }
            _ if Self::is_alphanumeric(ch) => {
                let identifier = self.read_identifier();

                match identifier {
                    "let" => TokenKind::Let,
                    "fn" => TokenKind::Fn,
                    "rec" => TokenKind::Rec,
                    "return" => TokenKind::Return,
                    "if" => TokenKind::If,
                    "else" => TokenKind::Else,
                    "match" => TokenKind::Match,
                    "enum" => TokenKind::Enum,
                    "struct" => TokenKind::Struct,
                    "true" => TokenKind::Literal(Literal::Boolean(true)),
                    "false" => TokenKind::Literal(Literal::Boolean(false)),
                    _ => TokenKind::Identifier(identifier.to_owned()),
                }
            }
            _ => {
                // TODO: We should probably use a Result type here
                panic!("Unexpected character: {}", ch);
            }
        };

        self.current_token = Some(token.clone());
        token
    }
}
