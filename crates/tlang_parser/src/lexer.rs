use tlang_ast::{
    span::{LineColumn, Span},
    token::{Literal, Token, TokenKind},
};

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    source: &'src str,
    position: usize,
    current_line: usize,
    current_column: usize,
}

impl Lexer<'_> {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            source,
            position: 0,
            current_line: 0,
            current_column: 0,
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

    #[inline(always)]
    fn current_char(&self) -> Option<char> {
        self.source[self.position..].chars().next()
    }

    #[inline(always)]
    fn peek_char(&self) -> Option<char> {
        self.source[self.position..].chars().nth(1)
    }

    #[inline(always)]
    fn previous_char(&self) -> Option<char> {
        self.source[..self.position].chars().last()
    }

    fn advance(&mut self) {
        if self.position < self.source.len() {
            self.position += 1;

            if let Some(ch) = self.previous_char() {
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

    fn is_number_literal_char(ch: char) -> bool {
        Self::is_digit(ch) || ch == '.' || ch == '_'
    }

    #[inline(always)]
    fn advance_while(&mut self, predicate: impl Fn(char) -> bool) {
        for ch in self.source[self.position..].chars() {
            if predicate(ch) {
                self.advance();
            } else {
                break;
            }
        }
    }

    fn read_number_literal(&mut self, start: LineColumn) -> Token {
        let start_position = self.position;

        self.advance_while(Self::is_number_literal_char);

        let slice = &self.source[start_position..self.position];
        let token_kind = if slice.contains('.') {
                TokenKind::Literal(Literal::Float(slice.parse().unwrap_or(0.0)))
        } else {
                TokenKind::Literal(Literal::Integer(slice.parse().unwrap_or(0)))
        };

        self.token(token_kind, start)
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
        let string_literal = self.source[start..self.position].to_string();
        self.advance();
        string_literal
    }

    fn line_column(&self) -> LineColumn {
        LineColumn {
            line: self.current_line,
            column: self.current_column,
        }
    }

    fn token(&self, kind: TokenKind, start: LineColumn) -> Token {
        Token::new(kind, Span::new(start, self.line_column()))
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let start = self.line_column();

        if self.position >= self.source.len() {
            return self.token(TokenKind::Eof, start);
        }

        let ch = self.current_char().unwrap();

        let token = match ch {
            '+' => {
                self.advance();
                self.token(TokenKind::Plus, start)
            }
            '-' => {
                if self.peek_char() == Some('>') {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::Arrow, start)
                } else {
                    self.advance();
                    self.token(TokenKind::Minus, start)
                }
            }
            '*' => {
                if self.peek_char() == Some('*') {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::AsteriskAsterisk, start)
                } else {
                    self.advance();
                    self.token(TokenKind::Asterisk, start)
                }
            }
            '/' => {
                if self.peek_char() == Some('/') {
                    self.advance();
                    self.advance();
                    let start_position = self.position;
                    self.advance_while(|ch| ch != '\n');
                    let comment = self.source[start_position..self.position].to_string();
                    self.token(TokenKind::SingleLineComment(comment), start)
                } else if self.peek_char() == Some('*') {
                    self.advance();
                    self.advance();
                    let start_position = self.position;
                    while !(self.source[self.position..].starts_with("*/")
                        || self.position >= self.source.len())
                    {
                        self.advance();
                    }
                    let comment = self.source[start_position..self.position].to_string();
                    self.advance();
                    self.advance();
                    self.token(TokenKind::MultiLineComment(comment), start)
                } else {
                    self.advance();
                    self.token(TokenKind::Slash, start)
                }
            }
            '%' => {
                self.advance();
                self.token(TokenKind::Percent, start)
            }
            '^' => {
                self.advance();
                self.token(TokenKind::Caret, start)
            }
            '|' => {
                if self.peek_char() == Some('|') {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::DoublePipe, start)
                } else if self.peek_char() == Some('>') {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::Pipeline, start)
                } else {
                    self.advance();
                    self.token(TokenKind::Pipe, start)
                }
            }
            '&' => {
                if self.peek_char() == Some('&') {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::DoubleAmpersand, start)
                } else {
                    self.advance();
                    self.token(TokenKind::Ampersand, start)
                }
            }
            '<' => {
                if self.peek_char() == Some('=') {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::LessThanOrEqual, start)
                } else {
                    self.advance();
                    self.token(TokenKind::LessThan, start)
                }
            }
            '>' => {
                if self.peek_char() == Some('=') {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::GreaterThanOrEqual, start)
                } else {
                    self.advance();
                    self.token(TokenKind::GreaterThan, start)
                }
            }
            '(' => {
                self.advance();
                self.token(TokenKind::LParen, start)
            }
            ')' => {
                self.advance();
                self.token(TokenKind::RParen, start)
            }
            '{' => {
                self.advance();
                self.token(TokenKind::LBrace, start)
            }
            '}' => {
                self.advance();
                self.token(TokenKind::RBrace, start)
            }
            '[' => {
                self.advance();
                self.token(TokenKind::LBracket, start)
            }
            ']' => {
                self.advance();
                self.token(TokenKind::RBracket, start)
            }
            ',' => {
                self.advance();
                self.token(TokenKind::Comma, start)
            }
            '=' => {
                if self.peek_char() == Some('=') {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::EqualEqual, start)
                } else if self.peek_char() == Some('>') {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::FatArrow, start)
                } else {
                    self.advance();
                    self.token(TokenKind::EqualSign, start)
                }
            }
            '!' => {
                if self.peek_char() == Some('=') {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::NotEqual, start)
                } else {
                    self.advance();
                    self.token(TokenKind::ExclamationMark, start)
                }
            }
            '?' => {
                self.advance();
                self.token(TokenKind::QuestionMark, start)
            }
            ':' => {
                if self.peek_char() == Some(':') {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::NamespaceSeparator, start)
                } else {
                    self.advance();
                    self.token(TokenKind::Colon, start)
                }
            }
            ';' => {
                self.advance();
                self.token(TokenKind::Semicolon, start)
            }
            '.' => {
                if let Some('.') = self.peek_char() {
                    if self.source[self.position + 1..].starts_with("..") {
                        self.advance();
                        self.advance();
                        self.advance();
                        self.token(TokenKind::DotDotDot, start)
                    } else {
                        self.advance();
                        self.advance();
                        self.token(TokenKind::DotDot, start)
                    }
                } else {
                    self.advance();
                    self.token(TokenKind::Dot, start)
                }
            }
            '0'..='9' => self.read_number_literal(start),
            '"' | '\'' => {
                let quote = ch;
                self.advance();
                let literal = self.read_string_literal(quote);

                if quote == '"' {
                    self.token(TokenKind::Literal(Literal::String(literal)), start)
                } else {
                    self.token(TokenKind::Literal(Literal::Char(literal)), start)
                }
            }
            ch if Self::is_alphanumeric(ch) => match self.read_identifier() {
                "let" => self.token(TokenKind::Let, start),
                "fn" => self.token(TokenKind::Fn, start),
                "rec" => self.token(TokenKind::Rec, start),
                "return" => self.token(TokenKind::Return, start),
                "if" => self.token(TokenKind::If, start),
                "else" => self.token(TokenKind::Else, start),
                "match" => self.token(TokenKind::Match, start),
                "not" => self.token(TokenKind::Not, start),
                "enum" => self.token(TokenKind::Enum, start),
                "struct" => self.token(TokenKind::Struct, start),
                "true" => self.token(TokenKind::Literal(Literal::Boolean(true)), start),
                "false" => self.token(TokenKind::Literal(Literal::Boolean(false)), start),
                "and" => self.token(TokenKind::And, start),
                "or" => self.token(TokenKind::Or, start),
                identifier => {
                    let identifier_string = identifier.to_string();
                    self.token(TokenKind::Identifier(identifier_string), start)
                }
            },
            ch => self.token(TokenKind::Unknown(ch.to_string()), start),
        };

        token
    }
}
