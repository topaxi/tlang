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

    // Tokens for numbers
    Literal(Literal),

    // Token for identifiers
    Identifier(String),

    // Keywords
    Let,
    Fn,
    Return,
    If,
    Else,
    Match,

    // Token for end-of-file
    Eof,
}

#[derive(Clone, Debug, PartialEq)]
pub enum Literal {
    Boolean(bool),
    Integer(i64),
    UnsignedInteger(u64),
    Float(f64),
}

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
    fn advance_while(&mut self, predicate: fn(char) -> bool) {
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

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        if self.position >= self.source.len() {
            return Token::Eof;
        }

        let ch = self.source.chars().nth(self.position).unwrap();

        let token = match ch {
            '+' => {
                self.advance();
                Token::Plus
            }
            '-' => {
                if self.peek_ahead() == Some('>') {
                    self.advance();
                    self.advance();
                    Token::Arrow
                } else {
                    self.advance();
                    Token::Minus
                }
            }
            '*' => {
                if self.peek_ahead() == Some('*') {
                    self.advance();
                    self.advance();
                    Token::AsteriskAsterisk
                } else {
                    self.advance();
                    Token::Asterisk
                }
            }
            '/' => {
                self.advance();
                Token::Slash
            }
            '%' => {
                self.advance();
                Token::Percent
            }
            '^' => {
                self.advance();
                Token::Caret
            }
            '|' => {
                if self.peek_ahead() == Some('|') {
                    self.advance();
                    self.advance();
                    Token::DoublePipe
                } else if self.peek_ahead() == Some('>') {
                    self.advance();
                    self.advance();
                    Token::Pipeline
                } else {
                    self.advance();
                    Token::Pipe
                }
            }
            '&' => {
                if self.peek_ahead() == Some('&') {
                    self.advance();
                    self.advance();
                    Token::DoubleAmpersand
                } else {
                    self.advance();
                    Token::Ampersand
                }
            }
            '<' => {
                if self.peek_ahead() == Some('=') {
                    self.advance();
                    self.advance();
                    Token::LessThanOrEqual
                } else {
                    self.advance();
                    Token::LessThan
                }
            }
            '>' => {
                if self.peek_ahead() == Some('=') {
                    self.advance();
                    self.advance();
                    Token::GreaterThanOrEqual
                } else {
                    self.advance();
                    Token::GreaterThan
                }
            }
            '(' => {
                self.advance();
                Token::LParen
            }
            ')' => {
                self.advance();
                Token::RParen
            }
            '{' => {
                self.advance();
                Token::LBrace
            }
            '}' => {
                self.advance();
                Token::RBrace
            }
            '[' => {
                self.advance();
                Token::LBracket
            }
            ']' => {
                self.advance();
                Token::RBracket
            }
            ',' => {
                self.advance();
                Token::Comma
            }
            '=' => {
                if self.peek_ahead() == Some('=') {
                    self.advance();
                    self.advance();
                    Token::EqualEqual
                } else if self.peek_ahead() == Some('>') {
                    self.advance();
                    self.advance();
                    Token::FatArrow
                } else {
                    self.advance();
                    Token::EqualSign
                }
            }
            '!' => {
                if self.peek_ahead() == Some('=') {
                    self.advance();
                    self.advance();
                    Token::NotEqual
                } else {
                    self.advance();
                    Token::ExclamationMark
                }
            }
            '?' => {
                self.advance();
                Token::QuestionMark
            }
            ':' => {
                self.advance();
                Token::Colon
            }
            ';' => {
                self.advance();
                Token::Semicolon
            }
            '.' | '0'..='9' => {
                let is_float = self.source[self.position..]
                    .chars()
                    .take_while(|&c| Self::is_digit(ch) || c == '.')
                    .any(|c| c == '.');

                if is_float {
                    Token::Literal(Literal::Float(self.read_float()))
                } else {
                    Token::Literal(Literal::Integer(self.read_integer()))
                }
            }
            _ if Self::is_alphanumeric(ch) => {
                let identifier = self.read_identifier();

                match identifier {
                    "let" => Token::Let,
                    "fn" => Token::Fn,
                    "return" => Token::Return,
                    "if" => Token::If,
                    "else" => Token::Else,
                    "match" => Token::Match,
                    "true" => Token::Literal(Literal::Boolean(true)),
                    "false" => Token::Literal(Literal::Boolean(false)),
                    _ => Token::Identifier(identifier.to_owned()),
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
