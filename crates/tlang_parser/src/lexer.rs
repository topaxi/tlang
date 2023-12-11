#[derive(Clone, Debug, PartialEq)]
pub enum Token {
    // Tokens for binary operators
    Caret,
    Plus,
    Minus,
    Asterisk,
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

    // Tokens for numbers
    Literal(Literal),

    // Token for identifiers
    Identifier(String),

    // Keywords
    Let,
    Fn,
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
    current_token: Option<Token>,
}

impl Lexer<'_> {
    pub fn new(source: &str) -> Lexer {
        Lexer {
            source,
            position: 0,
            current_token: None,
        }
    }

    fn peek_ahead(&self) -> Option<char> {
        self.source.chars().nth(self.position + 1)
    }

    fn advance(&mut self) {
        if self.position < self.source.len() {
            self.position += 1;
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
                self.advance();
                Token::Minus
            }
            '*' => {
                self.advance();
                Token::Asterisk
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

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_identifier() {
        let mut lexer = Lexer::new("test");

        assert_eq!(lexer.next_token(), Token::Identifier("test".to_string()));
        assert_eq!(lexer.next_token(), Token::Eof);
    }

    #[test]
    fn test_integer() {
        let mut lexer = Lexer::new("123");

        assert_eq!(lexer.next_token(), Token::Literal(Literal::Integer(123)));
        assert_eq!(lexer.next_token(), Token::Eof);
    }

    #[test]
    fn test_float() {
        let mut lexer = Lexer::new("123.456");

        assert_eq!(lexer.next_token(), Token::Literal(Literal::Float(123.456)));
        assert_eq!(lexer.next_token(), Token::Eof);
    }

    #[test]
    fn test_typed_integer() {
        let mut lexer = Lexer::new("123i64");
        assert_eq!(lexer.next_token(), Token::Literal(Literal::Integer(123)));
        assert_eq!(lexer.next_token(), Token::Identifier("i64".to_string()));
        assert_eq!(lexer.next_token(), Token::Eof);
    }

    #[test]
    fn test_typed_float() {
        let mut lexer = Lexer::new("123.456f64");
        assert_eq!(lexer.next_token(), Token::Literal(Literal::Float(123.456)));
        assert_eq!(lexer.next_token(), Token::Identifier("f64".to_string()));
        assert_eq!(lexer.next_token(), Token::Eof);
    }

    #[test]
    fn test_single_char_operators() {
        let mut lexer = Lexer::new("+ - * / % ( ) | & ^");

        assert_eq!(lexer.next_token(), Token::Plus);
        assert_eq!(lexer.next_token(), Token::Minus);
        assert_eq!(lexer.next_token(), Token::Asterisk);
        assert_eq!(lexer.next_token(), Token::Slash);
        assert_eq!(lexer.next_token(), Token::Percent);
        assert_eq!(lexer.next_token(), Token::LParen);
        assert_eq!(lexer.next_token(), Token::RParen);
        assert_eq!(lexer.next_token(), Token::Pipe);
        assert_eq!(lexer.next_token(), Token::Ampersand);
        assert_eq!(lexer.next_token(), Token::Caret);
        assert_eq!(lexer.next_token(), Token::Eof);
    }

    #[test]
    fn test_multi_char_operators() {
        let mut lexer = Lexer::new("|| && == != >= <=");

        assert_eq!(lexer.next_token(), Token::DoublePipe);
        assert_eq!(lexer.next_token(), Token::DoubleAmpersand);
        assert_eq!(lexer.next_token(), Token::EqualEqual);
        assert_eq!(lexer.next_token(), Token::NotEqual);
        assert_eq!(lexer.next_token(), Token::GreaterThanOrEqual);
        assert_eq!(lexer.next_token(), Token::LessThanOrEqual);
        assert_eq!(lexer.next_token(), Token::Eof);
    }

    #[test]
    fn test_keywords() {
        let mut lexer = Lexer::new("let fn if else match");

        assert_eq!(lexer.next_token(), Token::Let);
        assert_eq!(lexer.next_token(), Token::Fn);
        assert_eq!(lexer.next_token(), Token::If);
        assert_eq!(lexer.next_token(), Token::Else);
        assert_eq!(lexer.next_token(), Token::Match);
        assert_eq!(lexer.next_token(), Token::Eof);
    }

    #[test]
    fn test_equal_sign() {
        let mut lexer = Lexer::new("=");

        assert_eq!(lexer.next_token(), Token::EqualSign);
        assert_eq!(lexer.next_token(), Token::Eof);
    }

    #[test]
    fn test_comparison_operators() {
        let mut lexer = Lexer::new("== != >= <= > <");

        assert_eq!(lexer.next_token(), Token::EqualEqual);
        assert_eq!(lexer.next_token(), Token::NotEqual);
        assert_eq!(lexer.next_token(), Token::GreaterThanOrEqual);
        assert_eq!(lexer.next_token(), Token::LessThanOrEqual);
        assert_eq!(lexer.next_token(), Token::GreaterThan);
        assert_eq!(lexer.next_token(), Token::LessThan);
        assert_eq!(lexer.next_token(), Token::Eof);
    }

    #[test]
    fn test_simple_assignment() {
        let mut lexer = Lexer::new("let x = 1 + 2;");

        assert_eq!(lexer.next_token(), Token::Let);
        assert_eq!(lexer.next_token(), Token::Identifier("x".to_string()));
        assert_eq!(lexer.next_token(), Token::EqualSign);
        assert_eq!(lexer.next_token(), Token::Literal(Literal::Integer(1)));
        assert_eq!(lexer.next_token(), Token::Plus);
        assert_eq!(lexer.next_token(), Token::Literal(Literal::Integer(2)));
        assert_eq!(lexer.next_token(), Token::Semicolon);
        assert_eq!(lexer.next_token(), Token::Eof);
    }
}
