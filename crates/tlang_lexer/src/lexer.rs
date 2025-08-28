use tlang_ast::{
    keyword::{Keyword, is_keyword},
    token::{Literal, Token, TokenKind},
};
use tlang_span::{LineColumn, Span};

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    source: &'src str,
    chars: std::str::Chars<'src>, // char iterator over `source`.
    position: usize,
    current_line: u32,
    current_column: u32,
    current_char: char,
    next_char: char,
}

impl Lexer<'_> {
    pub fn new(source: &str) -> Lexer<'_> {
        let mut chars = source.chars();
        let current_char = chars.next().unwrap_or('\0');
        let next_char = chars.next().unwrap_or('\0');

        Lexer {
            source,
            position: 0,
            current_line: 0,
            current_column: 0,
            current_char,
            next_char,
            chars,
        }
    }

    pub fn set_line_offset(&mut self, line: u32) {
        self.current_line = line;
    }

    pub fn current_line(&self) -> u32 {
        self.current_line
    }

    pub fn current_column(&self) -> u32 {
        self.current_column
    }

    pub fn source(&self) -> &str {
        self.source
    }

    fn advance(&mut self) {
        let previous_char = self.current_char;

        self.current_char = self.next_char;
        self.next_char = self.chars.next().unwrap_or('\0');

        self.position += previous_char.len_utf8();

        if previous_char == '\n' {
            self.current_line += 1;
            self.current_column = 1;
        } else {
            self.current_column += 1;
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
        while predicate(self.current_char) && self.current_char != '\0' {
            self.advance();
        }
    }

    fn read_number_literal(&mut self, start: LineColumn) -> Token {
        let start_position = self.position;

        self.advance_while(Self::is_number_literal_char);

        let slice = &self.source[start_position..self.position];
        let token_kind = if slice.contains('.') {
            TokenKind::Literal(Literal::Float(slice.parse().unwrap_or(0.0)))
        } else {
            TokenKind::Literal(Literal::UnsignedInteger(slice.parse().unwrap_or(0)))
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

    fn read_string_literal(&mut self, quote: char) -> Result<String, String> {
        let mut result = String::new();

        while self.current_char != quote && self.current_char != '\0' {
            if self.current_char == '\\' {
                self.advance(); // consume the backslash
                match self.current_char {
                    '"' => result.push('"'),
                    '\'' => result.push('\''),
                    '\\' => result.push('\\'),
                    'n' => result.push('\n'),
                    't' => result.push('\t'),
                    'r' => result.push('\r'),
                    '0' => result.push('\0'),
                    'u' => {
                        // Handle Unicode escape sequence \u{...}
                        self.advance(); // consume 'u'
                        if self.current_char == '{' {
                            self.advance(); // consume '{'
                            match self.read_unicode_escape() {
                                Ok(unicode_char) => result.push(unicode_char),
                                Err(_) => {
                                    // Invalid Unicode escape, treat as literal characters
                                    // We need to backtrack and treat the whole sequence as literal
                                    result.push('\\');
                                    result.push('u');
                                    result.push('{');
                                    // Continue reading characters as literals until we find '}' or end
                                    while self.current_char != '}'
                                        && self.current_char != '\0'
                                        && self.current_char != quote
                                    {
                                        result.push(self.current_char);
                                        self.advance();
                                    }
                                    if self.current_char == '}' {
                                        result.push('}');
                                        self.advance(); // consume the '}'
                                    }
                                    continue; // Skip the advance at the end since we already handled it
                                }
                            }
                        } else {
                            // Not a valid Unicode escape, treat as literal
                            result.push('\\');
                            result.push('u');
                            // Don't advance here - self.current_char is the character after 'u'
                            // which will be handled by the main loop
                            continue; // Skip the advance at the end of the match
                        }
                    }
                    ch => {
                        // For unknown escape sequences, leave them as literal characters
                        result.push('\\');
                        result.push(ch);
                    }
                }
                self.advance();
            } else {
                result.push(self.current_char);
                self.advance();
            }
        }

        if self.current_char == quote {
            self.advance(); // consume the closing quote
            Ok(result)
        } else {
            Err("Unterminated string literal".to_string())
        }
    }

    fn read_unicode_escape(&mut self) -> Result<char, String> {
        let mut hex_digits = String::new();

        // Read hex digits until we find '}' or run out of characters
        while self.current_char != '}' && self.current_char != '\0' && hex_digits.len() < 6 {
            if self.current_char.is_ascii_hexdigit() {
                hex_digits.push(self.current_char);
                self.advance();
            } else {
                // Invalid character, but don't fail - treat as literal
                return Err("Invalid character in Unicode escape sequence".to_string());
            }
        }

        if self.current_char != '}' {
            // Unterminated or invalid Unicode escape, treat as literal
            return Err("Unterminated Unicode escape sequence".to_string());
        }

        if hex_digits.is_empty() {
            return Err("Empty Unicode escape sequence".to_string());
        }

        // Parse the hex value
        let code_point = u32::from_str_radix(&hex_digits, 16)
            .map_err(|_| "Invalid hexadecimal in Unicode escape sequence".to_string())?;

        // Convert to char, checking for valid Unicode scalar value
        char::from_u32(code_point).ok_or_else(|| "Invalid Unicode code point".to_string())
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

        if self.current_char == '\0' {
            return self.token(TokenKind::Eof, start);
        }

        let ch = self.current_char;

        match ch {
            '+' => {
                self.advance();
                self.token(TokenKind::Plus, start)
            }
            '-' => {
                if self.next_char == '>' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::Arrow, start)
                } else {
                    self.advance();
                    self.token(TokenKind::Minus, start)
                }
            }
            '*' => {
                if self.next_char == '*' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::AsteriskAsterisk, start)
                } else {
                    self.advance();
                    self.token(TokenKind::Asterisk, start)
                }
            }
            '/' => {
                if self.next_char == '/' {
                    self.advance();
                    self.advance();
                    let start_position = self.position;
                    self.advance_while(|ch| ch != '\n');
                    let comment = self.source[start_position..self.position].to_string();
                    self.token(TokenKind::SingleLineComment(comment), start)
                } else if self.next_char == '*' {
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
                if self.next_char == '|' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::DoublePipe, start)
                } else if self.next_char == '>' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::Pipeline, start)
                } else {
                    self.advance();
                    self.token(TokenKind::Pipe, start)
                }
            }
            '&' => {
                if self.next_char == '&' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::DoubleAmpersand, start)
                } else {
                    self.advance();
                    self.token(TokenKind::Ampersand, start)
                }
            }
            '<' => {
                if self.next_char == '=' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::LessThanOrEqual, start)
                } else {
                    self.advance();
                    self.token(TokenKind::LessThan, start)
                }
            }
            '>' => {
                if self.next_char == '=' {
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
                if self.next_char == '=' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::EqualEqual, start)
                } else if self.next_char == '>' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::FatArrow, start)
                } else {
                    self.advance();
                    self.token(TokenKind::EqualSign, start)
                }
            }
            '!' => {
                if self.next_char == '=' {
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
                if self.next_char == ':' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::PathSeparator, start)
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
                if self.next_char == '.' {
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
            '#' => {
                self.advance();
                self.token(TokenKind::Hash, start)
            }
            '0'..='9' => self.read_number_literal(start),
            '"' | '\'' => {
                let quote = ch;
                self.advance();
                match self.read_string_literal(quote) {
                    Ok(literal) => {
                        let literal = literal.into();
                        if quote == '"' {
                            self.token(TokenKind::Literal(Literal::String(literal)), start)
                        } else {
                            self.token(TokenKind::Literal(Literal::Char(literal)), start)
                        }
                    }
                    Err(_error) => {
                        // Only for unterminated strings, return an Unknown token
                        self.token(TokenKind::Unknown, start)
                    }
                }
            }
            ch if Self::is_alphanumeric(ch) => match self.read_identifier() {
                "true" => self.token(TokenKind::Literal(Literal::Boolean(true)), start),
                "false" => self.token(TokenKind::Literal(Literal::Boolean(false)), start),
                identifier if is_keyword(identifier) => {
                    let kw = Keyword::from(identifier);
                    self.token(TokenKind::Keyword(kw), start)
                }
                identifier => {
                    let identifier_string = identifier.to_string();
                    self.token(TokenKind::Identifier(identifier_string), start)
                }
            },
            _ => self.token(TokenKind::Unknown, start),
        }
    }
}
