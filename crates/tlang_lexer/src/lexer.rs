use tlang_ast::{
    intern::intern,
    keyword::{Keyword, is_keyword},
    token::{Literal, TaggedStringPart, Token, TokenKind},
};
use tlang_span::{LineColumn, Span};

#[derive(Debug, Clone)]
pub struct Lexer<'src> {
    source: &'src str,
    chars: std::str::Chars<'src>, // char iterator over `source`.
    position: usize,
    byte_offset: u32,
    current_line: u32,
    current_column: u32,
    current_char: char,
    next_char: char,
    /// Tagged string parts read eagerly when a tagged string start is detected.
    /// The parser consumes this via `take_tagged_string_parts()`.
    pending_tagged_parts: Option<Vec<TaggedStringPart>>,
}

impl<'src> Lexer<'src> {
    pub fn new(source: &'src str) -> Lexer<'src> {
        let mut chars = source.chars();
        let current_char = chars.next().unwrap_or('\0');
        let next_char = chars.next().unwrap_or('\0');

        Lexer {
            source,
            position: 0,
            byte_offset: 0,
            current_line: 0,
            current_column: 0,
            current_char,
            next_char,
            chars,
            pending_tagged_parts: None,
        }
    }

    /// Set the initial line/column/byte offset for span tracking.
    /// Used by sub-parsers to produce spans relative to the original source.
    pub fn with_offset(mut self, line: u32, column: u32, byte_offset: u32) -> Self {
        self.current_line = line;
        self.current_column = column;
        self.byte_offset = byte_offset;
        self
    }

    pub fn set_line_offset(&mut self, line: u32) {
        self.current_line = line;
    }

    pub fn set_byte_offset(&mut self, offset: u32) {
        self.byte_offset = offset;
    }

    pub fn current_line(&self) -> u32 {
        self.current_line
    }

    pub fn current_column(&self) -> u32 {
        self.current_column
    }

    pub fn source(&self) -> &'src str {
        self.source
    }

    /// Take the eagerly-read tagged string parts buffered by the last
    /// `TaggedStringStart` token. Returns `None` if no parts are pending.
    pub fn take_tagged_string_parts(&mut self) -> Option<Vec<TaggedStringPart>> {
        self.pending_tagged_parts.take()
    }

    /// Extract the source text covered by a span, adjusting for the byte offset.
    pub fn span_text(&self, span: Span) -> &'src str {
        let start = (span.start - self.byte_offset) as usize;
        let end = (span.end - self.byte_offset) as usize;
        &self.source[start..end]
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

    fn read_number_literal(&mut self, start_pos: u32, start_lc: LineColumn) -> Token {
        let start_position = self.position;

        self.advance_while(Self::is_number_literal_char);

        let slice = &self.source[start_position..self.position];
        let token_kind = if slice.contains('.') {
            TokenKind::Literal(Literal::Float(slice.parse().unwrap_or(0.0)))
        } else {
            TokenKind::Literal(Literal::UnsignedInteger(slice.parse().unwrap_or(0)))
        };

        self.token(token_kind, start_pos, start_lc)
    }

    fn is_alphanumeric(ch: char) -> bool {
        ch.is_ascii_alphanumeric() || ch == '_'
    }

    fn read_identifier(&mut self) -> &'src str {
        let start = self.position;
        self.advance_while(Self::is_alphanumeric);
        &self.source[start..self.position]
    }

    fn read_string_literal(&mut self, quote: char) -> Result<Literal, String> {
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
                            if let Ok(unicode_char) = self.read_unicode_escape() {
                                result.push(unicode_char);
                            } else {
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
            let id = intern(&result);
            if quote == '"' {
                Ok(Literal::String(id))
            } else {
                Ok(Literal::Char(id))
            }
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

    /// Read the content of a tagged string literal, splitting it into literal text
    /// segments and raw-source interpolation segments.
    ///
    /// Interpolation is triggered by `{` followed by an identifier-start character
    /// (`[a-zA-Z_]`). This avoids ambiguity with regex quantifiers like `{2,5}`.
    ///
    /// Escaping:
    /// - `{{` → literal `{`
    /// - `\{` → literal `{`
    /// - `}}` → literal `}`
    /// - `\}` → literal `}`
    ///
    /// Inside interpolation bodies, braces are balanced and string literals are
    /// skipped so nested tagged strings like `html"<div>{ html"<span/>" }</div>"`
    /// work correctly.
    ///
    /// This method is public so the parser can call it lazily after encountering
    /// a `TaggedStringStart` token.
    pub fn read_tagged_string_parts(
        &mut self,
        quote: char,
    ) -> Result<Vec<TaggedStringPart>, String> {
        let mut parts = Vec::new();
        let mut current_literal = String::new();

        while self.current_char != quote && self.current_char != '\0' {
            if self.current_char == '\\' {
                self.advance(); // consume backslash
                match self.current_char {
                    '{' | '}' => {
                        current_literal.push(self.current_char);
                        self.advance();
                    }
                    '"' => {
                        current_literal.push('"');
                        self.advance();
                    }
                    '\'' => {
                        current_literal.push('\'');
                        self.advance();
                    }
                    '\\' => {
                        current_literal.push('\\');
                        self.advance();
                    }
                    'n' => {
                        current_literal.push('\n');
                        self.advance();
                    }
                    't' => {
                        current_literal.push('\t');
                        self.advance();
                    }
                    'r' => {
                        current_literal.push('\r');
                        self.advance();
                    }
                    '0' => {
                        current_literal.push('\0');
                        self.advance();
                    }
                    ch => {
                        // Unknown escape → keep as-is (e.g. \d for regex)
                        current_literal.push('\\');
                        current_literal.push(ch);
                        self.advance();
                    }
                }
            } else if self.current_char == '{' {
                if self.next_char == '{' {
                    // `{{` → literal `{`
                    current_literal.push('{');
                    self.advance(); // consume first `{`
                    self.advance(); // consume second `{`
                } else if self.next_char.is_ascii_alphabetic() || self.next_char == '_' {
                    // Start of interpolation
                    parts.push(TaggedStringPart::Literal(
                        std::mem::take(&mut current_literal).into_boxed_str(),
                    ));
                    self.advance(); // consume `{`
                    // Capture the position of the first char inside the interpolation body
                    let interp_line = self.current_line;
                    let interp_column = self.current_column;
                    let interp_byte_offset = self.byte_position();
                    let raw = self.read_interpolation_body(quote)?;
                    parts.push(TaggedStringPart::Interpolation {
                        source: raw.into_boxed_str(),
                        line: interp_line,
                        column: interp_column,
                        byte_offset: interp_byte_offset,
                    });
                    // `}` already consumed by read_interpolation_body
                } else {
                    // Not an interpolation (e.g. `{2,5}` regex quantifier)
                    current_literal.push('{');
                    self.advance();
                }
            } else if self.current_char == '}' && self.next_char == '}' {
                // `}}` → literal `}`
                current_literal.push('}');
                self.advance();
                self.advance();
            } else {
                current_literal.push(self.current_char);
                self.advance();
            }
        }

        if self.current_char == quote {
            self.advance(); // consume closing quote
            // Always push trailing literal part
            parts.push(TaggedStringPart::Literal(current_literal.into_boxed_str()));
            Ok(parts)
        } else {
            Err("Unterminated tagged string literal".to_string())
        }
    }

    /// Read the raw source text of an interpolation body between `{` and `}`.
    /// The opening `{` must already be consumed. The closing `}` is consumed.
    /// Handles nested braces and skips over string literals to avoid confusion
    /// with the outer tagged string's closing quote.
    fn read_interpolation_body(&mut self, _outer_quote: char) -> Result<String, String> {
        let mut raw = String::new();
        let mut depth: u32 = 1;

        while depth > 0 && self.current_char != '\0' {
            match self.current_char {
                '{' => {
                    depth += 1;
                    raw.push('{');
                    self.advance();
                }
                '}' => {
                    depth -= 1;
                    if depth > 0 {
                        raw.push('}');
                        self.advance();
                    } else {
                        self.advance(); // consume final `}`
                    }
                }
                '"' | '\'' => {
                    // Skip over a nested string literal (including tagged strings)
                    let string_quote = self.current_char;
                    raw.push(string_quote);
                    self.advance();
                    while self.current_char != string_quote && self.current_char != '\0' {
                        if self.current_char == '\\' {
                            raw.push('\\');
                            self.advance();
                            if self.current_char != '\0' {
                                raw.push(self.current_char);
                                self.advance();
                            }
                        } else {
                            raw.push(self.current_char);
                            self.advance();
                        }
                    }
                    if self.current_char == string_quote {
                        raw.push(string_quote);
                        self.advance();
                    }
                }
                _ => {
                    raw.push(self.current_char);
                    self.advance();
                }
            }
        }

        if depth != 0 {
            return Err("Unterminated interpolation in tagged string".to_string());
        }

        Ok(raw)
    }

    fn line_column(&self) -> LineColumn {
        LineColumn {
            line: self.current_line,
            column: self.current_column,
        }
    }

    fn byte_position(&self) -> u32 {
        self.position as u32 + self.byte_offset
    }

    fn token(&self, kind: TokenKind, start_pos: u32, start_lc: LineColumn) -> Token {
        Token::new(
            kind,
            Span::new(
                start_pos,
                self.byte_position(),
                start_lc,
                self.line_column(),
            ),
        )
    }

    pub fn next_token(&mut self) -> Token {
        self.skip_whitespace();

        let start_pos = self.byte_position();
        let start_lc = self.line_column();

        if self.current_char == '\0' {
            return self.token(TokenKind::Eof, start_pos, start_lc);
        }

        let ch = self.current_char;

        match ch {
            '+' => {
                self.advance();
                self.token(TokenKind::Plus, start_pos, start_lc)
            }
            '-' => {
                if self.next_char == '>' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::Arrow, start_pos, start_lc)
                } else {
                    self.advance();
                    self.token(TokenKind::Minus, start_pos, start_lc)
                }
            }
            '*' => {
                if self.next_char == '*' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::AsteriskAsterisk, start_pos, start_lc)
                } else {
                    self.advance();
                    self.token(TokenKind::Asterisk, start_pos, start_lc)
                }
            }
            '/' => {
                if self.next_char == '/' {
                    self.advance();
                    self.advance();
                    self.advance_while(|ch| ch != '\n');
                    self.token(TokenKind::SingleLineComment, start_pos, start_lc)
                } else if self.next_char == '*' {
                    self.advance();
                    self.advance();
                    while !(self.source[self.position..].starts_with("*/")
                        || self.position >= self.source.len())
                    {
                        self.advance();
                    }
                    self.advance();
                    self.advance();
                    self.token(TokenKind::MultiLineComment, start_pos, start_lc)
                } else {
                    self.advance();
                    self.token(TokenKind::Slash, start_pos, start_lc)
                }
            }
            '%' => {
                self.advance();
                self.token(TokenKind::Percent, start_pos, start_lc)
            }
            '^' => {
                self.advance();
                self.token(TokenKind::Caret, start_pos, start_lc)
            }
            '|' => {
                if self.next_char == '|' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::DoublePipe, start_pos, start_lc)
                } else if self.next_char == '>' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::Pipeline, start_pos, start_lc)
                } else {
                    self.advance();
                    self.token(TokenKind::Pipe, start_pos, start_lc)
                }
            }
            '&' => {
                if self.next_char == '&' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::DoubleAmpersand, start_pos, start_lc)
                } else {
                    self.advance();
                    self.token(TokenKind::Ampersand, start_pos, start_lc)
                }
            }
            '<' => {
                if self.next_char == '=' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::LessThanOrEqual, start_pos, start_lc)
                } else {
                    self.advance();
                    self.token(TokenKind::LessThan, start_pos, start_lc)
                }
            }
            '>' => {
                if self.next_char == '=' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::GreaterThanOrEqual, start_pos, start_lc)
                } else {
                    self.advance();
                    self.token(TokenKind::GreaterThan, start_pos, start_lc)
                }
            }
            '(' => {
                self.advance();
                self.token(TokenKind::LParen, start_pos, start_lc)
            }
            ')' => {
                self.advance();
                self.token(TokenKind::RParen, start_pos, start_lc)
            }
            '{' => {
                self.advance();
                self.token(TokenKind::LBrace, start_pos, start_lc)
            }
            '}' => {
                self.advance();
                self.token(TokenKind::RBrace, start_pos, start_lc)
            }
            '[' => {
                self.advance();
                self.token(TokenKind::LBracket, start_pos, start_lc)
            }
            ']' => {
                self.advance();
                self.token(TokenKind::RBracket, start_pos, start_lc)
            }
            ',' => {
                self.advance();
                self.token(TokenKind::Comma, start_pos, start_lc)
            }
            '=' => {
                if self.next_char == '=' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::EqualEqual, start_pos, start_lc)
                } else if self.next_char == '>' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::FatArrow, start_pos, start_lc)
                } else if self.next_char == '~' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::Matches, start_pos, start_lc)
                } else {
                    self.advance();
                    self.token(TokenKind::EqualSign, start_pos, start_lc)
                }
            }
            '!' => {
                if self.next_char == '=' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::NotEqual, start_pos, start_lc)
                } else if self.next_char == '~' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::NotMatches, start_pos, start_lc)
                } else {
                    self.advance();
                    self.token(TokenKind::ExclamationMark, start_pos, start_lc)
                }
            }
            '?' => {
                self.advance();
                self.token(TokenKind::QuestionMark, start_pos, start_lc)
            }
            ':' => {
                if self.next_char == ':' {
                    self.advance();
                    self.advance();
                    self.token(TokenKind::PathSeparator, start_pos, start_lc)
                } else {
                    self.advance();
                    self.token(TokenKind::Colon, start_pos, start_lc)
                }
            }
            ';' => {
                self.advance();
                self.token(TokenKind::Semicolon, start_pos, start_lc)
            }
            '.' => {
                if self.next_char == '.' {
                    if self.source[self.position + 1..].starts_with("..") {
                        self.advance();
                        self.advance();
                        self.advance();
                        self.token(TokenKind::DotDotDot, start_pos, start_lc)
                    } else {
                        self.advance();
                        self.advance();
                        self.token(TokenKind::DotDot, start_pos, start_lc)
                    }
                } else {
                    self.advance();
                    self.token(TokenKind::Dot, start_pos, start_lc)
                }
            }
            '#' => {
                self.advance();
                self.token(TokenKind::Hash, start_pos, start_lc)
            }
            '0'..='9' => self.read_number_literal(start_pos, start_lc),
            '"' | '\'' => {
                let quote = ch;
                self.advance();
                match self.read_string_literal(quote) {
                    Ok(literal) => self.token(TokenKind::Literal(literal), start_pos, start_lc),
                    Err(_error) => {
                        // Only for unterminated strings, return an Unknown token
                        self.token(TokenKind::Unknown, start_pos, start_lc)
                    }
                }
            }
            ch if Self::is_alphanumeric(ch) => {
                let identifier = self.read_identifier();
                match identifier {
                    "true" => self.token(
                        TokenKind::Literal(Literal::Boolean(true)),
                        start_pos,
                        start_lc,
                    ),
                    "false" => self.token(
                        TokenKind::Literal(Literal::Boolean(false)),
                        start_pos,
                        start_lc,
                    ),
                    identifier if is_keyword(identifier) => {
                        let kw = Keyword::from(identifier);
                        self.token(TokenKind::Keyword(kw), start_pos, start_lc)
                    }
                    _identifier => {
                        // Check if the identifier is immediately followed by a quote
                        // (no whitespace), which makes it a tagged string literal.
                        if matches!(self.current_char, '"' | '\'') {
                            let quote = self.current_char;
                            self.advance(); // consume the opening quote
                            // Create the token BEFORE reading content so the span
                            // covers only "tag<quote>" (not the string content).
                            let tag_token = self.token(
                                TokenKind::TaggedStringStart(quote),
                                start_pos,
                                start_lc,
                            );
                            // Eagerly read the tagged string content so the lexer
                            // is positioned past the closing quote before the parser
                            // pre-loads the next token via lookahead.
                            let parts = match self.read_tagged_string_parts(quote) {
                                Ok(p) => p,
                                Err(_) => {
                                    return self.token(TokenKind::Unknown, start_pos, start_lc);
                                }
                            };
                            self.pending_tagged_parts = Some(parts);
                            return tag_token;
                        }
                        self.token(TokenKind::Identifier, start_pos, start_lc)
                    }
                }
            }
            _ => self.token(TokenKind::Unknown, start_pos, start_lc),
        }
    }
}
