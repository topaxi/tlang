use tlang_ast::{node::Module, token::Literal, visit::Visitor};
use tlang_span::Span;

use crate::{analyzer::SemanticAnalysisContext, analyzer::SemanticAnalysisPass, diagnostic};

/**
 * String literal validation pass that validates escape sequences
 * in string and character literals.
 */
pub struct StringLiteralValidator;

impl StringLiteralValidator {
    fn validate_escape_sequences(
        &mut self,
        string_content: &str,
        span: Span,
        ctx: &mut SemanticAnalysisContext,
    ) {
        let mut chars = string_content.chars().peekable();

        while let Some(ch) = chars.next() {
            if ch == '\\'
                && let Some(&next_ch) = chars.peek()
            {
                // Check if this is a valid escape sequence
                // Valid escape sequences in tlang: \", \', \\, \n, \t, \r, \0, \u{...}
                match next_ch {
                    '"' | '\'' | '\\' | 'n' | 't' | 'r' | '0' => {
                        // Valid escape sequence, no warning needed
                        chars.next(); // consume the character after backslash
                    }
                    'u' => {
                        // Check for Unicode escape sequence \u{...}
                        chars.next(); // consume 'u'
                        if let Some(&'{') = chars.peek() {
                            chars.next(); // consume '{'
                            let mut hex_digits = String::new();
                            let mut found_closing_brace = false;

                            // Collect hex digits until we find '}'
                            while let Some(&hex_ch) = chars.peek() {
                                if hex_ch == '}' {
                                    chars.next(); // consume '}'
                                    found_closing_brace = true;
                                    break;
                                } else if hex_ch.is_ascii_hexdigit() && hex_digits.len() < 6 {
                                    hex_digits.push(hex_ch);
                                    chars.next();
                                } else {
                                    // Invalid character or too many digits
                                    break;
                                }
                            }

                            // Validate the Unicode escape sequence
                            if !found_closing_brace {
                                ctx.add_diagnostic(diagnostic::warn_at!(
                                    span,
                                    "Unterminated Unicode escape sequence in string literal",
                                ));
                            } else if hex_digits.is_empty() {
                                ctx.add_diagnostic(diagnostic::warn_at!(
                                    span,
                                    "Empty Unicode escape sequence in string literal",
                                ));
                            } else if let Ok(code_point) = u32::from_str_radix(&hex_digits, 16) {
                                if char::from_u32(code_point).is_none() {
                                    ctx.add_diagnostic(diagnostic::warn_at!(
                                        span,
                                        "Invalid Unicode code point in string literal",
                                    ));
                                }
                                // Valid Unicode escape sequence, no warning needed
                            } else {
                                ctx.add_diagnostic(diagnostic::warn_at!(
                                    span,
                                    "Invalid hexadecimal in Unicode escape sequence",
                                ));
                            }
                        } else {
                            // \u not followed by {, treat as unknown escape sequence
                            ctx.add_diagnostic(diagnostic::warn_at!(
                                span,
                                "Unknown escape sequence '\\{}' in string literal",
                                next_ch
                            ));
                        }
                    }
                    _ => {
                        // Unknown escape sequence, emit warning
                        ctx.add_diagnostic(diagnostic::warn_at!(
                            span,
                            "Unknown escape sequence '\\{}' in string literal",
                            next_ch
                        ));
                        chars.next(); // consume the character after backslash
                    }
                }
            }
        }
    }
}

impl SemanticAnalysisPass for StringLiteralValidator {
    fn analyze(&mut self, module: &Module, ctx: &mut SemanticAnalysisContext, _is_root: bool) {
        self.visit_module(module, ctx);
    }
}

impl<'ast> Visitor<'ast> for StringLiteralValidator {
    type Context = SemanticAnalysisContext;

    fn visit_literal(&mut self, literal: &'ast Literal, span: Span, ctx: &mut Self::Context) {
        match literal {
            Literal::String(string_content) | Literal::Char(string_content) => {
                self.validate_escape_sequences(string_content, span, ctx);
            }
            _ => {
                // No validation needed for other literal types
            }
        }
    }
}
