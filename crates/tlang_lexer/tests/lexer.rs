use pretty_assertions::assert_eq;
use tlang_intern::intern;
use tlang_ast::keyword::Keyword;
use tlang_ast::token::{Literal, TaggedStringPart, TokenKind};

use tlang_lexer::Lexer;

macro_rules! assert_tokens {
    ($lexer:ident, $tokens:expr) => {{
        let tokens = $tokens;
        let mut tokens = tokens.iter();

        loop {
            let token = $lexer.next_token();
            match tokens.next() {
                Some(expected) => assert_eq!(token.kind, *expected),
                None => break,
            }
        }

        assert_eq!($lexer.next_token().kind, TokenKind::Eof);
    }};
}

#[test]
fn test_identifier() {
    let mut lexer = Lexer::new("test");

    assert_tokens!(lexer, [TokenKind::Identifier]);
}

#[test]
fn test_integer() {
    let mut lexer = Lexer::new("123");

    assert_tokens!(lexer, [TokenKind::Literal(Literal::UnsignedInteger(123))]);
}

#[test]
fn test_float() {
    let mut lexer = Lexer::new("123.456");

    assert_tokens!(lexer, [TokenKind::Literal(Literal::Float(123.456))]);
}

#[test]
fn test_typed_integer() {
    let mut lexer = Lexer::new("123i64");

    assert_tokens!(
        lexer,
        [
            TokenKind::Literal(Literal::UnsignedInteger(123)),
            TokenKind::Identifier
        ]
    );
}

#[test]
fn test_typed_float() {
    let mut lexer = Lexer::new("123.456f64");

    assert_tokens!(
        lexer,
        [
            TokenKind::Literal(Literal::Float(123.456)),
            TokenKind::Identifier
        ]
    );
}

#[test]
fn test_single_char_tokens() {
    let mut lexer = Lexer::new(". + - * / % ( ) | & ^ #");

    assert_tokens!(
        lexer,
        [
            TokenKind::Dot,
            TokenKind::Plus,
            TokenKind::Minus,
            TokenKind::Asterisk,
            TokenKind::Slash,
            TokenKind::Percent,
            TokenKind::LParen,
            TokenKind::RParen,
            TokenKind::Pipe,
            TokenKind::Ampersand,
            TokenKind::Caret,
            TokenKind::Hash,
        ]
    );
}

#[test]
fn test_multi_char_tokens() {
    let mut lexer = Lexer::new("|| && == != >= <= ** |>");

    assert_tokens!(
        lexer,
        [
            TokenKind::DoublePipe,
            TokenKind::DoubleAmpersand,
            TokenKind::EqualEqual,
            TokenKind::NotEqual,
            TokenKind::GreaterThanOrEqual,
            TokenKind::LessThanOrEqual,
            TokenKind::AsteriskAsterisk,
            TokenKind::Pipeline,
        ]
    );
}

#[test]
fn test_arrows() {
    let mut lexer = Lexer::new("-> =>");

    assert_tokens!(lexer, [TokenKind::Arrow, TokenKind::FatArrow,]);
}

#[test]
fn test_dotdot_dot() {
    let mut lexer = Lexer::new(".. ...");

    assert_tokens!(lexer, [TokenKind::DotDot, TokenKind::DotDotDot]);
}

#[test]
fn test_path_separator() {
    let mut lexer = Lexer::new("::");

    assert_tokens!(lexer, [TokenKind::PathSeparator]);
}

#[test]
fn test_keywords() {
    let mut lexer = Lexer::new("let fn rec return if else match enum struct and or self");

    assert_tokens!(
        lexer,
        [
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Keyword(Keyword::Fn),
            TokenKind::Keyword(Keyword::Rec),
            TokenKind::Keyword(Keyword::Return),
            TokenKind::Keyword(Keyword::If),
            TokenKind::Keyword(Keyword::Else),
            TokenKind::Keyword(Keyword::Match),
            TokenKind::Keyword(Keyword::Enum),
            TokenKind::Keyword(Keyword::Struct),
            TokenKind::Keyword(Keyword::And),
            TokenKind::Keyword(Keyword::Or),
            TokenKind::Keyword(Keyword::_Self),
        ]
    );
}

#[test]
fn test_equal_sign() {
    let mut lexer = Lexer::new("=");

    assert_tokens!(lexer, [TokenKind::EqualSign]);
}

#[test]
fn test_comparison_operators() {
    let mut lexer = Lexer::new("== != >= <= > <");

    assert_tokens!(
        lexer,
        [
            TokenKind::EqualEqual,
            TokenKind::NotEqual,
            TokenKind::GreaterThanOrEqual,
            TokenKind::LessThanOrEqual,
            TokenKind::GreaterThan,
            TokenKind::LessThan,
        ]
    );
}

#[test]
fn test_simple_assignment() {
    let mut lexer = Lexer::new("let x = 1 + 2;");

    assert_tokens!(
        lexer,
        [
            TokenKind::Keyword(Keyword::Let),
            TokenKind::Identifier,
            TokenKind::EqualSign,
            TokenKind::Literal(Literal::UnsignedInteger(1)),
            TokenKind::Plus,
            TokenKind::Literal(Literal::UnsignedInteger(2)),
            TokenKind::Semicolon,
        ]
    );
}

#[test]
fn test_function_declaration_with_explicit_return_statement() {
    let mut lexer = Lexer::new("fn add(a: i64, b: i64) -> i64 { return a + b; }");

    assert_tokens!(
        lexer,
        [
            TokenKind::Keyword(Keyword::Fn),
            TokenKind::Identifier,
            TokenKind::LParen,
            TokenKind::Identifier,
            TokenKind::Colon,
            TokenKind::Identifier,
            TokenKind::Comma,
            TokenKind::Identifier,
            TokenKind::Colon,
            TokenKind::Identifier,
            TokenKind::RParen,
            TokenKind::Arrow,
            TokenKind::Identifier,
            TokenKind::LBrace,
            TokenKind::Keyword(Keyword::Return),
            TokenKind::Identifier,
            TokenKind::Plus,
            TokenKind::Identifier,
            TokenKind::Semicolon,
            TokenKind::RBrace,
        ]
    );
}

#[test]
fn test_regression_integer_literal_followed_by_dot() {
    let mut lexer = Lexer::new("fn sum([]) { 0 }\nfn sum([x, ...xs]) { x + sum(xs) }");

    assert_tokens!(
        lexer,
        [
            TokenKind::Keyword(Keyword::Fn),
            TokenKind::Identifier,
            TokenKind::LParen,
            TokenKind::LBracket,
            TokenKind::RBracket,
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::Literal(Literal::UnsignedInteger(0)),
            TokenKind::RBrace,
            TokenKind::Keyword(Keyword::Fn),
            TokenKind::Identifier,
            TokenKind::LParen,
            TokenKind::LBracket,
            TokenKind::Identifier,
            TokenKind::Comma,
            TokenKind::DotDotDot,
            TokenKind::Identifier,
            TokenKind::RBracket,
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::Identifier,
            TokenKind::Plus,
            TokenKind::Identifier,
            TokenKind::LParen,
            TokenKind::Identifier,
            TokenKind::RParen,
            TokenKind::RBrace,
        ]
    );
}

#[test]
fn test_identifier_span_text() {
    let mut lexer = Lexer::new("test");
    let token = lexer.next_token();
    assert_eq!(token.kind, TokenKind::Identifier);
    assert_eq!(lexer.span_text(token.span), "test");
}

#[test]
fn test_single_line_comments() {
    let mut lexer = Lexer::new("// this is a comment");

    assert_tokens!(lexer, [TokenKind::SingleLineComment]);
}

#[test]
fn test_single_line_comment_span_text() {
    let mut lexer = Lexer::new("// this is a comment");
    let token = lexer.next_token();
    assert_eq!(token.kind, TokenKind::SingleLineComment);
    // Span covers the whole "// this is a comment"
    let text = lexer.span_text(token.span);
    assert_eq!(&text[2..], " this is a comment");
}

#[test]
fn test_multi_line_comments() {
    let mut lexer = Lexer::new("/* this is a comment */");
    assert_tokens!(lexer, [TokenKind::MultiLineComment]);

    let mut lexer = Lexer::new("/* this is a comment\non multiple lines */");
    assert_tokens!(lexer, [TokenKind::MultiLineComment]);
}

#[test]
fn test_multi_line_comment_span_text() {
    let mut lexer = Lexer::new("/* this is a comment */");
    let token = lexer.next_token();
    let text = lexer.span_text(token.span);
    // Strip /* and */
    assert_eq!(&text[2..text.len() - 2], " this is a comment ");
}

#[test]
fn test_string_literal() {
    let mut lexer = Lexer::new("\"this is a string\"");

    assert_tokens!(
        lexer,
        [TokenKind::Literal(Literal::String(intern(
            "this is a string"
        )))]
    );
}

#[test]
fn test_string_escape_sequences() {
    let mut lexer = Lexer::new("\"Quote: \\\"Hello\\\"\"");
    assert_tokens!(
        lexer,
        [TokenKind::Literal(Literal::String(intern(
            "Quote: \"Hello\""
        )))]
    );

    let mut lexer = Lexer::new("\"Backslash: \\\\\"");
    assert_tokens!(
        lexer,
        [TokenKind::Literal(Literal::String(intern("Backslash: \\")))]
    );

    let mut lexer = Lexer::new("\"Newline: \\n\"");
    assert_tokens!(
        lexer,
        [TokenKind::Literal(Literal::String(intern("Newline: \n")))]
    );

    let mut lexer = Lexer::new("\"Tab: \\t\"");
    assert_tokens!(
        lexer,
        [TokenKind::Literal(Literal::String(intern("Tab: \t")))]
    );

    let mut lexer = Lexer::new("\"Carriage: \\r\"");
    assert_tokens!(
        lexer,
        [TokenKind::Literal(Literal::String(intern("Carriage: \r")))]
    );

    let mut lexer = Lexer::new("\"Null: \\0\"");
    assert_tokens!(
        lexer,
        [TokenKind::Literal(Literal::String(intern("Null: \0")))]
    );
}

#[test]
fn test_char_escape_sequences() {
    let mut lexer = Lexer::new("'\\''");
    assert_tokens!(lexer, [TokenKind::Literal(Literal::Char(intern("'")))]);

    let mut lexer = Lexer::new("'\\\"'");
    assert_tokens!(lexer, [TokenKind::Literal(Literal::Char(intern("\"")))]);

    let mut lexer = Lexer::new("'\\\\'");
    assert_tokens!(lexer, [TokenKind::Literal(Literal::Char(intern("\\")))]);
}

#[test]
fn test_invalid_escape_sequences() {
    let mut lexer = Lexer::new("\"Invalid: \\z\"");
    let token = lexer.next_token();
    assert_eq!(
        token.kind,
        TokenKind::Literal(Literal::String(intern("Invalid: \\z")))
    );

    let mut lexer = Lexer::new("\"Another: \\x\"");
    let token = lexer.next_token();
    assert_eq!(
        token.kind,
        TokenKind::Literal(Literal::String(intern("Another: \\x")))
    );
}

#[test]
fn test_char_literal() {
    let mut lexer = Lexer::new("'a'");

    assert_tokens!(lexer, [TokenKind::Literal(Literal::Char(intern("a")))]);
}

#[test]
fn test_token_span() {
    let mut lexer = Lexer::new("let x = 1 + 2;");

    let token = lexer.next_token();
    assert_eq!(token.span.start_lc.line, 0);
    assert_eq!(token.span.start_lc.column, 0);
    assert_eq!(token.span.end_lc.line, 0);
    assert_eq!(token.span.end_lc.column, 3);

    let token = lexer.next_token();
    assert_eq!(token.span.start_lc.line, 0);
    assert_eq!(token.span.start_lc.column, 4);
    assert_eq!(token.span.end_lc.line, 0);
    assert_eq!(token.span.end_lc.column, 5);
}

#[test]
fn test_underscore_keyword() {
    let mut lexer = Lexer::new("_ _test");

    assert_tokens!(
        lexer,
        [
            TokenKind::Keyword(Keyword::Underscore),
            TokenKind::Identifier
        ]
    );
}

// ── Tagged string tests ────────────────────────────────────────────────────────

/// Helper: lex a tagged string start token and take the eagerly-read parts.
fn lex_tagged_string(source: &str) -> (String, Vec<TaggedStringPart>) {
    let mut lexer = Lexer::new(source);
    let token = lexer.next_token();
    let TokenKind::TaggedStringStart(quote) = token.kind else {
        panic!("Expected TaggedStringStart, got {:?}", token.kind);
    };
    // Extract tag from span text (everything before the trailing quote)
    let full = lexer.span_text(token.span);
    let tag = full[..full.len() - quote.len_utf8()].to_string();
    let parts = lexer.take_tagged_string_parts().unwrap();
    (tag, parts)
}

#[test]
fn test_tagged_string_simple() {
    let (tag, parts) = lex_tagged_string(r#"re"\d+""#);
    assert_eq!(tag, "re");
    assert_eq!(parts, vec![TaggedStringPart::Literal("\\d+".into())]);
}

#[test]
fn test_tagged_string_no_interpolation() {
    let (tag, parts) = lex_tagged_string(r#"html"<div>hello</div>""#);
    assert_eq!(tag, "html");
    assert_eq!(
        parts,
        vec![TaggedStringPart::Literal("<div>hello</div>".into())]
    );
}

#[test]
fn test_tagged_string_with_interpolation() {
    let (tag, parts) = lex_tagged_string(r#"html"<div>{name}</div>""#);
    assert_eq!(tag, "html");
    assert_eq!(parts.len(), 3);
    assert_eq!(parts[0], TaggedStringPart::Literal("<div>".into()));
    match &parts[1] {
        TaggedStringPart::Interpolation { source, .. } => {
            assert_eq!(source.as_ref(), "name");
        }
        _ => panic!("Expected Interpolation part"),
    }
    assert_eq!(parts[2], TaggedStringPart::Literal("</div>".into()));
}

#[test]
fn test_tagged_string_multiple_interpolations() {
    let (tag, parts) = lex_tagged_string(r#"sql"SELECT {cols} FROM {table}""#);
    assert_eq!(tag, "sql");
    assert_eq!(parts.len(), 5);
    assert_eq!(parts[0], TaggedStringPart::Literal("SELECT ".into()));
    match &parts[1] {
        TaggedStringPart::Interpolation { source, .. } => assert_eq!(source.as_ref(), "cols"),
        _ => panic!("Expected Interpolation"),
    }
    assert_eq!(parts[2], TaggedStringPart::Literal(" FROM ".into()));
    match &parts[3] {
        TaggedStringPart::Interpolation { source, .. } => assert_eq!(source.as_ref(), "table"),
        _ => panic!("Expected Interpolation"),
    }
    assert_eq!(parts[4], TaggedStringPart::Literal("".into()));
}

#[test]
fn test_tagged_string_regex_quantifier_not_interpolation() {
    let (tag, parts) = lex_tagged_string(r#"re"\d{2,5}""#);
    assert_eq!(tag, "re");
    assert_eq!(parts, vec![TaggedStringPart::Literal("\\d{2,5}".into())]);
}

#[test]
fn test_tagged_string_double_brace_escape() {
    let (tag, parts) = lex_tagged_string(r#"html"{{escaped}}""#);
    assert_eq!(tag, "html");
    assert_eq!(parts, vec![TaggedStringPart::Literal("{escaped}".into())]);
}

#[test]
fn test_tagged_string_backslash_brace_escape() {
    let (tag, parts) = lex_tagged_string(r#"html"\{escaped\}""#);
    assert_eq!(tag, "html");
    assert_eq!(parts, vec![TaggedStringPart::Literal("{escaped}".into())]);
}

#[test]
fn test_tagged_string_nested_braces_in_interpolation() {
    let (tag, parts) = lex_tagged_string(r#"tag"{if true { "a" } else { "b" }}""#);
    assert_eq!(tag, "tag");
    assert_eq!(parts.len(), 3);
    match &parts[1] {
        TaggedStringPart::Interpolation { source, .. } => {
            assert_eq!(source.as_ref(), "if true { \"a\" } else { \"b\" }");
        }
        _ => panic!("Expected Interpolation"),
    }
}

#[test]
fn test_tagged_string_nested_tagged_string_in_interpolation() {
    let (tag, parts) = lex_tagged_string(r#"html"<div>{html"<span/>"}</div>""#);
    assert_eq!(tag, "html");
    assert_eq!(parts.len(), 3);
    assert_eq!(parts[0], TaggedStringPart::Literal("<div>".into()));
    match &parts[1] {
        TaggedStringPart::Interpolation { source, .. } => {
            assert_eq!(source.as_ref(), "html\"<span/>\"");
        }
        _ => panic!("Expected Interpolation"),
    }
    assert_eq!(parts[2], TaggedStringPart::Literal("</div>".into()));
}

#[test]
fn test_tagged_string_interpolation_with_member_access() {
    let (tag, parts) = lex_tagged_string(r#"html"{user.name}""#);
    assert_eq!(tag, "html");
    match &parts[1] {
        TaggedStringPart::Interpolation { source, .. } => {
            assert_eq!(source.as_ref(), "user.name");
        }
        _ => panic!("Expected Interpolation"),
    }
}

#[test]
fn test_tagged_string_interpolation_position_tracking() {
    let (_, parts) = lex_tagged_string(r#"html"<div>{name}</div>""#);
    match &parts[1] {
        TaggedStringPart::Interpolation {
            source,
            line,
            column,
            byte_offset,
        } => {
            assert_eq!(source.as_ref(), "name");
            assert_eq!(*line, 0);
            assert_eq!(*column, 11);
            assert_eq!(*byte_offset, 11);
        }
        _ => panic!("Expected Interpolation part"),
    }
}
