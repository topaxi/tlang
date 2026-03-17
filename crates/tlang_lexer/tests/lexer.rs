use pretty_assertions::assert_eq;
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

    assert_tokens!(lexer, [TokenKind::Identifier("test".into())]);
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
            TokenKind::Identifier("i64".into())
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
            TokenKind::Identifier("f64".into())
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
            TokenKind::Identifier("x".into()),
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
            TokenKind::Identifier("add".into()),
            TokenKind::LParen,
            TokenKind::Identifier("a".into()),
            TokenKind::Colon,
            TokenKind::Identifier("i64".into()),
            TokenKind::Comma,
            TokenKind::Identifier("b".into()),
            TokenKind::Colon,
            TokenKind::Identifier("i64".into()),
            TokenKind::RParen,
            TokenKind::Arrow,
            TokenKind::Identifier("i64".into()),
            TokenKind::LBrace,
            TokenKind::Keyword(Keyword::Return),
            TokenKind::Identifier("a".into()),
            TokenKind::Plus,
            TokenKind::Identifier("b".into()),
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
            TokenKind::Identifier("sum".into()),
            TokenKind::LParen,
            TokenKind::LBracket,
            TokenKind::RBracket,
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::Literal(Literal::UnsignedInteger(0)),
            TokenKind::RBrace,
            TokenKind::Keyword(Keyword::Fn),
            TokenKind::Identifier("sum".into()),
            TokenKind::LParen,
            TokenKind::LBracket,
            TokenKind::Identifier("x".into()),
            TokenKind::Comma,
            TokenKind::DotDotDot,
            TokenKind::Identifier("xs".into()),
            TokenKind::RBracket,
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::Identifier("x".into()),
            TokenKind::Plus,
            TokenKind::Identifier("sum".into()),
            TokenKind::LParen,
            TokenKind::Identifier("xs".into()),
            TokenKind::RParen,
            TokenKind::RBrace,
        ]
    );
}

#[test]
fn test_single_line_comments() {
    let mut lexer = Lexer::new("// this is a comment");

    assert_tokens!(
        lexer,
        [TokenKind::SingleLineComment(" this is a comment".into())]
    );
}

#[test]
fn test_multi_line_comments() {
    let mut lexer = Lexer::new("/* this is a comment */");

    assert_tokens!(
        lexer,
        [TokenKind::MultiLineComment(" this is a comment ".into())]
    );

    let mut lexer = Lexer::new("/* this is a comment\non multiple lines */");

    assert_tokens!(
        lexer,
        [TokenKind::MultiLineComment(
            " this is a comment\non multiple lines ".into()
        )]
    );
}

#[test]
fn test_string_literal() {
    let mut lexer = Lexer::new("\"this is a string\"");

    assert_tokens!(
        lexer,
        [TokenKind::Literal(Literal::String(
            "this is a string".into()
        ))]
    );
}

#[test]
fn test_string_escape_sequences() {
    let mut lexer = Lexer::new("\"Quote: \\\"Hello\\\"\"");
    assert_tokens!(
        lexer,
        [TokenKind::Literal(Literal::String(
            "Quote: \"Hello\"".into()
        ))]
    );

    let mut lexer = Lexer::new("\"Backslash: \\\\\"");
    assert_tokens!(
        lexer,
        [TokenKind::Literal(Literal::String("Backslash: \\".into()))]
    );

    let mut lexer = Lexer::new("\"Newline: \\n\"");
    assert_tokens!(
        lexer,
        [TokenKind::Literal(Literal::String("Newline: \n".into()))]
    );

    let mut lexer = Lexer::new("\"Tab: \\t\"");
    assert_tokens!(
        lexer,
        [TokenKind::Literal(Literal::String("Tab: \t".into()))]
    );

    let mut lexer = Lexer::new("\"Carriage: \\r\"");
    assert_tokens!(
        lexer,
        [TokenKind::Literal(Literal::String("Carriage: \r".into()))]
    );

    let mut lexer = Lexer::new("\"Null: \\0\"");
    assert_tokens!(
        lexer,
        [TokenKind::Literal(Literal::String("Null: \0".into()))]
    );
}

#[test]
fn test_char_escape_sequences() {
    let mut lexer = Lexer::new("'\\''");
    assert_tokens!(lexer, [TokenKind::Literal(Literal::Char("'".into()))]);

    let mut lexer = Lexer::new("'\\\"'");
    assert_tokens!(lexer, [TokenKind::Literal(Literal::Char("\"".into()))]);

    let mut lexer = Lexer::new("'\\\\'");
    assert_tokens!(lexer, [TokenKind::Literal(Literal::Char("\\".into()))]);
}

#[test]
fn test_invalid_escape_sequences() {
    let mut lexer = Lexer::new("\"Invalid: \\z\"");
    let token = lexer.next_token();
    assert_eq!(
        token.kind,
        TokenKind::Literal(Literal::String("Invalid: \\z".into()))
    );

    let mut lexer = Lexer::new("\"Another: \\x\"");
    let token = lexer.next_token();
    assert_eq!(
        token.kind,
        TokenKind::Literal(Literal::String("Another: \\x".into()))
    );
}

#[test]
fn test_char_literal() {
    let mut lexer = Lexer::new("'a'");

    assert_tokens!(lexer, [TokenKind::Literal(Literal::Char("a".into()))]);
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
            TokenKind::Identifier("_test".into())
        ]
    );
}

// ── Tagged string tests ────────────────────────────────────────────────────────

#[test]
fn test_tagged_string_simple() {
    let mut lexer = Lexer::new(r#"re"\d+""#);
    let token = lexer.next_token();
    assert_eq!(
        token.kind,
        TokenKind::Literal(Literal::TaggedString(
            "re".into(),
            vec![TaggedStringPart::Literal("\\d+".into())]
        ))
    );
}

#[test]
fn test_tagged_string_no_interpolation() {
    let mut lexer = Lexer::new(r#"html"<div>hello</div>""#);
    let token = lexer.next_token();
    assert_eq!(
        token.kind,
        TokenKind::Literal(Literal::TaggedString(
            "html".into(),
            vec![TaggedStringPart::Literal("<div>hello</div>".into())]
        ))
    );
}

#[test]
fn test_tagged_string_with_interpolation() {
    let mut lexer = Lexer::new(r#"html"<div>{name}</div>""#);
    let token = lexer.next_token();
    assert_eq!(
        token.kind,
        TokenKind::Literal(Literal::TaggedString(
            "html".into(),
            vec![
                TaggedStringPart::Literal("<div>".into()),
                TaggedStringPart::Interpolation {
                    source: "name".into(),
                    line: 0,
                    column: 0,
                    byte_offset: 0
                },
                TaggedStringPart::Literal("</div>".into()),
            ]
        ))
    );
}

#[test]
fn test_tagged_string_multiple_interpolations() {
    let mut lexer = Lexer::new(r#"sql"SELECT {cols} FROM {table}""#);
    let token = lexer.next_token();
    assert_eq!(
        token.kind,
        TokenKind::Literal(Literal::TaggedString(
            "sql".into(),
            vec![
                TaggedStringPart::Literal("SELECT ".into()),
                TaggedStringPart::Interpolation {
                    source: "cols".into(),
                    line: 0,
                    column: 0,
                    byte_offset: 0
                },
                TaggedStringPart::Literal(" FROM ".into()),
                TaggedStringPart::Interpolation {
                    source: "table".into(),
                    line: 0,
                    column: 0,
                    byte_offset: 0
                },
                TaggedStringPart::Literal("".into()),
            ]
        ))
    );
}

#[test]
fn test_tagged_string_regex_quantifier_not_interpolation() {
    // {2,5} should NOT trigger interpolation (digit after {)
    let mut lexer = Lexer::new(r#"re"\d{2,5}""#);
    let token = lexer.next_token();
    assert_eq!(
        token.kind,
        TokenKind::Literal(Literal::TaggedString(
            "re".into(),
            vec![TaggedStringPart::Literal("\\d{2,5}".into())]
        ))
    );
}

#[test]
fn test_tagged_string_double_brace_escape() {
    let mut lexer = Lexer::new(r#"html"{{escaped}}""#);
    let token = lexer.next_token();
    assert_eq!(
        token.kind,
        TokenKind::Literal(Literal::TaggedString(
            "html".into(),
            vec![TaggedStringPart::Literal("{escaped}".into())]
        ))
    );
}

#[test]
fn test_tagged_string_backslash_brace_escape() {
    let mut lexer = Lexer::new(r#"html"\{escaped\}""#);
    let token = lexer.next_token();
    assert_eq!(
        token.kind,
        TokenKind::Literal(Literal::TaggedString(
            "html".into(),
            vec![TaggedStringPart::Literal("{escaped}".into())]
        ))
    );
}

#[test]
fn test_tagged_string_nested_braces_in_interpolation() {
    // Expression with nested braces: { if true { "a" } else { "b" } }
    let mut lexer = Lexer::new(r#"tag"{if true { "a" } else { "b" }}""#);
    let token = lexer.next_token();
    assert_eq!(
        token.kind,
        TokenKind::Literal(Literal::TaggedString(
            "tag".into(),
            vec![
                TaggedStringPart::Literal("".into()),
                TaggedStringPart::Interpolation {
                    source: "if true { \"a\" } else { \"b\" }".into(),
                    line: 0,
                    column: 0,
                    byte_offset: 0
                },
                TaggedStringPart::Literal("".into()),
            ]
        ))
    );
}

#[test]
fn test_tagged_string_nested_tagged_string_in_interpolation() {
    // html"<div>{html"<span/>"}</div>"
    let mut lexer = Lexer::new(r#"html"<div>{html"<span/>"}</div>""#);
    let token = lexer.next_token();
    assert_eq!(
        token.kind,
        TokenKind::Literal(Literal::TaggedString(
            "html".into(),
            vec![
                TaggedStringPart::Literal("<div>".into()),
                TaggedStringPart::Interpolation {
                    source: "html\"<span/>\"".into(),
                    line: 0,
                    column: 0,
                    byte_offset: 0
                },
                TaggedStringPart::Literal("</div>".into()),
            ]
        ))
    );
}

#[test]
fn test_tagged_string_interpolation_with_member_access() {
    let mut lexer = Lexer::new(r#"html"{user.name}""#);
    let token = lexer.next_token();
    assert_eq!(
        token.kind,
        TokenKind::Literal(Literal::TaggedString(
            "html".into(),
            vec![
                TaggedStringPart::Literal("".into()),
                TaggedStringPart::Interpolation {
                    source: "user.name".into(),
                    line: 0,
                    column: 0,
                    byte_offset: 0
                },
                TaggedStringPart::Literal("".into()),
            ]
        ))
    );
}

#[test]
fn test_tagged_string_interpolation_position_tracking() {
    // Verify that interpolation byte offsets and line/column are tracked correctly.
    // source: html"<div>{name}</div>"
    //         0         1
    //         0123456789012345678901234
    // `{` is at byte 10, body `name` starts at byte 11, column 11 (0-indexed), line 0.
    let mut lexer = Lexer::new(r#"html"<div>{name}</div>""#);
    let token = lexer.next_token();
    if let TokenKind::Literal(Literal::TaggedString(_, parts)) = token.kind {
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
    } else {
        panic!("Expected TaggedString literal");
    }
}
