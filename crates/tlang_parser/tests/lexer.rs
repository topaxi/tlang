use tlang_ast::token::{Keyword, Literal, TokenKind};

use tlang_parser::lexer::Lexer;

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

    assert_tokens!(lexer, [TokenKind::Identifier("test".to_string())]);
}

#[test]
fn test_integer() {
    let mut lexer = Lexer::new("123");

    assert_tokens!(lexer, [TokenKind::Literal(Literal::Integer(123))]);
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
            TokenKind::Literal(Literal::Integer(123)),
            TokenKind::Identifier("i64".to_string())
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
            TokenKind::Identifier("f64".to_string())
        ]
    );
}

#[test]
fn test_single_char_operators() {
    let mut lexer = Lexer::new(". + - * / % ( ) | & ^");

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
        ]
    );
}

#[test]
fn test_multi_char_operators() {
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
    let mut lexer = Lexer::new("..");

    assert_tokens!(lexer, [TokenKind::DotDot]);

    let mut lexer = Lexer::new("...");

    assert_tokens!(lexer, [TokenKind::DotDotDot]);
}

#[test]
fn test_namespace_separator() {
    let mut lexer = Lexer::new("::");

    assert_tokens!(lexer, [TokenKind::NamespaceSeparator]);
}

#[test]
fn test_keywords() {
    let mut lexer = Lexer::new("let fn rec return if else match enum struct and or");

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
            TokenKind::Identifier("x".to_string()),
            TokenKind::EqualSign,
            TokenKind::Literal(Literal::Integer(1)),
            TokenKind::Plus,
            TokenKind::Literal(Literal::Integer(2)),
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
            TokenKind::Identifier("add".to_string()),
            TokenKind::LParen,
            TokenKind::Identifier("a".to_string()),
            TokenKind::Colon,
            TokenKind::Identifier("i64".to_string()),
            TokenKind::Comma,
            TokenKind::Identifier("b".to_string()),
            TokenKind::Colon,
            TokenKind::Identifier("i64".to_string()),
            TokenKind::RParen,
            TokenKind::Arrow,
            TokenKind::Identifier("i64".to_string()),
            TokenKind::LBrace,
            TokenKind::Keyword(Keyword::Return),
            TokenKind::Identifier("a".to_string()),
            TokenKind::Plus,
            TokenKind::Identifier("b".to_string()),
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
            TokenKind::Identifier("sum".to_string()),
            TokenKind::LParen,
            TokenKind::LBracket,
            TokenKind::RBracket,
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::Literal(Literal::Integer(0)),
            TokenKind::RBrace,
            TokenKind::Keyword(Keyword::Fn),
            TokenKind::Identifier("sum".to_string()),
            TokenKind::LParen,
            TokenKind::LBracket,
            TokenKind::Identifier("x".to_string()),
            TokenKind::Comma,
            TokenKind::DotDotDot,
            TokenKind::Identifier("xs".to_string()),
            TokenKind::RBracket,
            TokenKind::RParen,
            TokenKind::LBrace,
            TokenKind::Identifier("x".to_string()),
            TokenKind::Plus,
            TokenKind::Identifier("sum".to_string()),
            TokenKind::LParen,
            TokenKind::Identifier("xs".to_string()),
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
        [TokenKind::SingleLineComment(
            " this is a comment".to_string()
        )]
    );
}

#[test]
fn test_multi_line_comments() {
    let mut lexer = Lexer::new("/* this is a comment */");

    assert_tokens!(
        lexer,
        [TokenKind::MultiLineComment(
            " this is a comment ".to_string()
        )]
    );

    let mut lexer = Lexer::new("/* this is a comment\non multiple lines */");

    assert_tokens!(
        lexer,
        [TokenKind::MultiLineComment(
            " this is a comment\non multiple lines ".to_string()
        )]
    );
}

#[test]
fn test_string_literal() {
    let mut lexer = Lexer::new("\"this is a string\"");

    assert_tokens!(
        lexer,
        [TokenKind::Literal(Literal::String(
            "this is a string".to_string()
        ))]
    );
}

#[test]
fn test_char_literal() {
    let mut lexer = Lexer::new("'a'");

    assert_tokens!(lexer, [TokenKind::Literal(Literal::Char("a".to_string()))]);
}

#[test]
fn test_token_span() {
    let mut lexer = Lexer::new("let x = 1 + 2;");

    let token = lexer.next_token();
    assert_eq!(token.span.start.line, 0);
    assert_eq!(token.span.start.column, 0);
    assert_eq!(token.span.end.line, 0);
    assert_eq!(token.span.end.column, 3);

    let token = lexer.next_token();
    assert_eq!(token.span.start.line, 0);
    assert_eq!(token.span.start.column, 4);
    assert_eq!(token.span.end.line, 0);
    assert_eq!(token.span.end.column, 5);
}

#[test]
fn test_underscore_keyword() {
    let mut lexer = Lexer::new("_ _test");

    assert_tokens!(
        lexer,
        [
            TokenKind::Keyword(Keyword::Underscore),
            TokenKind::Identifier("_test".to_string())
        ]
    );
}
