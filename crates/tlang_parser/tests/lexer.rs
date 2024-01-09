use tlang_ast::token::{Literal, TokenKind};

use tlang_parser::lexer::Lexer;

#[test]
fn test_identifier() {
    let mut lexer = Lexer::new("test");

    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("test".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_integer() {
    let mut lexer = Lexer::new("123");

    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Literal(Literal::Integer(123))
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_float() {
    let mut lexer = Lexer::new("123.456");

    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Literal(Literal::Float(123.456))
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_typed_integer() {
    let mut lexer = Lexer::new("123i64");

    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Literal(Literal::Integer(123))
    );
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("i64".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_typed_float() {
    let mut lexer = Lexer::new("123.456f64");

    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Literal(Literal::Float(123.456))
    );
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("f64".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_single_char_operators() {
    let mut lexer = Lexer::new(". + - * / % ( ) | & ^");

    assert_eq!(lexer.next_token().kind, TokenKind::Dot);
    assert_eq!(lexer.next_token().kind, TokenKind::Plus);
    assert_eq!(lexer.next_token().kind, TokenKind::Minus);
    assert_eq!(lexer.next_token().kind, TokenKind::Asterisk);
    assert_eq!(lexer.next_token().kind, TokenKind::Slash);
    assert_eq!(lexer.next_token().kind, TokenKind::Percent);
    assert_eq!(lexer.next_token().kind, TokenKind::LParen);
    assert_eq!(lexer.next_token().kind, TokenKind::RParen);
    assert_eq!(lexer.next_token().kind, TokenKind::Pipe);
    assert_eq!(lexer.next_token().kind, TokenKind::Ampersand);
    assert_eq!(lexer.next_token().kind, TokenKind::Caret);
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_multi_char_operators() {
    let mut lexer = Lexer::new("|| && == != >= <= ** |>");

    assert_eq!(lexer.next_token().kind, TokenKind::DoublePipe);
    assert_eq!(lexer.next_token().kind, TokenKind::DoubleAmpersand);
    assert_eq!(lexer.next_token().kind, TokenKind::EqualEqual);
    assert_eq!(lexer.next_token().kind, TokenKind::NotEqual);
    assert_eq!(lexer.next_token().kind, TokenKind::GreaterThanOrEqual);
    assert_eq!(lexer.next_token().kind, TokenKind::LessThanOrEqual);
    assert_eq!(lexer.next_token().kind, TokenKind::AsteriskAsterisk);
    assert_eq!(lexer.next_token().kind, TokenKind::Pipeline);
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_arrows() {
    let mut lexer = Lexer::new("-> =>");

    assert_eq!(lexer.next_token().kind, TokenKind::Arrow);
    assert_eq!(lexer.next_token().kind, TokenKind::FatArrow);
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_dotdot_dot() {
    let mut lexer = Lexer::new("..");

    assert_eq!(lexer.next_token().kind, TokenKind::DotDot);
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);

    let mut lexer = Lexer::new("...");

    assert_eq!(lexer.next_token().kind, TokenKind::DotDotDot);
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_namespace_separator() {
    let mut lexer = Lexer::new("::");

    assert_eq!(lexer.next_token().kind, TokenKind::NamespaceSeparator);
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_keywords() {
    let mut lexer = Lexer::new("let fn rec return if else match enum struct");

    assert_eq!(lexer.next_token().kind, TokenKind::Let);
    assert_eq!(lexer.next_token().kind, TokenKind::Fn);
    assert_eq!(lexer.next_token().kind, TokenKind::Rec);
    assert_eq!(lexer.next_token().kind, TokenKind::Return);
    assert_eq!(lexer.next_token().kind, TokenKind::If);
    assert_eq!(lexer.next_token().kind, TokenKind::Else);
    assert_eq!(lexer.next_token().kind, TokenKind::Match);
    assert_eq!(lexer.next_token().kind, TokenKind::Enum);
    assert_eq!(lexer.next_token().kind, TokenKind::Struct);
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_equal_sign() {
    let mut lexer = Lexer::new("=");

    assert_eq!(lexer.next_token().kind, TokenKind::EqualSign);
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_comparison_operators() {
    let mut lexer = Lexer::new("== != >= <= > <");

    assert_eq!(lexer.next_token().kind, TokenKind::EqualEqual);
    assert_eq!(lexer.next_token().kind, TokenKind::NotEqual);
    assert_eq!(lexer.next_token().kind, TokenKind::GreaterThanOrEqual);
    assert_eq!(lexer.next_token().kind, TokenKind::LessThanOrEqual);
    assert_eq!(lexer.next_token().kind, TokenKind::GreaterThan);
    assert_eq!(lexer.next_token().kind, TokenKind::LessThan);
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_simple_assignment() {
    let mut lexer = Lexer::new("let x = 1 + 2;");

    assert_eq!(lexer.next_token().kind, TokenKind::Let);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("x".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::EqualSign);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Literal(Literal::Integer(1))
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Plus);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Literal(Literal::Integer(2))
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_function_declaration_with_explicit_return_statement() {
    let mut lexer = Lexer::new("fn add(a: i64, b: i64) -> i64 { return a + b; }");

    assert_eq!(lexer.next_token().kind, TokenKind::Fn);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("add".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::LParen);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("a".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Colon);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("i64".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Comma);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("b".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Colon);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("i64".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::RParen);
    assert_eq!(lexer.next_token().kind, TokenKind::Arrow);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("i64".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::LBrace);
    assert_eq!(lexer.next_token().kind, TokenKind::Return);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("a".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Plus);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("b".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Semicolon);
    assert_eq!(lexer.next_token().kind, TokenKind::RBrace);
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_regression_integer_literal_followed_by_dot() {
    let mut lexer = Lexer::new("fn sum([]) { 0 }\nfn sum([x, ...xs]) { x + sum(xs) }");

    assert_eq!(lexer.next_token().kind, TokenKind::Fn);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("sum".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::LParen);
    assert_eq!(lexer.next_token().kind, TokenKind::LBracket);
    assert_eq!(lexer.next_token().kind, TokenKind::RBracket);
    assert_eq!(lexer.next_token().kind, TokenKind::RParen);
    assert_eq!(lexer.next_token().kind, TokenKind::LBrace);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Literal(Literal::Integer(0))
    );
    assert_eq!(lexer.next_token().kind, TokenKind::RBrace);
    assert_eq!(lexer.next_token().kind, TokenKind::Fn);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("sum".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::LParen);
    assert_eq!(lexer.next_token().kind, TokenKind::LBracket);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("x".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Comma);
    assert_eq!(lexer.next_token().kind, TokenKind::DotDotDot);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("xs".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::RBracket);
    assert_eq!(lexer.next_token().kind, TokenKind::RParen);
    assert_eq!(lexer.next_token().kind, TokenKind::LBrace);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("x".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Plus);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("sum".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::LParen);
    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Identifier("xs".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::RParen);
    assert_eq!(lexer.next_token().kind, TokenKind::RBrace);
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_single_line_comments() {
    let mut lexer = Lexer::new("// this is a comment");

    assert_eq!(
        lexer.next_token().kind,
        TokenKind::SingleLineComment(" this is a comment".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_multi_line_comments() {
    let mut lexer = Lexer::new("/* this is a comment */");

    assert_eq!(
        lexer.next_token().kind,
        TokenKind::MultiLineComment(" this is a comment ".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);

    let mut lexer = Lexer::new("/* this is a comment\non multiple lines */");

    assert_eq!(
        lexer.next_token().kind,
        TokenKind::MultiLineComment(" this is a comment\non multiple lines ".to_string())
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_string_literal() {
    let mut lexer = Lexer::new("\"this is a string\"");

    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Literal(Literal::String("this is a string".to_string()))
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_char_literal() {
    let mut lexer = Lexer::new("'a'");

    assert_eq!(
        lexer.next_token().kind,
        TokenKind::Literal(Literal::Char("a".to_string()))
    );
    assert_eq!(lexer.next_token().kind, TokenKind::Eof);
}

#[test]
fn test_token_span() {
    let mut lexer = Lexer::new("let x = 1 + 2;");
    let token = lexer.next_token();

    assert_eq!(token.span.start.line, 1);
    assert_eq!(token.span.start.column, 1);
    assert_eq!(token.span.end.line, 1);
    assert_eq!(token.span.end.column, 4);
}
