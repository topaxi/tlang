use tlang_ast::token::{Literal, TokenValue};

use crate::lexer::Lexer;

#[test]
fn test_identifier() {
    let mut lexer = Lexer::new("test");

    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("test".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_integer() {
    let mut lexer = Lexer::new("123");

    assert_eq!(
        lexer.next_token().value,
        TokenValue::Literal(Literal::Integer(123))
    );
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_float() {
    let mut lexer = Lexer::new("123.456");

    assert_eq!(
        lexer.next_token().value,
        TokenValue::Literal(Literal::Float(123.456))
    );
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_typed_integer() {
    let mut lexer = Lexer::new("123i64");

    assert_eq!(
        lexer.next_token().value,
        TokenValue::Literal(Literal::Integer(123))
    );
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("i64".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_typed_float() {
    let mut lexer = Lexer::new("123.456f64");

    assert_eq!(
        lexer.next_token().value,
        TokenValue::Literal(Literal::Float(123.456))
    );
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("f64".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_single_char_operators() {
    let mut lexer = Lexer::new(". + - * / % ( ) | & ^");

    assert_eq!(lexer.next_token().value, TokenValue::Dot);
    assert_eq!(lexer.next_token().value, TokenValue::Plus);
    assert_eq!(lexer.next_token().value, TokenValue::Minus);
    assert_eq!(lexer.next_token().value, TokenValue::Asterisk);
    assert_eq!(lexer.next_token().value, TokenValue::Slash);
    assert_eq!(lexer.next_token().value, TokenValue::Percent);
    assert_eq!(lexer.next_token().value, TokenValue::LParen);
    assert_eq!(lexer.next_token().value, TokenValue::RParen);
    assert_eq!(lexer.next_token().value, TokenValue::Pipe);
    assert_eq!(lexer.next_token().value, TokenValue::Ampersand);
    assert_eq!(lexer.next_token().value, TokenValue::Caret);
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_multi_char_operators() {
    let mut lexer = Lexer::new("|| && == != >= <= ** |>");

    assert_eq!(lexer.next_token().value, TokenValue::DoublePipe);
    assert_eq!(lexer.next_token().value, TokenValue::DoubleAmpersand);
    assert_eq!(lexer.next_token().value, TokenValue::EqualEqual);
    assert_eq!(lexer.next_token().value, TokenValue::NotEqual);
    assert_eq!(lexer.next_token().value, TokenValue::GreaterThanOrEqual);
    assert_eq!(lexer.next_token().value, TokenValue::LessThanOrEqual);
    assert_eq!(lexer.next_token().value, TokenValue::AsteriskAsterisk);
    assert_eq!(lexer.next_token().value, TokenValue::Pipeline);
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_arrows() {
    let mut lexer = Lexer::new("-> =>");

    assert_eq!(lexer.next_token().value, TokenValue::Arrow);
    assert_eq!(lexer.next_token().value, TokenValue::FatArrow);
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_dotdot_dot() {
    let mut lexer = Lexer::new("..");

    assert_eq!(lexer.next_token().value, TokenValue::DotDot);
    assert_eq!(lexer.next_token().value, TokenValue::Eof);

    let mut lexer = Lexer::new("...");

    assert_eq!(lexer.next_token().value, TokenValue::DotDotDot);
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_namespace_separator() {
    let mut lexer = Lexer::new("::");

    assert_eq!(lexer.next_token().value, TokenValue::NamespaceSeparator);
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_keywords() {
    let mut lexer = Lexer::new("let fn rec return if else match enum struct");

    assert_eq!(lexer.next_token().value, TokenValue::Let);
    assert_eq!(lexer.next_token().value, TokenValue::Fn);
    assert_eq!(lexer.next_token().value, TokenValue::Rec);
    assert_eq!(lexer.next_token().value, TokenValue::Return);
    assert_eq!(lexer.next_token().value, TokenValue::If);
    assert_eq!(lexer.next_token().value, TokenValue::Else);
    assert_eq!(lexer.next_token().value, TokenValue::Match);
    assert_eq!(lexer.next_token().value, TokenValue::Enum);
    assert_eq!(lexer.next_token().value, TokenValue::Struct);
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_equal_sign() {
    let mut lexer = Lexer::new("=");

    assert_eq!(lexer.next_token().value, TokenValue::EqualSign);
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_comparison_operators() {
    let mut lexer = Lexer::new("== != >= <= > <");

    assert_eq!(lexer.next_token().value, TokenValue::EqualEqual);
    assert_eq!(lexer.next_token().value, TokenValue::NotEqual);
    assert_eq!(lexer.next_token().value, TokenValue::GreaterThanOrEqual);
    assert_eq!(lexer.next_token().value, TokenValue::LessThanOrEqual);
    assert_eq!(lexer.next_token().value, TokenValue::GreaterThan);
    assert_eq!(lexer.next_token().value, TokenValue::LessThan);
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_simple_assignment() {
    let mut lexer = Lexer::new("let x = 1 + 2;");

    assert_eq!(lexer.next_token().value, TokenValue::Let);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("x".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::EqualSign);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Literal(Literal::Integer(1))
    );
    assert_eq!(lexer.next_token().value, TokenValue::Plus);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Literal(Literal::Integer(2))
    );
    assert_eq!(lexer.next_token().value, TokenValue::Semicolon);
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_function_declaration_with_explicit_return_statement() {
    let mut lexer = Lexer::new("fn add(a: i64, b: i64) -> i64 { return a + b; }");

    assert_eq!(lexer.next_token().value, TokenValue::Fn);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("add".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::LParen);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("a".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::Colon);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("i64".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::Comma);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("b".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::Colon);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("i64".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::RParen);
    assert_eq!(lexer.next_token().value, TokenValue::Arrow);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("i64".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::LBrace);
    assert_eq!(lexer.next_token().value, TokenValue::Return);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("a".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::Plus);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("b".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::Semicolon);
    assert_eq!(lexer.next_token().value, TokenValue::RBrace);
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_regression_integer_literal_followed_by_dot() {
    let mut lexer = Lexer::new("fn sum([]) { 0 }\nfn sum([x, ...xs]) { x + sum(xs) }");

    assert_eq!(lexer.next_token().value, TokenValue::Fn);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("sum".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::LParen);
    assert_eq!(lexer.next_token().value, TokenValue::LBracket);
    assert_eq!(lexer.next_token().value, TokenValue::RBracket);
    assert_eq!(lexer.next_token().value, TokenValue::RParen);
    assert_eq!(lexer.next_token().value, TokenValue::LBrace);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Literal(Literal::Integer(0))
    );
    assert_eq!(lexer.next_token().value, TokenValue::RBrace);
    assert_eq!(lexer.next_token().value, TokenValue::Fn);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("sum".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::LParen);
    assert_eq!(lexer.next_token().value, TokenValue::LBracket);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("x".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::Comma);
    assert_eq!(lexer.next_token().value, TokenValue::DotDotDot);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("xs".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::RBracket);
    assert_eq!(lexer.next_token().value, TokenValue::RParen);
    assert_eq!(lexer.next_token().value, TokenValue::LBrace);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("x".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::Plus);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("sum".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::LParen);
    assert_eq!(
        lexer.next_token().value,
        TokenValue::Identifier("xs".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::RParen);
    assert_eq!(lexer.next_token().value, TokenValue::RBrace);
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_single_line_comments() {
    let mut lexer = Lexer::new("// this is a comment");

    assert_eq!(
        lexer.next_token().value,
        TokenValue::SingleLineComment(" this is a comment".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_multi_line_comments() {
    let mut lexer = Lexer::new("/* this is a comment */");

    assert_eq!(
        lexer.next_token().value,
        TokenValue::MultiLineComment(" this is a comment ".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::Eof);

    let mut lexer = Lexer::new("/* this is a comment\non multiple lines */");

    assert_eq!(
        lexer.next_token().value,
        TokenValue::MultiLineComment(" this is a comment\non multiple lines ".to_string())
    );
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_string_literal() {
    let mut lexer = Lexer::new("\"this is a string\"");

    assert_eq!(
        lexer.next_token().value,
        TokenValue::Literal(Literal::String("this is a string".to_string()))
    );
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}

#[test]
fn test_char_literal() {
    let mut lexer = Lexer::new("'a'");

    assert_eq!(
        lexer.next_token().value,
        TokenValue::Literal(Literal::Char("a".to_string()))
    );
    assert_eq!(lexer.next_token().value, TokenValue::Eof);
}
