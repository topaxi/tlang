use tlang_ast::token::{Literal, Token};

use crate::lexer::Lexer;

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
    let mut lexer = Lexer::new("|| && == != >= <= ** |>");

    assert_eq!(lexer.next_token(), Token::DoublePipe);
    assert_eq!(lexer.next_token(), Token::DoubleAmpersand);
    assert_eq!(lexer.next_token(), Token::EqualEqual);
    assert_eq!(lexer.next_token(), Token::NotEqual);
    assert_eq!(lexer.next_token(), Token::GreaterThanOrEqual);
    assert_eq!(lexer.next_token(), Token::LessThanOrEqual);
    assert_eq!(lexer.next_token(), Token::AsteriskAsterisk);
    assert_eq!(lexer.next_token(), Token::Pipeline);
    assert_eq!(lexer.next_token(), Token::Eof);
}

#[test]
fn test_arrows() {
    let mut lexer = Lexer::new("-> =>");

    assert_eq!(lexer.next_token(), Token::Arrow);
    assert_eq!(lexer.next_token(), Token::FatArrow);
    assert_eq!(lexer.next_token(), Token::Eof);
}

#[test]
fn test_dotdot_dot() {
    let mut lexer = Lexer::new("..");

    assert_eq!(lexer.next_token(), Token::DotDot);
    assert_eq!(lexer.next_token(), Token::Eof);

    let mut lexer = Lexer::new("...");

    assert_eq!(lexer.next_token(), Token::DotDotDot);
    assert_eq!(lexer.next_token(), Token::Eof);
}

#[test]
fn test_namespace_separator() {
    let mut lexer = Lexer::new("::");

    assert_eq!(lexer.next_token(), Token::NamespaceSeparator);
    assert_eq!(lexer.next_token(), Token::Eof);
}

#[test]
fn test_keywords() {
    let mut lexer = Lexer::new("let fn rec return if else match enum struct");

    assert_eq!(lexer.next_token(), Token::Let);
    assert_eq!(lexer.next_token(), Token::Fn);
    assert_eq!(lexer.next_token(), Token::Rec);
    assert_eq!(lexer.next_token(), Token::Return);
    assert_eq!(lexer.next_token(), Token::If);
    assert_eq!(lexer.next_token(), Token::Else);
    assert_eq!(lexer.next_token(), Token::Match);
    assert_eq!(lexer.next_token(), Token::Enum);
    assert_eq!(lexer.next_token(), Token::Struct);
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

#[test]
fn test_function_declaration_with_explicit_return_statement() {
    let mut lexer = Lexer::new("fn add(a: i64, b: i64) -> i64 { return a + b; }");

    assert_eq!(lexer.next_token(), Token::Fn);
    assert_eq!(lexer.next_token(), Token::Identifier("add".to_string()));
    assert_eq!(lexer.next_token(), Token::LParen);
    assert_eq!(lexer.next_token(), Token::Identifier("a".to_string()));
    assert_eq!(lexer.next_token(), Token::Colon);
    assert_eq!(lexer.next_token(), Token::Identifier("i64".to_string()));
    assert_eq!(lexer.next_token(), Token::Comma);
    assert_eq!(lexer.next_token(), Token::Identifier("b".to_string()));
    assert_eq!(lexer.next_token(), Token::Colon);
    assert_eq!(lexer.next_token(), Token::Identifier("i64".to_string()));
    assert_eq!(lexer.next_token(), Token::RParen);
    assert_eq!(lexer.next_token(), Token::Arrow);
    assert_eq!(lexer.next_token(), Token::Identifier("i64".to_string()));
    assert_eq!(lexer.next_token(), Token::LBrace);
    assert_eq!(lexer.next_token(), Token::Return);
    assert_eq!(lexer.next_token(), Token::Identifier("a".to_string()));
    assert_eq!(lexer.next_token(), Token::Plus);
    assert_eq!(lexer.next_token(), Token::Identifier("b".to_string()));
    assert_eq!(lexer.next_token(), Token::Semicolon);
    assert_eq!(lexer.next_token(), Token::RBrace);
    assert_eq!(lexer.next_token(), Token::Eof);
}

#[test]
fn test_regression_integer_literal_followed_by_dot() {
    let mut lexer = Lexer::new("fn sum([]) { 0 }\nfn sum([x, ...xs]) { x + sum(xs) }");

    assert_eq!(lexer.next_token(), Token::Fn);
    assert_eq!(lexer.next_token(), Token::Identifier("sum".to_string()));
    assert_eq!(lexer.next_token(), Token::LParen);
    assert_eq!(lexer.next_token(), Token::LBracket);
    assert_eq!(lexer.next_token(), Token::RBracket);
    assert_eq!(lexer.next_token(), Token::RParen);
    assert_eq!(lexer.next_token(), Token::LBrace);
    assert_eq!(lexer.next_token(), Token::Literal(Literal::Integer(0)));
    assert_eq!(lexer.next_token(), Token::RBrace);
    assert_eq!(lexer.next_token(), Token::Fn);
    assert_eq!(lexer.next_token(), Token::Identifier("sum".to_string()));
    assert_eq!(lexer.next_token(), Token::LParen);
    assert_eq!(lexer.next_token(), Token::LBracket);
    assert_eq!(lexer.next_token(), Token::Identifier("x".to_string()));
    assert_eq!(lexer.next_token(), Token::Comma);
    assert_eq!(lexer.next_token(), Token::DotDotDot);
    assert_eq!(lexer.next_token(), Token::Identifier("xs".to_string()));
    assert_eq!(lexer.next_token(), Token::RBracket);
    assert_eq!(lexer.next_token(), Token::RParen);
    assert_eq!(lexer.next_token(), Token::LBrace);
    assert_eq!(lexer.next_token(), Token::Identifier("x".to_string()));
    assert_eq!(lexer.next_token(), Token::Plus);
    assert_eq!(lexer.next_token(), Token::Identifier("sum".to_string()));
    assert_eq!(lexer.next_token(), Token::LParen);
    assert_eq!(lexer.next_token(), Token::Identifier("xs".to_string()));
    assert_eq!(lexer.next_token(), Token::RParen);
    assert_eq!(lexer.next_token(), Token::RBrace);
    assert_eq!(lexer.next_token(), Token::Eof);
}

#[test]
fn test_single_line_comments() {
    let mut lexer = Lexer::new("// this is a comment");

    assert_eq!(
        lexer.next_token(),
        Token::SingleLineComment(" this is a comment".to_string())
    );
    assert_eq!(lexer.next_token(), Token::Eof);
}

#[test]
fn test_multi_line_comments() {
    let mut lexer = Lexer::new("/* this is a comment */");

    assert_eq!(
        lexer.next_token(),
        Token::MultiLineComment(" this is a comment ".to_string())
    );
    assert_eq!(lexer.next_token(), Token::Eof);

    let mut lexer = Lexer::new("/* this is a comment\non multiple lines */");

    assert_eq!(
        lexer.next_token(),
        Token::MultiLineComment(" this is a comment\non multiple lines ".to_string())
    );
    assert_eq!(lexer.next_token(), Token::Eof);
}

#[test]
fn test_string_literal() {
    let mut lexer = Lexer::new("\"this is a string\"");

    assert_eq!(
        lexer.next_token(),
        Token::Literal(Literal::String("this is a string".to_string()))
    );
    assert_eq!(lexer.next_token(), Token::Eof);
}

#[test]
fn test_char_literal() {
    let mut lexer = Lexer::new("'a'");

    assert_eq!(
        lexer.next_token(),
        Token::Literal(Literal::Char("a".to_string()))
    );
    assert_eq!(lexer.next_token(), Token::Eof);
}
