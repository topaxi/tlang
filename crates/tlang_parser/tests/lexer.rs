use tlang_ast::token::{Literal, TokenKind};

use tlang_parser::lexer::Lexer;

#[test]
fn test_identifier() {
    let mut lexer = Lexer::new("test");

    assert_eq!(
        lexer.next_token(),
        TokenKind::Identifier("test".to_string())
    );
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_integer() {
    let mut lexer = Lexer::new("123");

    assert_eq!(
        lexer.next_token(),
        TokenKind::Literal(Literal::Integer(123))
    );
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_float() {
    let mut lexer = Lexer::new("123.456");

    assert_eq!(
        lexer.next_token(),
        TokenKind::Literal(Literal::Float(123.456))
    );
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_typed_integer() {
    let mut lexer = Lexer::new("123i64");

    assert_eq!(
        lexer.next_token(),
        TokenKind::Literal(Literal::Integer(123))
    );
    assert_eq!(lexer.next_token(), TokenKind::Identifier("i64".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_typed_float() {
    let mut lexer = Lexer::new("123.456f64");

    assert_eq!(
        lexer.next_token(),
        TokenKind::Literal(Literal::Float(123.456))
    );
    assert_eq!(lexer.next_token(), TokenKind::Identifier("f64".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_single_char_operators() {
    let mut lexer = Lexer::new(". + - * / % ( ) | & ^");

    assert_eq!(lexer.next_token(), TokenKind::Dot);
    assert_eq!(lexer.next_token(), TokenKind::Plus);
    assert_eq!(lexer.next_token(), TokenKind::Minus);
    assert_eq!(lexer.next_token(), TokenKind::Asterisk);
    assert_eq!(lexer.next_token(), TokenKind::Slash);
    assert_eq!(lexer.next_token(), TokenKind::Percent);
    assert_eq!(lexer.next_token(), TokenKind::LParen);
    assert_eq!(lexer.next_token(), TokenKind::RParen);
    assert_eq!(lexer.next_token(), TokenKind::Pipe);
    assert_eq!(lexer.next_token(), TokenKind::Ampersand);
    assert_eq!(lexer.next_token(), TokenKind::Caret);
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_multi_char_operators() {
    let mut lexer = Lexer::new("|| && == != >= <= ** |>");

    assert_eq!(lexer.next_token(), TokenKind::DoublePipe);
    assert_eq!(lexer.next_token(), TokenKind::DoubleAmpersand);
    assert_eq!(lexer.next_token(), TokenKind::EqualEqual);
    assert_eq!(lexer.next_token(), TokenKind::NotEqual);
    assert_eq!(lexer.next_token(), TokenKind::GreaterThanOrEqual);
    assert_eq!(lexer.next_token(), TokenKind::LessThanOrEqual);
    assert_eq!(lexer.next_token(), TokenKind::AsteriskAsterisk);
    assert_eq!(lexer.next_token(), TokenKind::Pipeline);
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_arrows() {
    let mut lexer = Lexer::new("-> =>");

    assert_eq!(lexer.next_token(), TokenKind::Arrow);
    assert_eq!(lexer.next_token(), TokenKind::FatArrow);
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_dotdot_dot() {
    let mut lexer = Lexer::new("..");

    assert_eq!(lexer.next_token(), TokenKind::DotDot);
    assert_eq!(lexer.next_token(), TokenKind::Eof);

    let mut lexer = Lexer::new("...");

    assert_eq!(lexer.next_token(), TokenKind::DotDotDot);
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_namespace_separator() {
    let mut lexer = Lexer::new("::");

    assert_eq!(lexer.next_token(), TokenKind::NamespaceSeparator);
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_keywords() {
    let mut lexer = Lexer::new("let fn rec return if else match enum struct");

    assert_eq!(lexer.next_token(), TokenKind::Let);
    assert_eq!(lexer.next_token(), TokenKind::Fn);
    assert_eq!(lexer.next_token(), TokenKind::Rec);
    assert_eq!(lexer.next_token(), TokenKind::Return);
    assert_eq!(lexer.next_token(), TokenKind::If);
    assert_eq!(lexer.next_token(), TokenKind::Else);
    assert_eq!(lexer.next_token(), TokenKind::Match);
    assert_eq!(lexer.next_token(), TokenKind::Enum);
    assert_eq!(lexer.next_token(), TokenKind::Struct);
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_equal_sign() {
    let mut lexer = Lexer::new("=");

    assert_eq!(lexer.next_token(), TokenKind::EqualSign);
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_comparison_operators() {
    let mut lexer = Lexer::new("== != >= <= > <");

    assert_eq!(lexer.next_token(), TokenKind::EqualEqual);
    assert_eq!(lexer.next_token(), TokenKind::NotEqual);
    assert_eq!(lexer.next_token(), TokenKind::GreaterThanOrEqual);
    assert_eq!(lexer.next_token(), TokenKind::LessThanOrEqual);
    assert_eq!(lexer.next_token(), TokenKind::GreaterThan);
    assert_eq!(lexer.next_token(), TokenKind::LessThan);
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_simple_assignment() {
    let mut lexer = Lexer::new("let x = 1 + 2;");

    assert_eq!(lexer.next_token(), TokenKind::Let);
    assert_eq!(lexer.next_token(), TokenKind::Identifier("x".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::EqualSign);
    assert_eq!(lexer.next_token(), TokenKind::Literal(Literal::Integer(1)));
    assert_eq!(lexer.next_token(), TokenKind::Plus);
    assert_eq!(lexer.next_token(), TokenKind::Literal(Literal::Integer(2)));
    assert_eq!(lexer.next_token(), TokenKind::Semicolon);
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_function_declaration_with_explicit_return_statement() {
    let mut lexer = Lexer::new("fn add(a: i64, b: i64) -> i64 { return a + b; }");

    assert_eq!(lexer.next_token(), TokenKind::Fn);
    assert_eq!(lexer.next_token(), TokenKind::Identifier("add".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::LParen);
    assert_eq!(lexer.next_token(), TokenKind::Identifier("a".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::Colon);
    assert_eq!(lexer.next_token(), TokenKind::Identifier("i64".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::Comma);
    assert_eq!(lexer.next_token(), TokenKind::Identifier("b".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::Colon);
    assert_eq!(lexer.next_token(), TokenKind::Identifier("i64".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::RParen);
    assert_eq!(lexer.next_token(), TokenKind::Arrow);
    assert_eq!(lexer.next_token(), TokenKind::Identifier("i64".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::LBrace);
    assert_eq!(lexer.next_token(), TokenKind::Return);
    assert_eq!(lexer.next_token(), TokenKind::Identifier("a".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::Plus);
    assert_eq!(lexer.next_token(), TokenKind::Identifier("b".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::Semicolon);
    assert_eq!(lexer.next_token(), TokenKind::RBrace);
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_regression_integer_literal_followed_by_dot() {
    let mut lexer = Lexer::new("fn sum([]) { 0 }\nfn sum([x, ...xs]) { x + sum(xs) }");

    assert_eq!(lexer.next_token(), TokenKind::Fn);
    assert_eq!(lexer.next_token(), TokenKind::Identifier("sum".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::LParen);
    assert_eq!(lexer.next_token(), TokenKind::LBracket);
    assert_eq!(lexer.next_token(), TokenKind::RBracket);
    assert_eq!(lexer.next_token(), TokenKind::RParen);
    assert_eq!(lexer.next_token(), TokenKind::LBrace);
    assert_eq!(lexer.next_token(), TokenKind::Literal(Literal::Integer(0)));
    assert_eq!(lexer.next_token(), TokenKind::RBrace);
    assert_eq!(lexer.next_token(), TokenKind::Fn);
    assert_eq!(lexer.next_token(), TokenKind::Identifier("sum".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::LParen);
    assert_eq!(lexer.next_token(), TokenKind::LBracket);
    assert_eq!(lexer.next_token(), TokenKind::Identifier("x".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::Comma);
    assert_eq!(lexer.next_token(), TokenKind::DotDotDot);
    assert_eq!(lexer.next_token(), TokenKind::Identifier("xs".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::RBracket);
    assert_eq!(lexer.next_token(), TokenKind::RParen);
    assert_eq!(lexer.next_token(), TokenKind::LBrace);
    assert_eq!(lexer.next_token(), TokenKind::Identifier("x".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::Plus);
    assert_eq!(lexer.next_token(), TokenKind::Identifier("sum".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::LParen);
    assert_eq!(lexer.next_token(), TokenKind::Identifier("xs".to_string()));
    assert_eq!(lexer.next_token(), TokenKind::RParen);
    assert_eq!(lexer.next_token(), TokenKind::RBrace);
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_single_line_comments() {
    let mut lexer = Lexer::new("// this is a comment");

    assert_eq!(
        lexer.next_token(),
        TokenKind::SingleLineComment(" this is a comment".to_string())
    );
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_multi_line_comments() {
    let mut lexer = Lexer::new("/* this is a comment */");

    assert_eq!(
        lexer.next_token(),
        TokenKind::MultiLineComment(" this is a comment ".to_string())
    );
    assert_eq!(lexer.next_token(), TokenKind::Eof);

    let mut lexer = Lexer::new("/* this is a comment\non multiple lines */");

    assert_eq!(
        lexer.next_token(),
        TokenKind::MultiLineComment(" this is a comment\non multiple lines ".to_string())
    );
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_string_literal() {
    let mut lexer = Lexer::new("\"this is a string\"");

    assert_eq!(
        lexer.next_token(),
        TokenKind::Literal(Literal::String("this is a string".to_string()))
    );
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}

#[test]
fn test_char_literal() {
    let mut lexer = Lexer::new("'a'");

    assert_eq!(
        lexer.next_token(),
        TokenKind::Literal(Literal::Char("a".to_string()))
    );
    assert_eq!(lexer.next_token(), TokenKind::Eof);
}
