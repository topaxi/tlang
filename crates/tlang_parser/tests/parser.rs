use pretty_assertions::assert_eq;

use tlang_ast::{node, span::LineColumn, token::Literal};

mod common;

#[test]
fn test_unary_minus() {
    assert_parses!("-1;");
}

#[test]
fn test_simple_arithmetic_calculations() {
    assert_parser_snapshot!("1 + 2 + 3;");
    assert_parser_snapshot!("1 * 2 + 3;");
}

#[test]
fn test_simple_arithmetic_sum_mult_precedence() {
    assert_parser_snapshot!("1 + 2 * 3;");
}

#[test]
fn test_simple_arithmetic_sum_mult_precedence_parentheses() {
    assert_parser_snapshot!("(1 + 2) * 3;");
}

#[test]
fn test_simple_arithmetic_with_identifiers() {
    assert_parser_snapshot!("let x = 1; let y = 2; x + y;");
}

#[test]
fn test_simple_call() {
    assert_parser_snapshot!("foo(1, 2);");
}

#[test]
fn test_call_trailing_comma() {
    assert_parses!("foo(1, 2,);");
}

#[test]
#[should_panic]
fn test_call_list_mandatory_comma() {
    assert_parses!("foo(1 2);");
}

#[test]
fn test_nested_call() {
    assert_parser_snapshot!("foo(bar(1), 2);");
}

#[test]
fn test_call_with_expression() {
    assert_parser_snapshot!("foo(1 + 2, 3);");
}

#[test]
fn test_block_expression() {
    assert_parser_snapshot!("let x = { 1 + 2; };");
}

#[test]
fn test_if_statement() {
    assert_parser_snapshot!("if (1 + 2) { 3 + 4; }");
}

#[test]
fn test_if_else_statement() {
    assert_parser_snapshot!("if true { 1; } else { 2; }");
    assert_parser_snapshot!("if (1 + 2) { 3 + 4; } else { 5 + 6; }");
}

#[test]
fn test_if_else_if_statement() {
    assert_parser_snapshot!("if true { 1; } else if false { 2; }");
}

#[test]
fn test_if_else_as_last_expression() {
    assert_parser_snapshot!("fn main() { if true { 1 } else { 2 } }");
}

#[test]
fn test_if_expression() {
    assert_parser_snapshot!("let x = if (true) { 1; } else { 2; };");
}

#[test]
fn test_stray_semicolon() {
    assert_parses!("1 + 2;;");
}

#[test]
fn test_exponentiation() {
    assert_parser_snapshot!("1 * 2 ** 3;");
    assert_parser_snapshot!("1 ** 2 * 3;");
    assert_parser_snapshot!("1 ** 2 ** 3;");
}

#[test]
fn test_pattern_matching() {
    assert_parser_snapshot!("let x = match 1 { 1 => 2, 3 => 4, _ => 5 };");
}

#[test]
fn test_list_literal() {
    assert_parser_snapshot!("let x = [];");
    assert_parser_snapshot!("let x = [1, 2, 3];");
}

#[test]
fn test_list_literal_with_trailing_comma() {
    assert_parser_snapshot!("let x = [1, 2, 3,];");
}

#[test]
fn test_single_line_comments() {
    assert_parser_snapshot!("// this is a comment");
}

#[test]
fn test_multi_line_comments() {
    assert_parser_snapshot!("/* this is a comment */");
    assert_parser_snapshot!("/* this is a comment\n   spanning multiple lines */");
}

#[test]
fn test_string_literal() {
    let program = parse!(r#""foo";"#);

    assert_eq!(
        program,
        node::new!(Module(vec![node::new!(
            ExpressionStatement(Box::new(node::new!(
                Literal(Literal::String("foo".to_string())),
                Span::new(
                    LineColumn { line: 0, column: 0 },
                    LineColumn { line: 0, column: 5 }
                )
            ))),
            Span::new(
                LineColumn { line: 0, column: 0 },
                LineColumn { line: 0, column: 6 }
            )
        )]))
    );
}

#[test]
fn test_char_literal() {
    let program = parse!(r#"'a';"#);

    assert_eq!(
        program,
        node::new!(Module(vec![node::new!(
            ExpressionStatement(Box::new(node::new!(
                Literal(Literal::Char("a".to_string())),
                Span::new(
                    LineColumn { line: 0, column: 0 },
                    LineColumn { line: 0, column: 3 }
                )
            ))),
            Span::new(
                LineColumn { line: 0, column: 0 },
                LineColumn { line: 0, column: 4 }
            )
        )]))
    );
}

#[test]
fn test_dictionary_literal() {
    assert_parser_snapshot!(r#"let x = { foo: 1, bar: 2 };"#);
}

#[test]
fn test_function_call_with_dictionary_no_parens() {
    assert_parser_snapshot!(r#"foo { foo: 1, bar: 2 };"#);
}

#[test]
fn test_call_return_value() {
    assert_parser_snapshot!("foo()();");
}

#[test]
fn test_field_access_expressions() {
    assert_parser_snapshot!("foo.bar;");
    assert_parser_snapshot!("foo.bar.baz;");
}

#[test]
fn test_dynamic_index_access() {
    assert_parser_snapshot!("let x = foo[1];");
    assert_parser_snapshot!("let x = foo[1][2];");
}
