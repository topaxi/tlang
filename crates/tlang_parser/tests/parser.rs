#![feature(box_patterns)]

use pretty_assertions::assert_matches;

mod common;

#[test]
fn test_unsigned_literal() {
    let ast = parse!("1;");

    assert_matches!(
        ast.statements[0].kind,
        tlang_ast::node::StmtKind::Expr(box tlang_ast::node::Expr {
            kind:
                tlang_ast::node::ExprKind::Literal(box tlang_ast::token::Literal::UnsignedInteger(1)),
            ..
        })
    );
}

#[test]
fn test_signed_literal() {
    let ast = parse!("-1;");

    assert_matches!(
        ast.statements[0].kind,
        tlang_ast::node::StmtKind::Expr(box tlang_ast::node::Expr {
            kind: tlang_ast::node::ExprKind::Literal(box tlang_ast::token::Literal::Integer(-1)),
            ..
        })
    );
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
fn test_simple_unary_not() {
    assert_parser_snapshot!("!true;");
    assert_parser_snapshot!("not true;");
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
fn test_loop_expression() {
    assert_parser_snapshot!("let x = loop { break 1 + 2; };");
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
    assert_parser_snapshot!(
        r"
        match foo; {
            [] => Ok(bar),
            [bar, ...baz] => Ok(qux),
            _ => Err(error),
        }
        "
    );
}

#[test]
fn test_pattern_matching_with_guard() {
    assert_parser_snapshot!("let x = match 1 { a if a == 1 => 2, _ => 3 };");
}

#[test]
fn test_pattern_matching_enum() {
    assert_parser_snapshot!("let x = match 1 { Foo([bar]) => bar };");
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
    assert_parser_snapshot!("// this is a comment\nlet x = 1;");
}

#[test]
fn test_multi_line_comments() {
    assert_parser_snapshot!("/* this is a comment */\nlet x = 1;");
    assert_parser_snapshot!("/* this is a comment\n   spanning multiple lines */\nlet x = 1;");
}

#[test]
fn test_string_literal() {
    assert_parser_snapshot!(r#""foo";"#);
}

#[test]
fn test_char_literal() {
    assert_parser_snapshot!(r"'a';");
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

#[test]
fn test_comments_can_appear_anywhere() {
    assert_parser_snapshot!(
        r"
        // this is a third comment
        let y = /* comment before expr */ 2 /* comment after expr */;
        // trailing comment
        [1,2] // trailing comment
        |> foo // trailing comment
        //|> bar
        |> baz;
        "
    );
}
