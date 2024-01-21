use insta::assert_ron_snapshot;
use pretty_assertions::assert_eq;

use tlang_ast::{
    node::{self, BinaryOp, FunctionDeclaration},
    symbols::SymbolId,
    token::Literal,
};

mod common;

#[test]
fn test_unary_minus() {
    assert_parser_snapshot!(parse!("-1;"));
}

#[test]
fn test_simple_arithmetic_calculations() {
    assert_parser_snapshot!(parse!("1 + 2 + 3;"));
    assert_parser_snapshot!(parse!("1 * 2 + 3;"));
}

#[test]
fn test_simple_arithmetic_sum_mult_precedence() {
    assert_parser_snapshot!(parse!("1 + 2 * 3;"));
}

#[test]
fn test_simple_arithmetic_sum_mult_precedence_parentheses() {
    assert_parser_snapshot!(parse!("(1 + 2) * 3;"));
}

#[test]
fn test_simple_arithmetic_with_identifiers() {
    assert_parser_snapshot!(parse!("let x = 1; let y = 2; x + y;"));
}

#[test]
fn test_simple_call() {
    assert_parser_snapshot!(parse!("foo(1, 2);"));
}

#[test]
fn test_nested_call() {
    assert_parser_snapshot!(parse!("foo(bar(1), 2);"));
}

#[test]
fn test_call_with_expression() {
    assert_parser_snapshot!(parse!("foo(1 + 2, 3);"));
}

#[test]
fn test_block_expression() {
    assert_parser_snapshot!(parse!("let x = { 1 + 2; };"));
}

#[test]
fn test_if_statement() {
    assert_parser_snapshot!(parse!("if (1 + 2) { 3 + 4; }"));
}

#[test]
fn test_if_else_statement() {
    assert_parser_snapshot!(parse!("if true { 1; } else { 2; }"));
    assert_parser_snapshot!(parse!("if (1 + 2) { 3 + 4; } else { 5 + 6; }"));
}

#[test]
fn test_if_else_if_statement() {
    let program = parse!("if true { 1; } else if false { 2; }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(IfElse {
                condition: Box::new(node::new!(Literal(Literal::Boolean(true)))),
                then_branch: Box::new(node::new!(Block(
                    vec![node::new!(ExpressionStatement(Box::new(node::new!(
                        Literal(Literal::Integer(1))
                    ))))],
                    Box::new(None)
                ))),
                else_branch: Box::new(Some(node::new!(IfElse {
                    condition: Box::new(node::new!(Literal(Literal::Boolean(false)))),
                    then_branch: Box::new(node::new!(Block(
                        vec![node::new!(ExpressionStatement(Box::new(node::new!(
                            Literal(Literal::Integer(2))
                        ))))],
                        Box::new(None)
                    ))),
                    else_branch: Box::new(None),
                }))),
            })
        )))]))
    );
}

#[test]
fn test_if_else_as_last_expression() {
    let program = parse!("fn main() { if true { 1 } else { 2 } }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionSingleDeclaration {
            id: SymbolId::new(1),
            name: Box::new(node::new!(Identifier("main".to_string()))),
            declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                parameters: vec![],
                guard: Box::new(None),
                return_type_annotation: Box::new(None),
                body: Box::new(node::new!(Block(
                    vec![],
                    Box::new(Some(node::new!(IfElse {
                        condition: Box::new(node::new!(Literal(Literal::Boolean(true)))),
                        then_branch: Box::new(node::new!(Block(
                            vec![],
                            Box::new(Some(node::new!(Literal(Literal::Integer(1)))))
                        ))),
                        else_branch: Box::new(Some(node::new!(Block(
                            vec![],
                            Box::new(Some(node::new!(Literal(Literal::Integer(2)))))
                        )))),
                    })))
                )))
            })))
        })]))
    );
}

#[test]
fn test_if_expression() {
    let program = parse!("let x = if (true) { 1; } else { 2; };");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            pattern: Box::new(node::new!(Identifier("x".to_string()))),
            expression: Box::new(node::new!(IfElse {
                condition: Box::new(node::new!(Literal(Literal::Boolean(true)))),
                then_branch: Box::new(node::new!(Block(
                    vec![node::new!(ExpressionStatement(Box::new(node::new!(
                        Literal(Literal::Integer(1))
                    ))))],
                    Box::new(None)
                ))),
                else_branch: Box::new(Some(node::new!(Block(
                    vec![node::new!(ExpressionStatement(Box::new(node::new!(
                        Literal(Literal::Integer(2))
                    ))))],
                    Box::new(None)
                )))),
            })),
            type_annotation: Box::new(None),
        })]))
    );
}

#[test]
fn test_stray_semicolon() {
    let program = parse!("1 + 2;;");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
            })
        )))]))
    );
}

#[test]
fn test_exponentiation() {
    let program = parse!("1 * 2 ** 3;");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(BinaryOp {
                op: BinaryOp::Multiply,
                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                rhs: Box::new(node::new!(BinaryOp {
                    op: BinaryOp::Exponentiation,
                    lhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                    rhs: Box::new(node::new!(Literal(Literal::Integer(3)))),
                })),
            })
        )))]))
    );

    let program = parse!("1 ** 2 * 3;");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(BinaryOp {
                op: BinaryOp::Multiply,
                lhs: Box::new(node::new!(BinaryOp {
                    op: BinaryOp::Exponentiation,
                    lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                    rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                })),
                rhs: Box::new(node::new!(Literal(Literal::Integer(3)))),
            })
        )))]))
    );

    let program = parse!("1 ** 2 ** 3;");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(BinaryOp {
                op: BinaryOp::Exponentiation,
                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                rhs: Box::new(node::new!(BinaryOp {
                    op: BinaryOp::Exponentiation,
                    lhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                    rhs: Box::new(node::new!(Literal(Literal::Integer(3)))),
                })),
            })
        )))]))
    );
}

#[test]
fn test_pattern_matching() {
    let program = parse!("let x = match 1 { 1 => 2, 3 => 4, _ => 5 };");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            pattern: Box::new(node::new!(Identifier("x".to_string()))),
            expression: Box::new(node::new!(Match {
                expression: Box::new(node::new!(Literal(Literal::Integer(1)))),
                arms: vec![
                    node::new!(MatchArm {
                        pattern: Box::new(node::new!(Literal(Literal::Integer(1)))),
                        expression: Box::new(node::new!(Literal(Literal::Integer(2)))),
                    }),
                    node::new!(MatchArm {
                        pattern: Box::new(node::new!(Literal(Literal::Integer(3)))),
                        expression: Box::new(node::new!(Literal(Literal::Integer(4)))),
                    }),
                    node::new!(MatchArm {
                        pattern: Box::new(node::new!(Wildcard)),
                        expression: Box::new(node::new!(Literal(Literal::Integer(5)))),
                    })
                ]
            })),
            type_annotation: Box::new(None),
        })]))
    );
}

#[test]
fn test_list_literal() {
    let program = parse!("let x = [];");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            pattern: Box::new(node::new!(Identifier("x".to_string()))),
            expression: Box::new(node::new!(List(vec![]))),
            type_annotation: Box::new(None),
        })]))
    );

    let program = parse!("let x = [1, 2, 3];");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            pattern: Box::new(node::new!(Identifier("x".to_string()))),
            expression: Box::new(node::new!(List(vec![
                node::new!(Literal(Literal::Integer(1))),
                node::new!(Literal(Literal::Integer(2))),
                node::new!(Literal(Literal::Integer(3))),
            ]))),
            type_annotation: Box::new(None),
        })]))
    );
}

#[test]
fn test_list_literal_with_trailing_comma() {
    let program = parse!("let x = [1, 2, 3,];");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            pattern: Box::new(node::new!(Identifier("x".to_string()))),
            expression: Box::new(node::new!(List(vec![
                node::new!(Literal(Literal::Integer(1))),
                node::new!(Literal(Literal::Integer(2))),
                node::new!(Literal(Literal::Integer(3))),
            ]))),
            type_annotation: Box::new(None),
        })]))
    );
}

#[test]
fn test_single_line_comments() {
    let program = parse!("// this is a comment");
    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(SingleLineComment(
            " this is a comment".to_string()
        ))]))
    );
}

#[test]
fn test_multi_line_comments() {
    let program = parse!("/* this is a comment */");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(MultiLineComment(
            " this is a comment ".to_string()
        ))]))
    );

    let program = parse!("/* this is a comment\n   spanning multiple lines */");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(MultiLineComment(
            " this is a comment\n   spanning multiple lines ".to_string()
        ))]))
    );
}

#[test]
fn test_string_literal() {
    let program = parse!(r#""foo";"#);

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(Literal(Literal::String("foo".to_string())))
        )))]))
    );
}

#[test]
fn test_char_literal() {
    let program = parse!(r#"'a';"#);

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(Literal(Literal::Char("a".to_string())))
        )))]))
    );
}

#[test]
fn test_dictionary_literal() {
    let program = parse!(r#"let x = { foo: 1, bar: 2 };"#);

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            pattern: Box::new(node::new!(Identifier("x".to_string()))),
            expression: Box::new(node::new!(Dict(vec![
                (
                    node::new!(Identifier("foo".to_string())),
                    node::new!(Literal(Literal::Integer(1))),
                ),
                (
                    node::new!(Identifier("bar".to_string())),
                    node::new!(Literal(Literal::Integer(2))),
                )
            ]))),
            type_annotation: Box::new(None),
        })]))
    );
}

#[test]
fn test_function_call_with_dictionary_no_parens() {
    let program = parse!(r#"foo { foo: 1, bar: 2 };"#);
    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(Call {
                function: Box::new(node::new!(Identifier("foo".to_string()))),
                arguments: vec![node::new!(Dict(vec![
                    (
                        node::new!(Identifier("foo".to_string())),
                        node::new!(Literal(Literal::Integer(1))),
                    ),
                    (
                        node::new!(Identifier("bar".to_string())),
                        node::new!(Literal(Literal::Integer(2))),
                    )
                ]))]
            })
        )))]))
    );
}

#[test]
fn test_call_return_value() {
    let program = parse!("foo()();");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(Call {
                function: Box::new(node::new!(Call {
                    function: Box::new(node::new!(Identifier("foo".to_string()))),
                    arguments: vec![]
                })),
                arguments: vec![]
            })
        )))]))
    );
}

#[test]
fn test_field_access_expressions() {
    let program = parse!("foo.bar;");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(FieldExpression {
                base: Box::new(node::new!(Identifier("foo".to_string()))),
                field: Box::new(node::new!(Identifier("bar".to_string()))),
            })
        )))]))
    );

    let program = parse!("foo.bar.baz;");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(FieldExpression {
                base: Box::new(node::new!(FieldExpression {
                    base: Box::new(node::new!(Identifier("foo".to_string()))),
                    field: Box::new(node::new!(Identifier("bar".to_string()))),
                })),
                field: Box::new(node::new!(Identifier("baz".to_string()))),
            })
        )))]))
    );
}

#[test]
fn test_dynamic_index_access() {
    let program = parse!("let x = foo[1];");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            pattern: Box::new(node::new!(Identifier("x".to_string()))),
            expression: Box::new(node::new!(IndexExpression {
                base: Box::new(node::new!(Identifier("foo".to_string()))),
                index: Box::new(node::new!(Literal(Literal::Integer(1)))),
            })),
            type_annotation: Box::new(None),
        })]))
    );

    let program = parse!("let x = foo[1][2];");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            pattern: Box::new(node::new!(Identifier("x".to_string()))),
            expression: Box::new(node::new!(IndexExpression {
                base: Box::new(node::new!(IndexExpression {
                    base: Box::new(node::new!(Identifier("foo".to_string()))),
                    index: Box::new(node::new!(Literal(Literal::Integer(1)))),
                })),
                index: Box::new(node::new!(Literal(Literal::Integer(2)))),
            })),
            type_annotation: Box::new(None),
        })]))
    );
}
