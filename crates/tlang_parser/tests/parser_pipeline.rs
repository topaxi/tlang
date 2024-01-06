use indoc::indoc;
use pretty_assertions::assert_eq;

use tlang_ast::{
    node::{self, BinaryOp, FunctionDeclaration},
    symbols::SymbolId,
    token::Literal,
};

mod common;

#[test]
fn test_pipeline_operator_to_identifier() {
    let program = parse!("1 |> foo;");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(BinaryOp {
                op: BinaryOp::Pipeline,
                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                rhs: Box::new(node::new!(Identifier("foo".to_string()))),
            })
        )))]))
    );
}

#[test]
fn test_pipeline_operator() {
    let program = parse!("1 |> foo();");
    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(BinaryOp {
                op: BinaryOp::Pipeline,
                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                rhs: Box::new(node::new!(Call {
                    function: Box::new(node::new!(Identifier("foo".to_string()))),
                    arguments: vec![]
                })),
            })
        )))])),
    );

    let program = parse!("1 |> foo() |> bar();");
    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(BinaryOp {
                op: BinaryOp::Pipeline,
                lhs: Box::new(node::new!(BinaryOp {
                    op: BinaryOp::Pipeline,
                    lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                    rhs: Box::new(node::new!(Call {
                        function: Box::new(node::new!(Identifier("foo".to_string()))),
                        arguments: vec![]
                    })),
                })),
                rhs: Box::new(node::new!(Call {
                    function: Box::new(node::new!(Identifier("bar".to_string()))),
                    arguments: vec![]
                })),
            })
        )))])),
    );
}

#[test]
fn test_pipeline_operator_precedence() {
    let program = parse!("1 + 2 |> foo();");
    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(BinaryOp {
                op: BinaryOp::Pipeline,
                lhs: Box::new(node::new!(BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                    rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                })),
                rhs: Box::new(node::new!(Call {
                    function: Box::new(node::new!(Identifier("foo".to_string()))),
                    arguments: vec![]
                })),
            })
        )))])),
    );
}

#[test]
fn test_pipeline_operator_to_function_call_with_arguments() {
    let program = parse!("1 |> foo(2);");
    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(BinaryOp {
                op: BinaryOp::Pipeline,
                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                rhs: Box::new(node::new!(Call {
                    function: Box::new(node::new!(Identifier("foo".to_string()))),
                    arguments: vec![node::new!(Literal(Literal::Integer(2)))]
                })),
            })
        )))])),
    );
}

#[test]
fn test_pipeline_operator_to_function_call_with_wildcards() {
    let program = parse!("1 |> foo(2, _);");
    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(BinaryOp {
                op: BinaryOp::Pipeline,
                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                rhs: Box::new(node::new!(Call {
                    function: Box::new(node::new!(Identifier("foo".to_string()))),
                    arguments: vec![
                        node::new!(Literal(Literal::Integer(2))),
                        node::new!(Wildcard)
                    ]
                })),
            })
        )))])),
    );
}

#[test]
fn test_pipeline_operator_long_chaining() {
    let program = parse!(indoc! {"
        [1,2,3]
        |> map(fn (x) { x ** 2 })
        |> filter(fn (x) { x % 2 == 0 })
        |> foldl(fn (acc, x) { acc + x }, 0)
        |> log();
    "});
    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(BinaryOp {
                op: BinaryOp::Pipeline,
                lhs: Box::new(node::new!(BinaryOp {
                    op: BinaryOp::Pipeline,
                    lhs: Box::new(node::new!(BinaryOp {
                        op: BinaryOp::Pipeline,
                        lhs: Box::new(node::new!(BinaryOp {
                            op: BinaryOp::Pipeline,
                            lhs: Box::new(node::new!(List(vec![
                                node::new!(Literal(Literal::Integer(1))),
                                node::new!(Literal(Literal::Integer(2))),
                                node::new!(Literal(Literal::Integer(3))),
                            ]))),
                            rhs: Box::new(node::new!(Call {
                                function: Box::new(node::new!(Identifier("map".to_string()))),
                                arguments: vec![node::new!(FunctionExpression {
                                    id: SymbolId::new(2),
                                    name: None,
                                    declaration: Box::new(FunctionDeclaration {
                                        parameters: vec![node::new!(FunctionParameter {
                                            id: SymbolId::new(1),
                                            node: Box::new(node::new!(Identifier("x".to_string()))),
                                            type_annotation: None,
                                        })],
                                        guard: None,
                                        return_type_annotation: None,
                                        body: Box::new(node::new!(Block(
                                            vec![],
                                            Some(Box::new(node::new!(BinaryOp {
                                                op: BinaryOp::Exponentiation,
                                                lhs: Box::new(node::new!(Identifier(
                                                    "x".to_string()
                                                ))),
                                                rhs: Box::new(node::new!(Literal(
                                                    Literal::Integer(2)
                                                ))),
                                            })))
                                        )))
                                    }),
                                })]
                            })),
                        })),
                        rhs: Box::new(node::new!(Call {
                            function: Box::new(node::new!(Identifier("filter".to_string()))),
                            arguments: vec![node::new!(FunctionExpression {
                                id: SymbolId::new(4),
                                name: None,
                                declaration: Box::new(FunctionDeclaration {
                                    parameters: vec![node::new!(FunctionParameter {
                                        id: SymbolId::new(3),
                                        node: Box::new(node::new!(Identifier("x".to_string()))),
                                        type_annotation: None,
                                    })],
                                    guard: None,
                                    return_type_annotation: None,
                                    body: Box::new(node::new!(Block(
                                        vec![],
                                        Some(Box::new(node::new!(BinaryOp {
                                            op: BinaryOp::Equal,
                                            lhs: Box::new(node::new!(BinaryOp {
                                                op: BinaryOp::Modulo,
                                                lhs: Box::new(node::new!(Identifier(
                                                    "x".to_string()
                                                ))),
                                                rhs: Box::new(node::new!(Literal(
                                                    Literal::Integer(2)
                                                ))),
                                            })),
                                            rhs: Box::new(node::new!(Literal(Literal::Integer(0)))),
                                        })))
                                    )))
                                }),
                            })]
                        })),
                    })),
                    rhs: Box::new(node::new!(Call {
                        function: Box::new(node::new!(Identifier("foldl".to_string()))),
                        arguments: vec![
                            node::new!(FunctionExpression {
                                id: SymbolId::new(7),
                                name: None,
                                declaration: Box::new(FunctionDeclaration {
                                    parameters: vec![
                                        node::new!(FunctionParameter {
                                            id: SymbolId::new(5),
                                            node: Box::new(node::new!(Identifier(
                                                "acc".to_string()
                                            ))),
                                            type_annotation: None,
                                        }),
                                        node::new!(FunctionParameter {
                                            id: SymbolId::new(6),
                                            node: Box::new(node::new!(Identifier("x".to_string()))),
                                            type_annotation: None,
                                        }),
                                    ],
                                    guard: None,
                                    return_type_annotation: None,
                                    body: Box::new(node::new!(Block(
                                        vec![],
                                        Some(Box::new(node::new!(BinaryOp {
                                            op: BinaryOp::Add,
                                            lhs: Box::new(node::new!(Identifier(
                                                "acc".to_string()
                                            ))),
                                            rhs: Box::new(node::new!(Identifier("x".to_string()))),
                                        })))
                                    )))
                                })
                            }),
                            node::new!(Literal(Literal::Integer(0))),
                        ]
                    })),
                })),
                rhs: Box::new(node::new!(Call {
                    function: Box::new(node::new!(Identifier("log".to_string()))),
                    arguments: vec![],
                })),
            })
        )))]))
    );
}
