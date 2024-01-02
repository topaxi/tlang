use indoc::indoc;
use pretty_assertions::assert_eq;

use crate::parser::Parser;
use tlang_ast::{
    node::{self, AstNode, BinaryOp, FunctionDeclaration, Node, PrefixOp},
    symbols::SymbolId,
    token::Literal,
};

macro_rules! parse {
    ($source:expr) => {{
        let mut parser = Parser::from_source($source);
        parser.parse()
    }};
}

#[test]
fn test_simple_variable_declaration() {
    let program = parse!("let x = 1 + 2;");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            name: "x".to_string(),
            value: Box::new(node::new!(BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
            })),
        })]))
    );
}

#[test]
fn test_simple_arithmetic_calculations() {
    let program = parse!("1 + 2 + 3;");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(node::new!(BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                    rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                })),
                rhs: Box::new(node::new!(Literal(Literal::Integer(3)))),
            })
        )))]))
    );

    let program = parse!("1 * 2 + 3;");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(node::new!(BinaryOp {
                    op: BinaryOp::Multiply,
                    lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                    rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                })),
                rhs: Box::new(node::new!(Literal(Literal::Integer(3)))),
            })
        )))]))
    );
}

#[test]
fn test_simple_arithmetic_sum_mult_precedence() {
    let program = parse!("1 + 2 * 3;");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                rhs: Box::new(node::new!(BinaryOp {
                    op: BinaryOp::Multiply,
                    lhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                    rhs: Box::new(node::new!(Literal(Literal::Integer(3)))),
                })),
            })
        )))]))
    );
}

#[test]
fn test_simple_arithmetic_sum_mult_precedence_parentheses() {
    let program = parse!("(1 + 2) * 3;");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(BinaryOp {
                op: BinaryOp::Multiply,
                lhs: Box::new(node::new!(BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                    rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                })),
                rhs: Box::new(node::new!(Literal(Literal::Integer(3)))),
            })
        )))]))
    );
}

#[test]
fn test_simple_arithmetic_with_identifiers() {
    let program = parse!("let x = 1; let y = 2; x + y;");

    assert_eq!(
        program,
        node::new!(Program(vec![
            node::new!(VariableDeclaration {
                id: SymbolId::new(1),
                name: "x".to_string(),
                value: Box::new(node::new!(Literal(Literal::Integer(1)))),
            }),
            node::new!(VariableDeclaration {
                id: SymbolId::new(2),
                name: "y".to_string(),
                value: Box::new(node::new!(Literal(Literal::Integer(2)))),
            }),
            node::new!(ExpressionStatement(Box::new(node::new!(BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(node::new!(Identifier("x".to_string()))),
                rhs: Box::new(node::new!(Identifier("y".to_string()))),
            })))),
        ]))
    );
}

#[test]
fn test_simple_call() {
    let program = parse!("foo(1, 2);");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(Call {
                function: Box::new(node::new!(Identifier("foo".to_string()))),
                arguments: vec![
                    node::new!(Literal(Literal::Integer(1))),
                    node::new!(Literal(Literal::Integer(2)))
                ],
            })
        )))]))
    );
}

#[test]
fn test_nested_call() {
    let program = parse!("foo(bar(1), 2);");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(Call {
                function: Box::new(node::new!(Identifier("foo".to_string()))),
                arguments: vec![
                    node::new!(Call {
                        function: Box::new(node::new!(Identifier("bar".to_string()))),
                        arguments: vec![node::new!(Literal(Literal::Integer(1)))]
                    }),
                    node::new!(Literal(Literal::Integer(2)))
                ],
            })
        )))]))
    );
}

#[test]
fn test_call_with_expression() {
    let program = parse!("foo(1 + 2, 3);");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(Call {
                function: Box::new(node::new!(Identifier("foo".to_string()))),
                arguments: vec![
                    node::new!(BinaryOp {
                        op: BinaryOp::Add,
                        lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                        rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                    }),
                    node::new!(Literal(Literal::Integer(3))),
                ],
            })
        )))]))
    );
}

#[test]
fn test_block_expression() {
    let program = parse!("let x = { 1 + 2; };");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            name: "x".to_string(),
            value: Box::new(node::new!(Block(
                vec![node::new!(ExpressionStatement(Box::new(node::new!(
                    BinaryOp {
                        op: BinaryOp::Add,
                        lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                        rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                    }
                ))))],
                None
            )))
        })]))
    );
}

#[test]
fn test_if_statement() {
    let program = parse!("if (1 + 2) { 3 + 4; }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(IfElse {
                condition: Box::new(node::new!(BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                    rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                })),
                then_branch: Box::new(node::new!(Block(
                    vec![node::new!(ExpressionStatement(Box::new(node::new!(
                        BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(node::new!(Literal(Literal::Integer(3)))),
                            rhs: Box::new(node::new!(Literal(Literal::Integer(4)))),
                        }
                    ))))],
                    None
                ))),
                else_branch: None,
            })
        )))]))
    );
}

#[test]
fn test_if_else_statement() {
    let program = parse!("if true { 1; } else { 2; }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(IfElse {
                condition: Box::new(node::new!(Literal(Literal::Boolean(true)))),
                then_branch: Box::new(node::new!(Block(
                    vec![node::new!(ExpressionStatement(Box::new(node::new!(
                        Literal(Literal::Integer(1))
                    ))))],
                    None
                ))),
                else_branch: Some(Box::new(node::new!(Block(
                    vec![node::new!(ExpressionStatement(Box::new(node::new!(
                        Literal(Literal::Integer(2))
                    ))))],
                    None
                )))),
            })
        )))]))
    );

    let program = parse!("if (1 + 2) { 3 + 4; } else { 5 + 6; }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(IfElse {
                condition: Box::new(node::new!(BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                    rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                })),
                then_branch: Box::new(node::new!(Block(
                    vec![node::new!(ExpressionStatement(Box::new(node::new!(
                        BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(node::new!(Literal(Literal::Integer(3)))),
                            rhs: Box::new(node::new!(Literal(Literal::Integer(4)))),
                        }
                    ))))],
                    None
                ))),
                else_branch: Some(Box::new(node::new!(Block(
                    vec![node::new!(ExpressionStatement(Box::new(node::new!(
                        BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(node::new!(Literal(Literal::Integer(5)))),
                            rhs: Box::new(node::new!(Literal(Literal::Integer(6)))),
                        }
                    ))))],
                    None
                )))),
            })
        )))]))
    );
}

#[test]
fn test_if_else_as_last_expression() {
    let program = parse!("fn main() { if true { 1 } else { 2 } }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclaration {
            id: SymbolId::new(1),
            name: "main".to_string(),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![],
                body: Box::new(node::new!(Block(
                    vec![],
                    Some(Box::new(node::new!(IfElse {
                        condition: Box::new(node::new!(Literal(Literal::Boolean(true)))),
                        then_branch: Box::new(node::new!(Block(
                            vec![],
                            Some(Box::new(node::new!(Literal(Literal::Integer(1)))))
                        ))),
                        else_branch: Some(Box::new(node::new!(Block(
                            vec![],
                            Some(Box::new(node::new!(Literal(Literal::Integer(2)))))
                        )))),
                    })))
                )))
            })
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
            name: "x".to_string(),
            value: Box::new(node::new!(IfElse {
                condition: Box::new(node::new!(Literal(Literal::Boolean(true)))),
                then_branch: Box::new(node::new!(Block(
                    vec![node::new!(ExpressionStatement(Box::new(node::new!(
                        Literal(Literal::Integer(1))
                    ))))],
                    None
                ))),
                else_branch: Some(Box::new(node::new!(Block(
                    vec![node::new!(ExpressionStatement(Box::new(node::new!(
                        Literal(Literal::Integer(2))
                    ))))],
                    None
                )))),
            }))
        })]))
    );
}

#[test]
fn test_function_declaration() {
    let program = parse!("fn foo() { bar(); 1 + 2; }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclaration {
            id: SymbolId::new(1),
            name: "foo".to_string(),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![],
                body: Box::new(node::new!(Block(
                    vec![
                        node::new!(ExpressionStatement(Box::new(node::new!(Call {
                            function: Box::new(node::new!(Identifier("bar".to_string()))),
                            arguments: vec![]
                        })))),
                        node::new!(ExpressionStatement(Box::new(node::new!(BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                            rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                        }))))
                    ],
                    None
                )))
            })
        })]))
    );
}

#[test]
fn test_function_declaration_with_parameters() {
    let program = parse!("fn foo(x, y) { 1 + 2; }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclaration {
            id: SymbolId::new(3),
            name: "foo".to_string(),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![
                    node::new!(FunctionParameter {
                        id: SymbolId::new(1),
                        node: Box::new(node::new!(Identifier("x".to_string())))
                    }),
                    node::new!(FunctionParameter {
                        id: SymbolId::new(2),
                        node: Box::new(node::new!(Identifier("y".to_string())))
                    }),
                ],
                body: Box::new(node::new!(Block(
                    vec![node::new!(ExpressionStatement(Box::new(node::new!(
                        BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                            rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                        }
                    ))))],
                    None
                )))
            })
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
fn test_nameless_function_expressions() {
    let program = parse!("let x = fn() { 1 + 2; };");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(2),
            name: "x".to_string(),
            value: Box::new(node::new!(FunctionExpression {
                id: SymbolId::new(1),
                name: None,
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![],
                    body: Box::new(node::new!(Block(
                        vec![node::new!(ExpressionStatement(Box::new(node::new!(
                            BinaryOp {
                                op: BinaryOp::Add,
                                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                                rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                            }
                        ))))],
                        None
                    )))
                })
            }))
        })]))
    );

    let program = parse!("let x = fn(x, y) { 1 + 2; };");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(4),
            name: "x".to_string(),
            value: Box::new(node::new!(FunctionExpression {
                id: SymbolId::new(3),
                name: None,
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(1),
                            node: Box::new(node::new!(Identifier("x".to_string())))
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            node: Box::new(node::new!(Identifier("y".to_string())))
                        }),
                    ],
                    body: Box::new(node::new!(Block(
                        vec![node::new!(ExpressionStatement(Box::new(node::new!(
                            BinaryOp {
                                op: BinaryOp::Add,
                                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                                rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                            }
                        ))))],
                        None
                    )))
                })
            }))
        })]))
    );
}

#[test]
fn test_function_expression_without_name_no_argument_parenthesis() {
    let program = parse!("let x = fn { 1 + 2; };");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(2),
            name: "x".to_string(),
            value: Box::new(node::new!(FunctionExpression {
                id: SymbolId::new(1),
                name: None,
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![],
                    body: Box::new(node::new!(Block(
                        vec![node::new!(ExpressionStatement(Box::new(node::new!(
                            BinaryOp {
                                op: BinaryOp::Add,
                                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                                rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                            }
                        ))))],
                        None
                    )))
                })
            }))
        })]))
    );
}

#[test]
fn test_function_expressions() {
    let program = parse!("let x = fn foo() { 1 + 2; };");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(2),
            name: "x".to_string(),
            value: Box::new(node::new!(FunctionExpression {
                id: SymbolId::new(1),
                name: Some("foo".to_string()),
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![],
                    body: Box::new(node::new!(Block(
                        vec![node::new!(ExpressionStatement(Box::new(node::new!(
                            BinaryOp {
                                op: BinaryOp::Add,
                                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                                rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                            }
                        ))))],
                        None
                    )))
                })
            }))
        })]))
    );

    let program = parse!("let x = fn foo(x, y) { 1 + 2; };");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(4),
            name: "x".to_string(),
            value: Box::new(node::new!(FunctionExpression {
                id: SymbolId::new(3),
                name: Some("foo".to_string()),
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(1),
                            node: Box::new(node::new!(Identifier("x".to_string())))
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            node: Box::new(node::new!(Identifier("y".to_string())))
                        }),
                    ],
                    body: Box::new(node::new!(Block(
                        vec![node::new!(ExpressionStatement(Box::new(node::new!(
                            BinaryOp {
                                op: BinaryOp::Add,
                                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                                rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                            }
                        ))))],
                        None
                    )))
                })
            }))
        })]))
    );
}

#[test]
fn test_pattern_matching() {
    let program = parse!("let x = match 1 { 1 => 2, 3 => 4, _ => 5 };");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            name: "x".to_string(),
            value: Box::new(node::new!(Match {
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
            }))
        })]))
    );
}

#[test]
fn test_explicit_return_statements() {
    let program = parse!("fn foo() { return 1 + 2; }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclaration {
            id: SymbolId::new(1),
            name: "foo".to_string(),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![],
                body: Box::new(node::new!(Block(
                    vec![node::new!(ReturnStatement(Some(Box::new(node::new!(
                        BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                            rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                        }
                    )))))],
                    None
                )))
            })
        })]))
    );

    let program = parse!("fn foo() { return; }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclaration {
            id: SymbolId::new(1),
            name: "foo".to_string(),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![],
                body: Box::new(node::new!(Block(
                    vec![node::new!(ReturnStatement(None))],
                    None
                )))
            })
        })]))
    );

    let program = parse!("let x = fn() { return 1 + 2; };");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(2),
            name: "x".to_string(),
            value: Box::new(node::new!(FunctionExpression {
                id: SymbolId::new(1),
                name: None,
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![],
                    body: Box::new(node::new!(Block(
                        vec![node::new!(ReturnStatement(Some(Box::new(node::new!(
                            BinaryOp {
                                op: BinaryOp::Add,
                                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                                rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                            }
                        )))))],
                        None
                    )))
                })
            }))
        })]))
    );
}

#[test]
fn test_implicit_return_expressions() {
    let program = parse!("fn foo() { 1 + 2 }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclaration {
            id: SymbolId::new(1),
            name: "foo".to_string(),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![],
                body: Box::new(node::new!(Block(
                    vec![],
                    Some(Box::new(node::new!(BinaryOp {
                        op: BinaryOp::Add,
                        lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                        rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                    })))
                )))
            })
        })]))
    );

    let program = parse!("fn foo() {}");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclaration {
            id: SymbolId::new(1),
            name: "foo".to_string(),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![],
                body: Box::new(node::new!(Block(vec![], None)))
            })
        })]))
    );

    let program = parse!("let x = fn() { 1 + 2 };");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(2),
            name: "x".to_string(),
            value: Box::new(node::new!(FunctionExpression {
                id: SymbolId::new(1),
                name: None,
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![],
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                            rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                        })))
                    )))
                })
            }))
        })]))
    );

    let program = parse!("fn foo() { let x = 1; x }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclaration {
            id: SymbolId::new(2),
            name: "foo".to_string(),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![],
                body: Box::new(node::new!(Block(
                    vec![node::new!(VariableDeclaration {
                        id: SymbolId::new(1),
                        name: "x".to_string(),
                        value: Box::new(node::new!(Literal(Literal::Integer(1)))),
                    })],
                    Some(Box::new(node::new!(Identifier("x".to_string()))))
                )))
            })
        })]))
    );
}

#[test]
fn test_recursive_factorial_functional_definition() {
    let program = parse!(indoc! {"
        fn factorial(0) { return 1; }
        fn factorial(n) { return n * factorial(n - 1); }
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclarations {
            id: SymbolId::new(3),
            name: "factorial".to_string(),
            declarations: vec![
                FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(1),
                        node: Box::new(node::new!(Literal(Literal::Integer(0))))
                    })],
                    body: Box::new(node::new!(Block(
                        vec![node::new!(ReturnStatement(Some(Box::new(node::new!(
                            Literal(Literal::Integer(1))
                        )))))],
                        None
                    )))
                },
                FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(2),
                        node: Box::new(node::new!(Identifier("n".to_string())))
                    })],
                    body: Box::new(node::new!(Block(
                        vec![node::new!(ReturnStatement(Some(Box::new(node::new!(
                            BinaryOp {
                                op: BinaryOp::Multiply,
                                lhs: Box::new(node::new!(Identifier("n".to_string()))),
                                rhs: Box::new(node::new!(Call {
                                    function: Box::new(node::new!(Identifier(
                                        "factorial".to_string()
                                    ))),
                                    arguments: vec![node::new!(BinaryOp {
                                        op: BinaryOp::Subtract,
                                        lhs: Box::new(node::new!(Identifier("n".to_string()))),
                                        rhs: Box::new(node::new!(Literal(Literal::Integer(1))))
                                    })]
                                }))
                            }
                        )))))],
                        None
                    )))
                }
            ]
        })]))
    );
}

#[test]
fn test_recursive_sum() {
    let program = parse!(indoc! {"
        fn sum([]) { 0 }
        fn sum([x, ...xs]) { x + sum(xs) }
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclarations {
            id: SymbolId::new(3),
            name: "sum".to_string(),
            declarations: vec![
                FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(1),
                        node: Box::new(node::new!(List(vec![])))
                    })],
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(Literal(Literal::Integer(0))))),
                    )))
                },
                FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(2),
                        node: Box::new(node::new!(List(vec![
                            node::new!(Identifier("x".to_string())),
                            node::new!(PrefixOp(
                                PrefixOp::Rest,
                                Box::new(node::new!(Identifier("xs".to_string())))
                            ))
                        ])))
                    })],
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(node::new!(Identifier("x".to_string()))),
                            rhs: Box::new(node::new!(Call {
                                function: Box::new(node::new!(Identifier("sum".to_string()))),
                                arguments: vec![node::new!(Identifier("xs".to_string()))]
                            }))
                        })))
                    )))
                }
            ]
        })]))
    )
}

// TODO: Comments between function declarations should be allowed.
#[ignore = "not implemented yet"]
#[test]
fn test_functional_function_declaration_with_comments_inbetween() {
    let program = parse!(indoc! {"
        fn foo(1) { 1 }
        // comment
        fn foo(n) { n * 2 }
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclarations {
            id: SymbolId::new(1),
            name: "foo".to_string(),
            declarations: vec![
                FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(2),
                        node: Box::new(node::new!(Literal(Literal::Integer(1))))
                    })],
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(Literal(Literal::Integer(1))))),
                    )))
                },
                FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(3),
                        node: Box::new(node::new!(Identifier("n".to_string())))
                    })],
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(BinaryOp {
                            op: BinaryOp::Multiply,
                            lhs: Box::new(node::new!(Identifier("n".to_string()))),
                            rhs: Box::new(node::new!(Literal(Literal::Integer(2))))
                        })))
                    )))
                }
            ]
        })]))
    );
}

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
fn test_list_literal() {
    let program = parse!("let x = [];");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            name: "x".to_string(),
            value: Box::new(node::new!(List(vec![])))
        })]))
    );

    let program = parse!("let x = [1, 2, 3];");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            name: "x".to_string(),
            value: Box::new(node::new!(List(vec![
                node::new!(Literal(Literal::Integer(1))),
                node::new!(Literal(Literal::Integer(2))),
                node::new!(Literal(Literal::Integer(3))),
            ])))
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
            name: "x".to_string(),
            value: Box::new(node::new!(List(vec![
                node::new!(Literal(Literal::Integer(1))),
                node::new!(Literal(Literal::Integer(2))),
                node::new!(Literal(Literal::Integer(3))),
            ])))
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
            name: "x".to_string(),
            value: Box::new(node::new!(Dict(vec![
                (
                    node::new!(Identifier("foo".to_string())),
                    node::new!(Literal(Literal::Integer(1))),
                ),
                (
                    node::new!(Identifier("bar".to_string())),
                    node::new!(Literal(Literal::Integer(2))),
                )
            ])))
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
fn test_enums() {
    let program = parse!(indoc! {"
        enum Option {
            Some(x),
            None,
        }

        let x = Option::Some(42);
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![
            node::new!(EnumDeclaration {
                id: SymbolId::new(1),
                name: "Option".to_string(),
                variants: vec![
                    node::new!(EnumVariant {
                        name: "Some".to_string(),
                        named_fields: false,
                        parameters: vec![node::new!(Identifier("x".to_string()))]
                    }),
                    node::new!(EnumVariant {
                        name: "None".to_string(),
                        named_fields: false,
                        parameters: vec![]
                    }),
                ]
            }),
            node::new!(VariableDeclaration {
                id: SymbolId::new(2),
                name: "x".to_string(),
                value: Box::new(node::new!(Call {
                    function: Box::new(node::new!(NestedIdentifier(vec![
                        "Option".to_string(),
                        "Some".to_string()
                    ]))),
                    arguments: vec![node::new!(Literal(Literal::Integer(42)))]
                }))
            }),
        ]))
    );
}

#[test]
fn test_enums_with_fields() {
    let program = parse!(indoc! {"
        enum Option {
            Some { x },
            None,
        }
        let x = Option::Some { x: 42 };
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![
            node::new!(EnumDeclaration {
                id: SymbolId::new(1),
                name: "Option".to_string(),
                variants: vec![
                    node::new!(EnumVariant {
                        name: "Some".to_string(),
                        named_fields: true,
                        parameters: vec![node::new!(Identifier("x".to_string()))]
                    }),
                    node::new!(EnumVariant {
                        name: "None".to_string(),
                        named_fields: false,
                        parameters: vec![]
                    }),
                ]
            }),
            node::new!(VariableDeclaration {
                id: SymbolId::new(2),
                name: "x".to_string(),
                value: Box::new(node::new!(Call {
                    function: Box::new(node::new!(NestedIdentifier(vec![
                        "Option".to_string(),
                        "Some".to_string()
                    ]))),
                    arguments: vec![node::new!(Dict(vec![(
                        node::new!(Identifier("x".to_string())),
                        node::new!(Literal(Literal::Integer(42))),
                    )]))]
                }))
            }),
        ]))
    );
}

#[test]
fn test_enum_tree_max_depth() {
    let program = parse!(indoc! {"
        enum Tree {
            Leaf(value),
            Node { left, right },
        }

        fn maximum_depth(Tree::Leaf(_)) { 1 }
        fn maximum_depth(Tree::Node { left, right }) { 1 + max(maximum_depth(left), maximum_depth(right)) }

        fn main() {
            let x = Tree::Node {
                left: Tree::Leaf(1),
                right: Tree::Node {
                    left: Tree::Leaf(2),
                    right: Tree::Leaf(3),
                },
            };
        }
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![
            node::new!(EnumDeclaration {
                id: SymbolId::new(1),
                name: "Tree".to_string(),
                variants: vec![
                    node::new!(EnumVariant {
                        name: "Leaf".to_string(),
                        named_fields: false,
                        parameters: vec![node::new!(Identifier("value".to_string()))]
                    }),
                    node::new!(EnumVariant {
                        name: "Node".to_string(),
                        named_fields: true,
                        parameters: vec![
                            node::new!(Identifier("left".to_string())),
                            node::new!(Identifier("right".to_string()))
                        ]
                    }),
                ]
            }),
            node::new!(FunctionDeclarations {
                id: SymbolId::new(4),
                name: "maximum_depth".to_string(),
                declarations: vec![
                    FunctionDeclaration {
                        parameters: vec![node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            node: Box::new(node::new!(EnumExtraction {
                                identifier: Box::new(node::new!(NestedIdentifier(vec![
                                    "Tree".to_string(),
                                    "Leaf".to_string()
                                ]))),
                                elements: vec![node::new!(Wildcard)],
                                named_fields: false,
                            }))
                        })],
                        body: Box::new(node::new!(Block(
                            vec![],
                            Some(Box::new(node::new!(Literal(Literal::Integer(1)))))
                        )))
                    },
                    FunctionDeclaration {
                        parameters: vec![node::new!(FunctionParameter {
                            id: SymbolId::new(3),
                            node: Box::new(node::new!(EnumExtraction {
                                identifier: Box::new(node::new!(NestedIdentifier(vec![
                                    "Tree".to_string(),
                                    "Node".to_string()
                                ]))),
                                elements: vec![
                                    node::new!(Identifier("left".to_string())),
                                    node::new!(Identifier("right".to_string()))
                                ],
                                named_fields: true,
                            }))
                        })],
                        body: Box::new(node::new!(Block(
                            vec![],
                            Some(Box::new(node::new!(BinaryOp {
                                op: BinaryOp::Add,
                                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                                rhs: Box::new(node::new!(Call {
                                    function: Box::new(node::new!(Identifier("max".to_string()))),
                                    arguments: vec![
                                        node::new!(Call {
                                            function: Box::new(node::new!(Identifier(
                                                "maximum_depth".to_string()
                                            ))),
                                            arguments: vec![node::new!(Identifier(
                                                "left".to_string()
                                            ))]
                                        }),
                                        node::new!(Call {
                                            function: Box::new(node::new!(Identifier(
                                                "maximum_depth".to_string()
                                            ))),
                                            arguments: vec![node::new!(Identifier(
                                                "right".to_string()
                                            ))]
                                        }),
                                    ]
                                }))
                            })))
                        )))
                    }
                ]
            }),
            node::new!(FunctionDeclaration {
                id: SymbolId::new(6),
                name: "main".to_string(),
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![],
                    body: Box::new(node::new!(Block(
                        vec![node::new!(VariableDeclaration {
                            id: SymbolId::new(5),
                            name: "x".to_string(),
                            value: Box::new(node::new!(Call {
                                function: Box::new(node::new!(NestedIdentifier(vec![
                                    "Tree".to_string(),
                                    "Node".to_string()
                                ]))),
                                arguments: vec![node::new!(Dict(vec![
                                    (
                                        node::new!(Identifier("left".to_string())),
                                        node::new!(Call {
                                            function: Box::new(node::new!(NestedIdentifier(vec![
                                                "Tree".to_string(),
                                                "Leaf".to_string()
                                            ]))),
                                            arguments: vec![node::new!(Literal(Literal::Integer(
                                                1
                                            )))]
                                        })
                                    ),
                                    (
                                        node::new!(Identifier("right".to_string())),
                                        node::new!(Call {
                                            function: Box::new(node::new!(NestedIdentifier(vec![
                                                "Tree".to_string(),
                                                "Node".to_string()
                                            ]))),
                                            arguments: vec![node::new!(Dict(vec![
                                                (
                                                    node::new!(Identifier("left".to_string())),
                                                    node::new!(Call {
                                                        function: Box::new(node::new!(
                                                            NestedIdentifier(vec![
                                                                "Tree".to_string(),
                                                                "Leaf".to_string()
                                                            ])
                                                        )),
                                                        arguments: vec![node::new!(Literal(
                                                            Literal::Integer(2)
                                                        ))]
                                                    })
                                                ),
                                                (
                                                    node::new!(Identifier("right".to_string())),
                                                    node::new!(Call {
                                                        function: Box::new(node::new!(
                                                            NestedIdentifier(vec![
                                                                "Tree".to_string(),
                                                                "Leaf".to_string()
                                                            ])
                                                        )),
                                                        arguments: vec![node::new!(Literal(
                                                            Literal::Integer(3)
                                                        ))]
                                                    })
                                                )
                                            ]))]
                                        })
                                    )
                                ]))]
                            }))
                        })],
                        None
                    )))
                })
            })
        ]))
    );
}

#[test]
fn test_enum_extraction() {
    let program = parse!(indoc! {"
        enum Option {
            Some(value),
            None,
        }

        fn unwrap(Option::None) { panic(\"Cannot unwrap None\") }
        fn unwrap(Option::Some(value)) { value }
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![
            node::new!(EnumDeclaration {
                id: SymbolId::new(1),
                name: "Option".to_string(),
                variants: vec![
                    node::new!(EnumVariant {
                        name: "Some".to_string(),
                        named_fields: false,
                        parameters: vec![node::new!(Identifier("value".to_string()))]
                    }),
                    node::new!(EnumVariant {
                        name: "None".to_string(),
                        named_fields: false,
                        parameters: vec![]
                    }),
                ]
            }),
            node::new!(FunctionDeclarations {
                id: SymbolId::new(4),
                name: "unwrap".to_string(),
                declarations: vec![
                    FunctionDeclaration {
                        parameters: vec![node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            node: Box::new(node::new!(EnumExtraction {
                                identifier: Box::new(node::new!(NestedIdentifier(vec![
                                    "Option".to_string(),
                                    "None".to_string()
                                ]))),
                                elements: vec![],
                                named_fields: false,
                            }))
                        })],
                        body: Box::new(node::new!(Block(
                            vec![],
                            Some(Box::new(node::new!(Call {
                                function: Box::new(node::new!(Identifier("panic".to_string()))),
                                arguments: vec![node::new!(Literal(Literal::String(
                                    "Cannot unwrap None".to_string()
                                )))]
                            })))
                        )))
                    },
                    FunctionDeclaration {
                        parameters: vec![node::new!(FunctionParameter {
                            id: SymbolId::new(3),
                            node: Box::new(node::new!(EnumExtraction {
                                identifier: Box::new(node::new!(NestedIdentifier(vec![
                                    "Option".to_string(),
                                    "Some".to_string()
                                ]))),
                                elements: vec![node::new!(Identifier("value".to_string()))],
                                named_fields: false,
                            }))
                        })],
                        body: Box::new(node::new!(Block(
                            vec![],
                            Some(Box::new(node::new!(Identifier("value".to_string()))))
                        )))
                    }
                ]
            })
        ]))
    );
}

#[test]
fn test_explicit_tail_recursive_call() {
    let program = parse!(indoc! {"
        fn factorial(n) { factorial(n, 1) }
        fn factorial(0, acc) { acc }
        fn factorial(n, acc) { rec factorial(n - 1, n * acc) }
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclarations {
            id: SymbolId::new(6),
            name: "factorial".to_string(),
            declarations: vec![
                FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(1),
                        node: Box::new(node::new!(Identifier("n".to_string())))
                    })],
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(Call {
                            function: Box::new(node::new!(Identifier("factorial".to_string()))),
                            arguments: vec![
                                node::new!(Identifier("n".to_string())),
                                node::new!(Literal(Literal::Integer(1)))
                            ]
                        })))
                    ))),
                },
                FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            node: Box::new(node::new!(Literal(Literal::Integer(0))))
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(3),
                            node: Box::new(node::new!(Identifier("acc".to_string())))
                        })
                    ],
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(Identifier("acc".to_string()))))
                    ))),
                },
                FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(4),
                            node: Box::new(node::new!(Identifier("n".to_string())))
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(5),
                            node: Box::new(node::new!(Identifier("acc".to_string())))
                        })
                    ],
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(RecursiveCall(Box::new(node::new!(
                            Call {
                                function: Box::new(node::new!(Identifier("factorial".to_string()))),
                                arguments: vec![
                                    node::new!(BinaryOp {
                                        op: BinaryOp::Subtract,
                                        lhs: Box::new(node::new!(Identifier("n".to_string()))),
                                        rhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                                    }),
                                    node::new!(BinaryOp {
                                        op: BinaryOp::Multiply,
                                        lhs: Box::new(node::new!(Identifier("n".to_string()))),
                                        rhs: Box::new(node::new!(Identifier("acc".to_string())))
                                    }),
                                ]
                            }
                        ))))))
                    ))),
                }
            ]
        })]))
    );
}

#[test]
#[should_panic]
fn test_panic_on_keyword_as_identifier() {
    parse!("let fn = 1;");
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
            name: "x".to_string(),
            value: Box::new(node::new!(IndexExpression {
                base: Box::new(node::new!(Identifier("foo".to_string()))),
                index: Box::new(node::new!(Literal(Literal::Integer(1)))),
            }))
        })]))
    );

    let program = parse!("let x = foo[1][2];");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            name: "x".to_string(),
            value: Box::new(node::new!(IndexExpression {
                base: Box::new(node::new!(IndexExpression {
                    base: Box::new(node::new!(Identifier("foo".to_string()))),
                    index: Box::new(node::new!(Literal(Literal::Integer(1)))),
                })),
                index: Box::new(node::new!(Literal(Literal::Integer(2)))),
            }))
        })]))
    );
}
