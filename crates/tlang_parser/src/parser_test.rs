use indoc::indoc;

use crate::lexer::Literal;
use crate::parser::{BinaryOp, Node, Parser, PrefixOp};

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
        Node::Program(vec![Node::VariableDeclaration {
            name: "x".to_string(),
            value: Box::new(Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }),
        }])
    );
}

#[test]
fn test_simple_arithmetic_calculations() {
    let program = parse!("1 + 2 + 3;");

    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
            op: BinaryOp::Add,
            lhs: Box::new(Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }),
            rhs: Box::new(Node::Literal(Literal::Integer(3))),
        }))])
    );

    let program = parse!("1 * 2 + 3;");

    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
            op: BinaryOp::Add,
            lhs: Box::new(Node::BinaryOp {
                op: BinaryOp::Multiply,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }),
            rhs: Box::new(Node::Literal(Literal::Integer(3))),
        }))])
    );
}

#[test]
fn test_simple_arithmetic_sum_mult_precedence() {
    let program = parse!("1 + 2 * 3;");

    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
            op: BinaryOp::Add,
            lhs: Box::new(Node::Literal(Literal::Integer(1))),
            rhs: Box::new(Node::BinaryOp {
                op: BinaryOp::Multiply,
                lhs: Box::new(Node::Literal(Literal::Integer(2))),
                rhs: Box::new(Node::Literal(Literal::Integer(3))),
            }),
        }))])
    );
}

#[test]
fn test_simple_arithmetic_sum_mult_precedence_parentheses() {
    let program = parse!("(1 + 2) * 3;");

    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
            op: BinaryOp::Multiply,
            lhs: Box::new(Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }),
            rhs: Box::new(Node::Literal(Literal::Integer(3))),
        }))])
    );
}

#[test]
fn test_simple_arithmetic_with_identifiers() {
    let program = parse!("let x = 1; let y = 2; x + y;");

    assert_eq!(
        program,
        Node::Program(vec![
            Node::VariableDeclaration {
                name: "x".to_string(),
                value: Box::new(Node::Literal(Literal::Integer(1))),
            },
            Node::VariableDeclaration {
                name: "y".to_string(),
                value: Box::new(Node::Literal(Literal::Integer(2))),
            },
            Node::ExpressionStatement(Box::new(Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Identifier("x".to_string())),
                rhs: Box::new(Node::Identifier("y".to_string())),
            }))
        ])
    );
}

#[test]
fn test_simple_call() {
    let program = parse!("foo(1, 2);");

    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::Call {
            function: Box::new(Node::Identifier("foo".to_string())),
            arguments: vec![
                Node::Literal(Literal::Integer(1)),
                Node::Literal(Literal::Integer(2))
            ],
        }))])
    );
}

#[test]
fn test_nested_call() {
    let program = parse!("foo(bar(1), 2);");

    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::Call {
            function: Box::new(Node::Identifier("foo".to_string())),
            arguments: vec![
                Node::Call {
                    function: Box::new(Node::Identifier("bar".to_string())),
                    arguments: vec![Node::Literal(Literal::Integer(1))]
                },
                Node::Literal(Literal::Integer(2))
            ],
        }))])
    );
}

#[test]
fn test_call_with_expression() {
    let program = parse!("foo(1 + 2, 3);");

    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::Call {
            function: Box::new(Node::Identifier("foo".to_string())),
            arguments: vec![
                Node::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(Node::Literal(Literal::Integer(1))),
                    rhs: Box::new(Node::Literal(Literal::Integer(2))),
                },
                Node::Literal(Literal::Integer(3))
            ],
        }))])
    );
}

#[test]
fn test_block_expression() {
    let program = parse!("let x = { 1 + 2; };");

    assert_eq!(
        program,
        Node::Program(vec![Node::VariableDeclaration {
            name: "x".to_string(),
            value: Box::new(Node::Block(
                vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(Node::Literal(Literal::Integer(1))),
                    rhs: Box::new(Node::Literal(Literal::Integer(2))),
                }))],
                None
            ))
        }])
    );
}

#[test]
fn test_if_statement() {
    let program = parse!("if (1 + 2) { 3 + 4; }");

    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::IfElse {
            condition: Box::new(Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }),
            then_branch: Box::new(Node::Block(
                vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(Node::Literal(Literal::Integer(3))),
                    rhs: Box::new(Node::Literal(Literal::Integer(4))),
                }))],
                None
            )),
            else_branch: None,
        }))])
    );
}

#[test]
fn test_if_else_statement() {
    let program = parse!("if true { 1; } else { 2; }");
    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::IfElse {
            condition: Box::new(Node::Literal(Literal::Boolean(true))),
            then_branch: Box::new(Node::Block(
                vec![Node::ExpressionStatement(Box::new(Node::Literal(
                    Literal::Integer(1)
                )))],
                None
            )),
            else_branch: Some(Box::new(Node::Block(
                vec![Node::ExpressionStatement(Box::new(Node::Literal(
                    Literal::Integer(2)
                )))],
                None
            ))),
        }))])
    );

    let program = parse!("if (1 + 2) { 3 + 4; } else { 5 + 6; }");

    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::IfElse {
            condition: Box::new(Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }),
            then_branch: Box::new(Node::Block(
                vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(Node::Literal(Literal::Integer(3))),
                    rhs: Box::new(Node::Literal(Literal::Integer(4))),
                }))],
                None
            )),
            else_branch: Some(Box::new(Node::Block(
                vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(Node::Literal(Literal::Integer(5))),
                    rhs: Box::new(Node::Literal(Literal::Integer(6))),
                }))],
                None
            ))),
        }))])
    );
}

#[test]
fn test_if_else_as_last_expression() {
    let program = parse!("fn main() { if true { 1 } else { 2 } }");

    assert_eq!(
        program,
        Node::Program(vec![Node::FunctionDeclaration {
            name: "main".to_string(),
            parameters: vec![],
            body: Box::new(Node::Block(
                vec![],
                Some(Box::new(Node::IfElse {
                    condition: Box::new(Node::Literal(Literal::Boolean(true))),
                    then_branch: Box::new(Node::Block(
                        vec![],
                        Some(Box::new(Node::Literal(Literal::Integer(1))))
                    )),
                    else_branch: Some(Box::new(Node::Block(
                        vec![],
                        Some(Box::new(Node::Literal(Literal::Integer(2))))
                    ))),
                }))
            ))
        }])
    );
}

#[test]
fn test_if_expression() {
    let program = parse!("let x = if (true) { 1; } else { 2; };");

    assert_eq!(
        program,
        Node::Program(vec![Node::VariableDeclaration {
            name: "x".to_string(),
            value: Box::new(Node::IfElse {
                condition: Box::new(Node::Literal(Literal::Boolean(true))),
                then_branch: Box::new(Node::Block(
                    vec![Node::ExpressionStatement(Box::new(Node::Literal(
                        Literal::Integer(1)
                    )))],
                    None
                )),
                else_branch: Some(Box::new(Node::Block(
                    vec![Node::ExpressionStatement(Box::new(Node::Literal(
                        Literal::Integer(2)
                    )))],
                    None
                ))),
            })
        }])
    );
}

#[test]
fn test_function_declaration() {
    let program = parse!("fn foo() { bar(); 1 + 2; }");

    assert_eq!(
        program,
        Node::Program(vec![Node::FunctionDeclaration {
            name: "foo".to_string(),
            parameters: vec![],
            body: Box::new(Node::Block(
                vec![
                    Node::ExpressionStatement(Box::new(Node::Call {
                        function: Box::new(Node::Identifier("bar".to_string())),
                        arguments: vec![]
                    })),
                    Node::ExpressionStatement(Box::new(Node::BinaryOp {
                        op: BinaryOp::Add,
                        lhs: Box::new(Node::Literal(Literal::Integer(1))),
                        rhs: Box::new(Node::Literal(Literal::Integer(2))),
                    }))
                ],
                None
            ))
        }])
    );
}

#[test]
fn test_function_declaration_with_parameters() {
    let program = parse!("fn foo(x, y) { 1 + 2; }");

    assert_eq!(
        program,
        Node::Program(vec![Node::FunctionDeclaration {
            name: "foo".to_string(),
            parameters: vec![
                Node::FunctionParameter(Box::new(Node::Identifier("x".to_string()))),
                Node::FunctionParameter(Box::new(Node::Identifier("y".to_string())))
            ],
            body: Box::new(Node::Block(
                vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(Node::Literal(Literal::Integer(1))),
                    rhs: Box::new(Node::Literal(Literal::Integer(2))),
                }))],
                None
            ))
        }])
    );
}

#[test]
fn test_stray_semicolon() {
    let program = parse!("1 + 2;;");

    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
            op: BinaryOp::Add,
            lhs: Box::new(Node::Literal(Literal::Integer(1))),
            rhs: Box::new(Node::Literal(Literal::Integer(2))),
        }))])
    );
}

#[test]
fn test_exponentiation() {
    let program = parse!("1 * 2 ** 3;");

    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
            op: BinaryOp::Multiply,
            lhs: Box::new(Node::Literal(Literal::Integer(1))),
            rhs: Box::new(Node::BinaryOp {
                op: BinaryOp::Exponentiation,
                lhs: Box::new(Node::Literal(Literal::Integer(2))),
                rhs: Box::new(Node::Literal(Literal::Integer(3))),
            }),
        }))])
    );

    let program = parse!("1 ** 2 * 3;");

    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
            op: BinaryOp::Multiply,
            lhs: Box::new(Node::BinaryOp {
                op: BinaryOp::Exponentiation,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }),
            rhs: Box::new(Node::Literal(Literal::Integer(3))),
        }))])
    );

    let program = parse!("1 ** 2 ** 3;");

    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
            op: BinaryOp::Exponentiation,
            lhs: Box::new(Node::Literal(Literal::Integer(1))),
            rhs: Box::new(Node::BinaryOp {
                op: BinaryOp::Exponentiation,
                lhs: Box::new(Node::Literal(Literal::Integer(2))),
                rhs: Box::new(Node::Literal(Literal::Integer(3))),
            }),
        }))])
    );
}

#[test]
fn test_nameless_function_expressions() {
    let program = parse!("let x = fn() { 1 + 2; };");

    assert_eq!(
        program,
        Node::Program(vec![Node::VariableDeclaration {
            name: "x".to_string(),
            value: Box::new(Node::FunctionExpression {
                name: None,
                parameters: vec![],
                body: Box::new(Node::Block(
                    vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
                        op: BinaryOp::Add,
                        lhs: Box::new(Node::Literal(Literal::Integer(1))),
                        rhs: Box::new(Node::Literal(Literal::Integer(2))),
                    }))],
                    None
                ))
            })
        }])
    );

    let program = parse!("let x = fn(x, y) { 1 + 2; };");

    assert_eq!(
        program,
        Node::Program(vec![Node::VariableDeclaration {
            name: "x".to_string(),
            value: Box::new(Node::FunctionExpression {
                name: None,
                parameters: vec![
                    Node::FunctionParameter(Box::new(Node::Identifier("x".to_string()))),
                    Node::FunctionParameter(Box::new(Node::Identifier("y".to_string())))
                ],
                body: Box::new(Node::Block(
                    vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
                        op: BinaryOp::Add,
                        lhs: Box::new(Node::Literal(Literal::Integer(1))),
                        rhs: Box::new(Node::Literal(Literal::Integer(2))),
                    }))],
                    None
                ))
            })
        }])
    );
}

#[test]
fn test_function_expression_without_name_no_argument_parenthesis() {
    let program = parse!("let x = fn { 1 + 2; };");
    assert_eq!(
        program,
        Node::Program(vec![Node::VariableDeclaration {
            name: "x".to_string(),
            value: Box::new(Node::FunctionExpression {
                name: None,
                parameters: vec![],
                body: Box::new(Node::Block(
                    vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
                        op: BinaryOp::Add,
                        lhs: Box::new(Node::Literal(Literal::Integer(1))),
                        rhs: Box::new(Node::Literal(Literal::Integer(2))),
                    }))],
                    None
                ))
            })
        }])
    );
}

#[test]
fn test_function_expressions() {
    let program = parse!("let x = fn foo() { 1 + 2; };");
    assert_eq!(
        program,
        Node::Program(vec![Node::VariableDeclaration {
            name: "x".to_string(),
            value: Box::new(Node::FunctionExpression {
                name: Some("foo".to_string()),
                parameters: vec![],
                body: Box::new(Node::Block(
                    vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
                        op: BinaryOp::Add,
                        lhs: Box::new(Node::Literal(Literal::Integer(1))),
                        rhs: Box::new(Node::Literal(Literal::Integer(2))),
                    }))],
                    None
                ))
            })
        }])
    );
    let program = parse!("let x = fn foo(x, y) { 1 + 2; };");
    assert_eq!(
        program,
        Node::Program(vec![Node::VariableDeclaration {
            name: "x".to_string(),
            value: Box::new(Node::FunctionExpression {
                name: Some("foo".to_string()),
                parameters: vec![
                    Node::FunctionParameter(Box::new(Node::Identifier("x".to_string()))),
                    Node::FunctionParameter(Box::new(Node::Identifier("y".to_string())))
                ],
                body: Box::new(Node::Block(
                    vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
                        op: BinaryOp::Add,
                        lhs: Box::new(Node::Literal(Literal::Integer(1))),
                        rhs: Box::new(Node::Literal(Literal::Integer(2))),
                    }))],
                    None
                ))
            })
        }])
    );
}

#[test]
fn test_pattern_matching() {
    let program = parse!("let x = match 1 { 1 => 2, 3 => 4, _ => 5 };");

    assert_eq!(
        program,
        Node::Program(vec![Node::VariableDeclaration {
            name: "x".to_string(),
            value: Box::new(Node::Match {
                expression: Box::new(Node::Literal(Literal::Integer(1))),
                arms: vec![
                    Node::MatchArm {
                        pattern: Box::new(Node::Literal(Literal::Integer(1))),
                        expression: Box::new(Node::Literal(Literal::Integer(2)))
                    },
                    Node::MatchArm {
                        pattern: Box::new(Node::Literal(Literal::Integer(3))),
                        expression: Box::new(Node::Literal(Literal::Integer(4)))
                    },
                    Node::MatchArm {
                        pattern: Box::new(Node::Wildcard),
                        expression: Box::new(Node::Literal(Literal::Integer(5)))
                    }
                ]
            })
        }])
    );
}

#[test]
fn test_explicit_return_statements() {
    let program = parse!("fn foo() { return 1 + 2; }");

    assert_eq!(
        program,
        Node::Program(vec![Node::FunctionDeclaration {
            name: "foo".to_string(),
            parameters: vec![],
            body: Box::new(Node::Block(
                vec![Node::ReturnStatement(Some(Box::new(Node::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(Node::Literal(Literal::Integer(1))),
                    rhs: Box::new(Node::Literal(Literal::Integer(2))),
                })))],
                None
            ))
        }])
    );

    let program = parse!("fn foo() { return; }");

    assert_eq!(
        program,
        Node::Program(vec![Node::FunctionDeclaration {
            name: "foo".to_string(),
            parameters: vec![],
            body: Box::new(Node::Block(vec![Node::ReturnStatement(None)], None))
        }])
    );

    let program = parse!("let x = fn() { return 1 + 2; };");

    assert_eq!(
        program,
        Node::Program(vec![Node::VariableDeclaration {
            name: "x".to_string(),
            value: Box::new(Node::FunctionExpression {
                name: None,
                parameters: vec![],
                body: Box::new(Node::Block(
                    vec![Node::ReturnStatement(Some(Box::new(Node::BinaryOp {
                        op: BinaryOp::Add,
                        lhs: Box::new(Node::Literal(Literal::Integer(1))),
                        rhs: Box::new(Node::Literal(Literal::Integer(2))),
                    })))],
                    None
                ))
            })
        }])
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
        Node::Program(vec![Node::FunctionDeclarations(
            "factorial".to_string(),
            vec![
                (
                    vec![Node::FunctionParameter(Box::new(Node::Literal(
                        Literal::Integer(0)
                    )))],
                    Box::new(Node::Block(
                        vec![Node::ReturnStatement(Some(Box::new(Node::Literal(
                            Literal::Integer(1)
                        ))))],
                        None
                    ))
                ),
                (
                    vec![Node::FunctionParameter(Box::new(Node::Identifier(
                        "n".to_string()
                    )))],
                    Box::new(Node::Block(
                        vec![Node::ReturnStatement(Some(Box::new(Node::BinaryOp {
                            op: BinaryOp::Multiply,
                            lhs: Box::new(Node::Identifier("n".to_string())),
                            rhs: Box::new(Node::Call {
                                function: Box::new(Node::Identifier("factorial".to_string())),
                                arguments: vec![Node::BinaryOp {
                                    op: BinaryOp::Subtract,
                                    lhs: Box::new(Node::Identifier("n".to_string())),
                                    rhs: Box::new(Node::Literal(Literal::Integer(1)))
                                }]
                            })
                        })))],
                        None
                    ))
                )
            ]
        )])
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
        Node::Program(vec![Node::FunctionDeclarations(
            "sum".to_string(),
            vec![
                (
                    vec![Node::FunctionParameter(Box::new(Node::List(vec![])))],
                    Box::new(Node::Block(
                        vec![],
                        Some(Box::new(Node::Literal(Literal::Integer(0)))),
                    ))
                ),
                (
                    vec![Node::FunctionParameter(Box::new(Node::List(vec![
                        Node::Identifier("x".to_string()),
                        Node::PrefixOp(
                            PrefixOp::Rest,
                            Box::new(Node::Identifier("xs".to_string()))
                        )
                    ])))],
                    Box::new(Node::Block(
                        vec![],
                        Some(Box::new(Node::BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(Node::Identifier("x".to_string())),
                            rhs: Box::new(Node::Call {
                                function: Box::new(Node::Identifier("sum".to_string())),
                                arguments: vec![Node::Identifier("xs".to_string())]
                            })
                        }))
                    ))
                )
            ]
        )])
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
        Node::Program(vec![Node::FunctionDeclarations(
            "foo".to_string(),
            vec![
                (
                    vec![Node::FunctionParameter(Box::new(Node::Literal(
                        Literal::Integer(1)
                    )))],
                    Box::new(Node::Block(
                        vec![],
                        Some(Box::new(Node::Literal(Literal::Integer(1)))),
                    ))
                ),
                (
                    vec![Node::FunctionParameter(Box::new(Node::Identifier(
                        "n".to_string()
                    )))],
                    Box::new(Node::Block(
                        vec![],
                        Some(Box::new(Node::BinaryOp {
                            op: BinaryOp::Multiply,
                            lhs: Box::new(Node::Identifier("n".to_string())),
                            rhs: Box::new(Node::Literal(Literal::Integer(2)))
                        }))
                    ))
                )
            ]
        )])
    );
}

#[test]
fn test_pipeline_operator_to_identifier() {
    let program = parse!("1 |> foo;");

    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
            op: BinaryOp::Pipeline,
            lhs: Box::new(Node::Literal(Literal::Integer(1))),
            rhs: Box::new(Node::Identifier("foo".to_string())),
        }))])
    );
}

#[test]
fn test_pipeline_operator() {
    let program = parse!("1 |> foo();");
    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
            op: BinaryOp::Pipeline,
            lhs: Box::new(Node::Literal(Literal::Integer(1))),
            rhs: Box::new(Node::Call {
                function: Box::new(Node::Identifier("foo".to_string())),
                arguments: vec![]
            }),
        }))]),
    );

    let program = parse!("1 |> foo() |> bar();");
    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
            op: BinaryOp::Pipeline,
            lhs: Box::new(Node::BinaryOp {
                op: BinaryOp::Pipeline,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Call {
                    function: Box::new(Node::Identifier("foo".to_string())),
                    arguments: vec![]
                }),
            }),
            rhs: Box::new(Node::Call {
                function: Box::new(Node::Identifier("bar".to_string())),
                arguments: vec![]
            }),
        }))]),
    );
}

#[test]
fn test_pipeline_operator_precedence() {
    let program = parse!("1 + 2 |> foo();");
    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
            op: BinaryOp::Pipeline,
            lhs: Box::new(Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }),
            rhs: Box::new(Node::Call {
                function: Box::new(Node::Identifier("foo".to_string())),
                arguments: vec![]
            }),
        }))]),
    );
}

#[test]
fn test_pipeline_operator_to_function_call_with_arguments() {
    let program = parse!("1 |> foo(2);");
    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
            op: BinaryOp::Pipeline,
            lhs: Box::new(Node::Literal(Literal::Integer(1))),
            rhs: Box::new(Node::Call {
                function: Box::new(Node::Identifier("foo".to_string())),
                arguments: vec![Node::Literal(Literal::Integer(2))]
            }),
        }))]),
    );
}

#[test]
fn test_pipeline_operator_to_function_call_with_wildcards() {
    let program = parse!("1 |> foo(2, _);");
    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::BinaryOp {
            op: BinaryOp::Pipeline,
            lhs: Box::new(Node::Literal(Literal::Integer(1))),
            rhs: Box::new(Node::Call {
                function: Box::new(Node::Identifier("foo".to_string())),
                arguments: vec![Node::Literal(Literal::Integer(2)), Node::Wildcard,]
            }),
        }))]),
    );
}

#[test]
fn test_list_literal() {
    let program = parse!("let x = [];");

    assert_eq!(
        program,
        Node::Program(vec![Node::VariableDeclaration {
            name: "x".to_string(),
            value: Box::new(Node::List(vec![]))
        }])
    );

    let program = parse!("let x = [1, 2, 3];");

    assert_eq!(
        program,
        Node::Program(vec![Node::VariableDeclaration {
            name: "x".to_string(),
            value: Box::new(Node::List(vec![
                Node::Literal(Literal::Integer(1)),
                Node::Literal(Literal::Integer(2)),
                Node::Literal(Literal::Integer(3)),
            ]))
        }])
    );
}

#[test]
fn test_list_literal_with_trailing_comma() {
    let program = parse!("let x = [1, 2, 3,];");

    assert_eq!(
        program,
        Node::Program(vec![Node::VariableDeclaration {
            name: "x".to_string(),
            value: Box::new(Node::List(vec![
                Node::Literal(Literal::Integer(1)),
                Node::Literal(Literal::Integer(2)),
                Node::Literal(Literal::Integer(3)),
            ]))
        }])
    );
}

#[test]
fn test_single_line_comments() {
    let program = parse!("// this is a comment");
    assert_eq!(
        program,
        Node::Program(vec![Node::SingleLineComment(
            " this is a comment".to_string()
        )])
    );
}

#[test]
fn test_multi_line_comments() {
    let program = parse!("/* this is a comment */");

    assert_eq!(
        program,
        Node::Program(vec![Node::MultiLineComment(
            " this is a comment ".to_string()
        )])
    );

    let program = parse!("/* this is a comment\n   spanning multiple lines */");

    assert_eq!(
        program,
        Node::Program(vec![Node::MultiLineComment(
            " this is a comment\n   spanning multiple lines ".to_string()
        )])
    );
}

#[test]
fn test_string_literal() {
    let program = parse!(r#""foo";"#);

    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::Literal(
            Literal::String("foo".to_string())
        )))])
    );
}

#[test]
fn test_char_literal() {
    let program = parse!(r#"'a';"#);

    assert_eq!(
        program,
        Node::Program(vec![Node::ExpressionStatement(Box::new(Node::Literal(
            Literal::Char("a".to_string())
        )))])
    );
}
