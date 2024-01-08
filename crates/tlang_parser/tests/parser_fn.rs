use indoc::indoc;
use pretty_assertions::assert_eq;

use tlang_ast::{
    node::{self, BinaryOp, FunctionDeclaration, PrefixOp},
    symbols::SymbolId,
    token::Literal,
};

mod common;

#[test]
fn test_function_declaration() {
    let program = parse!("fn foo() { bar(); 1 + 2; }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclaration {
            id: SymbolId::new(1),
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![],
                guard: None,
                return_type_annotation: None,
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
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![
                    node::new!(FunctionParameter {
                        id: SymbolId::new(1),
                        node: Box::new(node::new!(Identifier("x".to_string()))),
                        type_annotation: None,
                    }),
                    node::new!(FunctionParameter {
                        id: SymbolId::new(2),
                        node: Box::new(node::new!(Identifier("y".to_string()))),
                        type_annotation: None,
                    }),
                ],
                guard: None,
                return_type_annotation: None,
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
fn test_nameless_function_expressions() {
    let program = parse!("let x = fn() { 1 + 2; };");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(2),
            pattern: Box::new(node::new!(Identifier("x".to_string()))),
            expression: Box::new(node::new!(FunctionExpression {
                id: SymbolId::new(1),
                name: None,
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![],
                    guard: None,
                    return_type_annotation: None,
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
            })),
            type_annotation: None,
        })]))
    );

    let program = parse!("let x = fn(x, y) { 1 + 2; };");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(4),
            pattern: Box::new(node::new!(Identifier("x".to_string()))),
            expression: Box::new(node::new!(FunctionExpression {
                id: SymbolId::new(3),
                name: None,
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(1),
                            node: Box::new(node::new!(Identifier("x".to_string()))),
                            type_annotation: None,
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            node: Box::new(node::new!(Identifier("y".to_string()))),
                            type_annotation: None,
                        }),
                    ],
                    guard: None,
                    return_type_annotation: None,
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
            })),
            type_annotation: None,
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
            pattern: Box::new(node::new!(Identifier("x".to_string()))),
            expression: Box::new(node::new!(FunctionExpression {
                id: SymbolId::new(1),
                name: None,
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![],
                    guard: None,
                    return_type_annotation: None,
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
            })),
            type_annotation: None,
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
            pattern: Box::new(node::new!(Identifier("x".to_string()))),
            expression: Box::new(node::new!(FunctionExpression {
                id: SymbolId::new(1),
                name: Some(Box::new(node::new!(Identifier("foo".to_string())))),
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![],
                    guard: None,
                    return_type_annotation: None,
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
            })),
            type_annotation: None,
        })]))
    );

    let program = parse!("let x = fn foo(x, y) { 1 + 2; };");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(4),
            pattern: Box::new(node::new!(Identifier("x".to_string()))),
            expression: Box::new(node::new!(FunctionExpression {
                id: SymbolId::new(3),
                name: Some(Box::new(node::new!(Identifier("foo".to_string())))),
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(1),
                            node: Box::new(node::new!(Identifier("x".to_string()))),
                            type_annotation: None,
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            node: Box::new(node::new!(Identifier("y".to_string()))),
                            type_annotation: None,
                        }),
                    ],
                    guard: None,
                    return_type_annotation: None,
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
            })),
            type_annotation: None,
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
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![],
                guard: None,
                return_type_annotation: None,
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
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![],
                guard: None,
                return_type_annotation: None,
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
            pattern: Box::new(node::new!(Identifier("x".to_string()))),
            expression: Box::new(node::new!(FunctionExpression {
                id: SymbolId::new(1),
                name: None,
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![],
                    guard: None,
                    return_type_annotation: None,
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
            })),
            type_annotation: None,
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
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![],
                guard: None,
                return_type_annotation: None,
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
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![],
                guard: None,
                return_type_annotation: None,
                body: Box::new(node::new!(Block(vec![], None)))
            })
        })]))
    );

    let program = parse!("let x = fn() { 1 + 2 };");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(2),
            pattern: Box::new(node::new!(Identifier("x".to_string()))),
            expression: Box::new(node::new!(FunctionExpression {
                id: SymbolId::new(1),
                name: None,
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![],
                    guard: None,
                    return_type_annotation: None,
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                            rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                        })))
                    )))
                })
            })),
            type_annotation: None,
        })]))
    );

    let program = parse!("fn foo() { let x = 1; x }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclaration {
            id: SymbolId::new(2),
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![],
                guard: None,
                return_type_annotation: None,
                body: Box::new(node::new!(Block(
                    vec![node::new!(VariableDeclaration {
                        id: SymbolId::new(1),
                        pattern: Box::new(node::new!(Identifier("x".to_string()))),
                        expression: Box::new(node::new!(Literal(Literal::Integer(1)))),
                        type_annotation: None,
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
            name: Box::new(node::new!(Identifier("factorial".to_string()))),
            declarations: vec![
                FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(1),
                        node: Box::new(node::new!(Literal(Literal::Integer(0)))),
                        type_annotation: None,
                    })],
                    guard: None,
                    return_type_annotation: None,
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
                        node: Box::new(node::new!(Identifier("n".to_string()))),
                        type_annotation: None,
                    })],
                    guard: None,
                    return_type_annotation: None,
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
            name: Box::new(node::new!(Identifier("sum".to_string()))),
            declarations: vec![
                FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(1),
                        node: Box::new(node::new!(List(vec![]))),
                        type_annotation: None,
                    })],
                    guard: None,
                    return_type_annotation: None,
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
                        ]))),
                        type_annotation: None,
                    })],
                    guard: None,
                    return_type_annotation: None,
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
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declarations: vec![
                FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(2),
                        node: Box::new(node::new!(Literal(Literal::Integer(1)))),
                        type_annotation: None,
                    })],
                    guard: None,
                    return_type_annotation: None,
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(Literal(Literal::Integer(1))))),
                    )))
                },
                FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(3),
                        node: Box::new(node::new!(Identifier("n".to_string()))),
                        type_annotation: None,
                    })],
                    guard: None,
                    return_type_annotation: None,
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
            name: Box::new(node::new!(Identifier("factorial".to_string()))),
            declarations: vec![
                FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(1),
                        node: Box::new(node::new!(Identifier("n".to_string()))),
                        type_annotation: None,
                    })],
                    guard: None,
                    return_type_annotation: None,
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
                            node: Box::new(node::new!(Literal(Literal::Integer(0)))),
                            type_annotation: None,
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(3),
                            node: Box::new(node::new!(Identifier("acc".to_string()))),
                            type_annotation: None,
                        })
                    ],
                    guard: None,
                    return_type_annotation: None,
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(Identifier("acc".to_string()))))
                    ))),
                },
                FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(4),
                            node: Box::new(node::new!(Identifier("n".to_string()))),
                            type_annotation: None,
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(5),
                            node: Box::new(node::new!(Identifier("acc".to_string()))),
                            type_annotation: None,
                        })
                    ],
                    guard: None,
                    return_type_annotation: None,
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
fn test_foldl_impl() {
    let program = parse!(indoc! {"
        fn foldl([], acc, _) { acc }
        fn foldl([x, ...xs], acc, f) { rec foldl(xs, f(acc, x), f) }
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclarations {
            id: SymbolId::new(7),
            name: Box::new(node::new!(Identifier("foldl".to_string()))),
            declarations: vec![
                FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(1),
                            node: Box::new(node::new!(List(vec![]))),
                            type_annotation: None,
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            node: Box::new(node::new!(Identifier("acc".to_string()))),
                            type_annotation: None,
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(3),
                            node: Box::new(node::new!(Wildcard)),
                            type_annotation: None,
                        })
                    ],
                    guard: None,
                    return_type_annotation: None,
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(Identifier("acc".to_string()))))
                    ))),
                },
                FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(4),
                            node: Box::new(node::new!(List(vec![
                                node::new!(Identifier("x".to_string())),
                                node::new!(PrefixOp(
                                    PrefixOp::Rest,
                                    Box::new(node::new!(Identifier("xs".to_string())))
                                ))
                            ]))),
                            type_annotation: None,
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(5),
                            node: Box::new(node::new!(Identifier("acc".to_string()))),
                            type_annotation: None,
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(6),
                            node: Box::new(node::new!(Identifier("f".to_string()))),
                            type_annotation: None,
                        })
                    ],
                    guard: None,
                    return_type_annotation: None,
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(RecursiveCall(Box::new(node::new!(
                            Call {
                                function: Box::new(node::new!(Identifier("foldl".to_string()))),
                                arguments: vec![
                                    node::new!(Identifier("xs".to_string())),
                                    node::new!(Call {
                                        function: Box::new(node::new!(Identifier("f".to_string()))),
                                        arguments: vec![
                                            node::new!(Identifier("acc".to_string())),
                                            node::new!(Identifier("x".to_string()))
                                        ]
                                    }),
                                    node::new!(Identifier("f".to_string()))
                                ]
                            }
                        ))))))
                    )))
                },
            ],
        }),])),
    );
}

#[test]
fn test_function_declarations_with_guard() {
    let program = parse!(indoc! {"
        fn filter([], f) { [] }
        fn filter([x, ...xs], f) if f(x) { [x, ...filter(xs, f)] }
        fn filter([x, ...xs], f) { rec filter(xs, f) }
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclarations {
            id: SymbolId::new(7),
            name: Box::new(node::new!(Identifier("filter".to_string()))),
            declarations: vec![
                FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(1),
                            node: Box::new(node::new!(List(vec![]))),
                            type_annotation: None,
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            node: Box::new(node::new!(Identifier("f".to_string()))),
                            type_annotation: None,
                        })
                    ],
                    guard: None,
                    return_type_annotation: None,
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(List(vec![]))))
                    )))
                },
                FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(3),
                            node: Box::new(node::new!(List(vec![
                                node::new!(Identifier("x".to_string())),
                                node::new!(PrefixOp(
                                    PrefixOp::Rest,
                                    Box::new(node::new!(Identifier("xs".to_string())))
                                ))
                            ]))),
                            type_annotation: None,
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(4),
                            node: Box::new(node::new!(Identifier("f".to_string()))),
                            type_annotation: None,
                        })
                    ],
                    guard: Some(Box::new(node::new!(Call {
                        function: Box::new(node::new!(Identifier("f".to_string()))),
                        arguments: vec![node::new!(Identifier("x".to_string()))]
                    }))),
                    return_type_annotation: None,
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(List(vec![
                            node::new!(Identifier("x".to_string())),
                            node::new!(PrefixOp(
                                PrefixOp::Spread,
                                Box::new(node::new!(Call {
                                    function: Box::new(node::new!(Identifier(
                                        "filter".to_string()
                                    ))),
                                    arguments: vec![
                                        node::new!(Identifier("xs".to_string())),
                                        node::new!(Identifier("f".to_string()))
                                    ]
                                }))
                            ))
                        ]))))
                    )))
                },
                FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(5),
                            node: Box::new(node::new!(List(vec![
                                node::new!(Identifier("x".to_string())),
                                node::new!(PrefixOp(
                                    PrefixOp::Rest,
                                    Box::new(node::new!(Identifier("xs".to_string())))
                                ))
                            ]))),
                            type_annotation: None,
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(6),
                            node: Box::new(node::new!(Identifier("f".to_string()))),
                            type_annotation: None,
                        })
                    ],
                    guard: None,
                    return_type_annotation: None,
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(RecursiveCall(Box::new(node::new!(
                            Call {
                                function: Box::new(node::new!(Identifier("filter".to_string()))),
                                arguments: vec![
                                    node::new!(Identifier("xs".to_string())),
                                    node::new!(Identifier("f".to_string()))
                                ]
                            }
                        ))))))
                    )))
                },
            ],
        })])),
    );
}

#[test]
fn test_function_declarations_with_let_guard() {
    let program = parse!(indoc! {"
        fn filter_map([], f) { [] }
        fn filter_map([x, ...xs], f) if let y = f(x) { [y, ...filter_map(xs, f)] }
        fn filter_map([x, ...xs], f) { rec filter_map(xs, f) }
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclarations {
            id: SymbolId::new(8),
            name: Box::new(node::new!(Identifier("filter_map".to_string()))),
            declarations: vec![
                FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(1),
                            node: Box::new(node::new!(List(vec![]))),
                            type_annotation: None,
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            node: Box::new(node::new!(Identifier("f".to_string()))),
                            type_annotation: None,
                        })
                    ],
                    guard: None,
                    return_type_annotation: None,
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(List(vec![]))))
                    )))
                },
                FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(3),
                            node: Box::new(node::new!(List(vec![
                                node::new!(Identifier("x".to_string())),
                                node::new!(PrefixOp(
                                    PrefixOp::Rest,
                                    Box::new(node::new!(Identifier("xs".to_string())))
                                ))
                            ]))),
                            type_annotation: None,
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(4),
                            node: Box::new(node::new!(Identifier("f".to_string()))),
                            type_annotation: None,
                        })
                    ],
                    guard: Some(Box::new(node::new!(VariableDeclaration {
                        id: SymbolId::new(5),
                        pattern: Box::new(node::new!(Identifier("y".to_string()))),
                        expression: Box::new(node::new!(Call {
                            function: Box::new(node::new!(Identifier("f".to_string()))),
                            arguments: vec![node::new!(Identifier("x".to_string()))]
                        })),
                        type_annotation: None
                    }))),
                    return_type_annotation: None,
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(List(vec![
                            node::new!(Identifier("y".to_string())),
                            node::new!(PrefixOp(
                                PrefixOp::Spread,
                                Box::new(node::new!(Call {
                                    function: Box::new(node::new!(Identifier(
                                        "filter_map".to_string()
                                    ))),
                                    arguments: vec![
                                        node::new!(Identifier("xs".to_string())),
                                        node::new!(Identifier("f".to_string()))
                                    ]
                                }))
                            ))
                        ]))))
                    )))
                },
                FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(6),
                            node: Box::new(node::new!(List(vec![
                                node::new!(Identifier("x".to_string())),
                                node::new!(PrefixOp(
                                    PrefixOp::Rest,
                                    Box::new(node::new!(Identifier("xs".to_string())))
                                ))
                            ]))),
                            type_annotation: None,
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(7),
                            node: Box::new(node::new!(Identifier("f".to_string()))),
                            type_annotation: None,
                        })
                    ],
                    guard: None,
                    return_type_annotation: None,
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(RecursiveCall(Box::new(node::new!(
                            Call {
                                function: Box::new(node::new!(Identifier(
                                    "filter_map".to_string()
                                ))),
                                arguments: vec![
                                    node::new!(Identifier("xs".to_string())),
                                    node::new!(Identifier("f".to_string()))
                                ]
                            }
                        ))))))
                    )))
                },
            ],
        })])),
    );
}
