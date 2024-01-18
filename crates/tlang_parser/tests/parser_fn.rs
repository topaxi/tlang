use indoc::indoc;
use pretty_assertions::assert_eq;

use tlang_ast::{
    node::{self, BinaryOp, FunctionDeclaration, UnaryOp},
    symbols::SymbolId,
    token::Literal,
};

mod common;

#[test]
fn test_function_declaration() {
    let program = parse!("fn foo() { bar(); 1 + 2; }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionSingleDeclaration {
            id: SymbolId::new(1),
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                parameters: vec![],
                guard: Box::new(None),
                return_type_annotation: Box::new(None),
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
                    Box::new(None)
                )))
            })))
        })]))
    );
}

#[test]
fn test_function_declaration_with_parameters() {
    let program = parse!("fn foo(x, y) { 1 + 2; }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionSingleDeclaration {
            id: SymbolId::new(3),
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                parameters: vec![
                    node::new!(FunctionParameter {
                        id: SymbolId::new(1),
                        pattern: Box::new(node::new!(Identifier("x".to_string()))),
                        type_annotation: Box::new(None),
                    }),
                    node::new!(FunctionParameter {
                        id: SymbolId::new(2),
                        pattern: Box::new(node::new!(Identifier("y".to_string()))),
                        type_annotation: Box::new(None),
                    }),
                ],
                guard: Box::new(None),
                return_type_annotation: Box::new(None),
                body: Box::new(node::new!(Block(
                    vec![node::new!(ExpressionStatement(Box::new(node::new!(
                        BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                            rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                        }
                    ))))],
                    Box::new(None)
                )))
            })))
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
                name: Box::new(None),
                declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![node::new!(ExpressionStatement(Box::new(node::new!(
                            BinaryOp {
                                op: BinaryOp::Add,
                                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                                rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                            }
                        ))))],
                        Box::new(None)
                    )))
                })))
            })),
            type_annotation: Box::new(None),
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
                name: Box::new(None),
                declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(1),
                            pattern: Box::new(node::new!(Identifier("x".to_string()))),
                            type_annotation: Box::new(None),
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            pattern: Box::new(node::new!(Identifier("y".to_string()))),
                            type_annotation: Box::new(None),
                        }),
                    ],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![node::new!(ExpressionStatement(Box::new(node::new!(
                            BinaryOp {
                                op: BinaryOp::Add,
                                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                                rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                            }
                        ))))],
                        Box::new(None)
                    )))
                })))
            })),
            type_annotation: Box::new(None),
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
                name: Box::new(None),
                declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![node::new!(ExpressionStatement(Box::new(node::new!(
                            BinaryOp {
                                op: BinaryOp::Add,
                                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                                rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                            }
                        ))))],
                        Box::new(None)
                    )))
                })))
            })),
            type_annotation: Box::new(None),
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
                name: Box::new(Some(node::new!(Identifier("foo".to_string())))),
                declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![node::new!(ExpressionStatement(Box::new(node::new!(
                            BinaryOp {
                                op: BinaryOp::Add,
                                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                                rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                            }
                        ))))],
                        Box::new(None)
                    )))
                })))
            })),
            type_annotation: Box::new(None),
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
                name: Box::new(Some(node::new!(Identifier("foo".to_string())))),
                declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(1),
                            pattern: Box::new(node::new!(Identifier("x".to_string()))),
                            type_annotation: Box::new(None),
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            pattern: Box::new(node::new!(Identifier("y".to_string()))),
                            type_annotation: Box::new(None),
                        }),
                    ],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![node::new!(ExpressionStatement(Box::new(node::new!(
                            BinaryOp {
                                op: BinaryOp::Add,
                                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                                rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                            }
                        ))))],
                        Box::new(None)
                    )))
                })))
            })),
            type_annotation: Box::new(None),
        })]))
    );
}

#[test]
fn test_explicit_return_statements() {
    let program = parse!("fn foo() { return 1 + 2; }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionSingleDeclaration {
            id: SymbolId::new(1),
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                parameters: vec![],
                guard: Box::new(None),
                return_type_annotation: Box::new(None),
                body: Box::new(node::new!(Block(
                    vec![node::new!(ReturnStatement(Box::new(Some(node::new!(
                        BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                            rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                        }
                    )))))],
                    Box::new(None)
                )))
            })))
        })]))
    );

    let program = parse!("fn foo() { return; }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionSingleDeclaration {
            id: SymbolId::new(1),
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                parameters: vec![],
                guard: Box::new(None),
                return_type_annotation: Box::new(None),
                body: Box::new(node::new!(Block(
                    vec![node::new!(ReturnStatement(Box::new(None)))],
                    Box::new(None)
                )))
            })))
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
                name: Box::new(None),
                declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![node::new!(ReturnStatement(Box::new(Some(node::new!(
                            BinaryOp {
                                op: BinaryOp::Add,
                                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                                rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                            }
                        )))))],
                        Box::new(None)
                    )))
                })))
            })),
            type_annotation: Box::new(None),
        })]))
    );
}

#[test]
fn test_implicit_return_expressions() {
    let program = parse!("fn foo() { 1 + 2 }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionSingleDeclaration {
            id: SymbolId::new(1),
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                parameters: vec![],
                guard: Box::new(None),
                return_type_annotation: Box::new(None),
                body: Box::new(node::new!(Block(
                    vec![],
                    Box::new(Some(node::new!(BinaryOp {
                        op: BinaryOp::Add,
                        lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                        rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                    })))
                )))
            })))
        })]))
    );

    let program = parse!("fn foo() {}");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionSingleDeclaration {
            id: SymbolId::new(1),
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                parameters: vec![],
                guard: Box::new(None),
                return_type_annotation: Box::new(None),
                body: Box::new(node::new!(Block(vec![], Box::new(None))))
            })))
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
                name: Box::new(None),
                declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![],
                        Box::new(Some(node::new!(BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                            rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
                        })))
                    )))
                })))
            })),
            type_annotation: Box::new(None),
        })]))
    );

    let program = parse!("fn foo() { let x = 1; x }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionSingleDeclaration {
            id: SymbolId::new(2),
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                parameters: vec![],
                guard: Box::new(None),
                return_type_annotation: Box::new(None),
                body: Box::new(node::new!(Block(
                    vec![node::new!(VariableDeclaration {
                        id: SymbolId::new(1),
                        pattern: Box::new(node::new!(Identifier("x".to_string()))),
                        expression: Box::new(node::new!(Literal(Literal::Integer(1)))),
                        type_annotation: Box::new(None),
                    })],
                    Box::new(Some(node::new!(Identifier("x".to_string()))))
                )))
            })))
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
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(1),
                        pattern: Box::new(node::new!(Literal(Literal::Integer(0)))),
                        type_annotation: Box::new(None),
                    })],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![node::new!(ReturnStatement(Box::new(Some(node::new!(
                            Literal(Literal::Integer(1))
                        )))))],
                        Box::new(None)
                    )))
                })),
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(2),
                        pattern: Box::new(node::new!(Identifier("n".to_string()))),
                        type_annotation: Box::new(None),
                    })],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![node::new!(ReturnStatement(Box::new(Some(node::new!(
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
                        Box::new(None)
                    )))
                }))
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
            id: SymbolId::new(5),
            name: Box::new(node::new!(Identifier("sum".to_string()))),
            declarations: vec![
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(1),
                        pattern: Box::new(node::new!(ListPattern(vec![]))),
                        type_annotation: Box::new(None),
                    })],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![],
                        Box::new(Some(node::new!(Literal(Literal::Integer(0))))),
                    )))
                })),
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(4),
                        pattern: Box::new(node::new!(ListPattern(vec![
                            node::new!(IdentifierPattern {
                                id: SymbolId::new(2),
                                name: "x".to_string()
                            }),
                            node::new!(UnaryOp(
                                UnaryOp::Rest,
                                Box::new(node::new!(IdentifierPattern {
                                    id: SymbolId::new(3),
                                    name: "xs".to_string()
                                }))
                            ))
                        ]))),
                        type_annotation: Box::new(None),
                    })],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![],
                        Box::new(Some(node::new!(BinaryOp {
                            op: BinaryOp::Add,
                            lhs: Box::new(node::new!(Identifier("x".to_string()))),
                            rhs: Box::new(node::new!(Call {
                                function: Box::new(node::new!(Identifier("sum".to_string()))),
                                arguments: vec![node::new!(Identifier("xs".to_string()))]
                            }))
                        })))
                    )))
                }))
            ]
        })]))
    )
}

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
            id: SymbolId::new(3),
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declarations: vec![
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(1),
                        pattern: Box::new(node::new!(Literal(Literal::Integer(1)))),
                        type_annotation: Box::new(None),
                    })],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![],
                        Box::new(Some(node::new!(Literal(Literal::Integer(1))))),
                    )))
                })),
                node::new!(SingleLineComment(" comment".to_string())),
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(2),
                        pattern: Box::new(node::new!(Identifier("n".to_string()))),
                        type_annotation: Box::new(None),
                    })],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![],
                        Box::new(Some(node::new!(BinaryOp {
                            op: BinaryOp::Multiply,
                            lhs: Box::new(node::new!(Identifier("n".to_string()))),
                            rhs: Box::new(node::new!(Literal(Literal::Integer(2))))
                        })))
                    )))
                }))
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
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![node::new!(FunctionParameter {
                        id: SymbolId::new(1),
                        pattern: Box::new(node::new!(Identifier("n".to_string()))),
                        type_annotation: Box::new(None),
                    })],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![],
                        Box::new(Some(node::new!(Call {
                            function: Box::new(node::new!(Identifier("factorial".to_string()))),
                            arguments: vec![
                                node::new!(Identifier("n".to_string())),
                                node::new!(Literal(Literal::Integer(1)))
                            ]
                        })))
                    ))),
                })),
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            pattern: Box::new(node::new!(Literal(Literal::Integer(0)))),
                            type_annotation: Box::new(None),
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(3),
                            pattern: Box::new(node::new!(Identifier("acc".to_string()))),
                            type_annotation: Box::new(None),
                        })
                    ],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![],
                        Box::new(Some(node::new!(Identifier("acc".to_string()))))
                    ))),
                })),
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(4),
                            pattern: Box::new(node::new!(Identifier("n".to_string()))),
                            type_annotation: Box::new(None),
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(5),
                            pattern: Box::new(node::new!(Identifier("acc".to_string()))),
                            type_annotation: Box::new(None),
                        })
                    ],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![],
                        Box::new(Some(node::new!(RecursiveCall(Box::new(node::new!(
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
                }))
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
            id: SymbolId::new(9),
            name: Box::new(node::new!(Identifier("foldl".to_string()))),
            declarations: vec![
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(1),
                            pattern: Box::new(node::new!(ListPattern(vec![]))),
                            type_annotation: Box::new(None),
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            pattern: Box::new(node::new!(Identifier("acc".to_string()))),
                            type_annotation: Box::new(None),
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(3),
                            pattern: Box::new(node::new!(Wildcard)),
                            type_annotation: Box::new(None),
                        })
                    ],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![],
                        Box::new(Some(node::new!(Identifier("acc".to_string()))))
                    ))),
                })),
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(6),
                            pattern: Box::new(node::new!(ListPattern(vec![
                                node::new!(IdentifierPattern {
                                    id: SymbolId::new(4),
                                    name: "x".to_string()
                                }),
                                node::new!(UnaryOp(
                                    UnaryOp::Rest,
                                    Box::new(node::new!(IdentifierPattern {
                                        id: SymbolId::new(5),
                                        name: "xs".to_string()
                                    }))
                                ))
                            ]))),
                            type_annotation: Box::new(None),
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(7),
                            pattern: Box::new(node::new!(Identifier("acc".to_string()))),
                            type_annotation: Box::new(None),
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(8),
                            pattern: Box::new(node::new!(Identifier("f".to_string()))),
                            type_annotation: Box::new(None),
                        })
                    ],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![],
                        Box::new(Some(node::new!(RecursiveCall(Box::new(node::new!(
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
                })),
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
            id: SymbolId::new(11),
            name: Box::new(node::new!(Identifier("filter".to_string()))),
            declarations: vec![
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(1),
                            pattern: Box::new(node::new!(ListPattern(vec![]))),
                            type_annotation: Box::new(None),
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            pattern: Box::new(node::new!(Identifier("f".to_string()))),
                            type_annotation: Box::new(None),
                        })
                    ],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![],
                        Box::new(Some(node::new!(List(vec![]))))
                    )))
                })),
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(5),
                            pattern: Box::new(node::new!(ListPattern(vec![
                                node::new!(IdentifierPattern {
                                    id: SymbolId::new(3),
                                    name: "x".to_string()
                                }),
                                node::new!(UnaryOp(
                                    UnaryOp::Rest,
                                    Box::new(node::new!(IdentifierPattern {
                                        id: SymbolId::new(4),
                                        name: "xs".to_string()
                                    }))
                                ))
                            ]))),
                            type_annotation: Box::new(None),
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(6),
                            pattern: Box::new(node::new!(Identifier("f".to_string()))),
                            type_annotation: Box::new(None),
                        })
                    ],
                    guard: Box::new(Some(node::new!(Call {
                        function: Box::new(node::new!(Identifier("f".to_string()))),
                        arguments: vec![node::new!(Identifier("x".to_string()))]
                    }))),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![],
                        Box::new(Some(node::new!(List(vec![
                            node::new!(Identifier("x".to_string())),
                            node::new!(UnaryOp(
                                UnaryOp::Spread,
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
                })),
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(9),
                            pattern: Box::new(node::new!(ListPattern(vec![
                                node::new!(IdentifierPattern {
                                    id: SymbolId::new(7),
                                    name: "x".to_string()
                                }),
                                node::new!(UnaryOp(
                                    UnaryOp::Rest,
                                    Box::new(node::new!(IdentifierPattern {
                                        id: SymbolId::new(8),
                                        name: "xs".to_string()
                                    }))
                                ))
                            ]))),
                            type_annotation: Box::new(None),
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(10),
                            pattern: Box::new(node::new!(Identifier("f".to_string()))),
                            type_annotation: Box::new(None),
                        })
                    ],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![],
                        Box::new(Some(node::new!(RecursiveCall(Box::new(node::new!(
                            Call {
                                function: Box::new(node::new!(Identifier("filter".to_string()))),
                                arguments: vec![
                                    node::new!(Identifier("xs".to_string())),
                                    node::new!(Identifier("f".to_string()))
                                ]
                            }
                        ))))))
                    )))
                })),
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
            id: SymbolId::new(12),
            name: Box::new(node::new!(Identifier("filter_map".to_string()))),
            declarations: vec![
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(1),
                            pattern: Box::new(node::new!(ListPattern(vec![]))),
                            type_annotation: Box::new(None),
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            pattern: Box::new(node::new!(Identifier("f".to_string()))),
                            type_annotation: Box::new(None),
                        })
                    ],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![],
                        Box::new(Some(node::new!(List(vec![]))))
                    )))
                })),
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(5),
                            pattern: Box::new(node::new!(ListPattern(vec![
                                node::new!(IdentifierPattern {
                                    id: SymbolId::new(3),
                                    name: "x".to_string()
                                }),
                                node::new!(UnaryOp(
                                    UnaryOp::Rest,
                                    Box::new(node::new!(IdentifierPattern {
                                        id: SymbolId::new(4),
                                        name: "xs".to_string()
                                    }))
                                ))
                            ]))),
                            type_annotation: Box::new(None),
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(6),
                            pattern: Box::new(node::new!(Identifier("f".to_string()))),
                            type_annotation: Box::new(None),
                        })
                    ],
                    guard: Box::new(Some(node::new!(VariableDeclaration {
                        id: SymbolId::new(7),
                        pattern: Box::new(node::new!(Identifier("y".to_string()))),
                        expression: Box::new(node::new!(Call {
                            function: Box::new(node::new!(Identifier("f".to_string()))),
                            arguments: vec![node::new!(Identifier("x".to_string()))]
                        })),
                        type_annotation: Box::new(None)
                    }))),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![],
                        Box::new(Some(node::new!(List(vec![
                            node::new!(Identifier("y".to_string())),
                            node::new!(UnaryOp(
                                UnaryOp::Spread,
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
                })),
                node::new!(FunctionDeclaration(FunctionDeclaration {
                    parameters: vec![
                        node::new!(FunctionParameter {
                            id: SymbolId::new(10),
                            pattern: Box::new(node::new!(ListPattern(vec![
                                node::new!(IdentifierPattern {
                                    id: SymbolId::new(8),
                                    name: "x".to_string()
                                }),
                                node::new!(UnaryOp(
                                    UnaryOp::Rest,
                                    Box::new(node::new!(IdentifierPattern {
                                        id: SymbolId::new(9),
                                        name: "xs".to_string()
                                    }))
                                ))
                            ]))),
                            type_annotation: Box::new(None),
                        }),
                        node::new!(FunctionParameter {
                            id: SymbolId::new(11),
                            pattern: Box::new(node::new!(Identifier("f".to_string()))),
                            type_annotation: Box::new(None),
                        })
                    ],
                    guard: Box::new(None),
                    return_type_annotation: Box::new(None),
                    body: Box::new(node::new!(Block(
                        vec![],
                        Box::new(Some(node::new!(RecursiveCall(Box::new(node::new!(
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
                })),
            ],
        })])),
    );
}

#[test]
#[should_panic]
fn test_parameter_list_mandatory_comma() {
    parse!("fn foo(x y) {}");
}

#[test]
fn test_list_matching_wildcard() {
    let program = parse!("fn tail([_, ...xs]) { xs }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionSingleDeclaration {
            id: SymbolId::new(3),
            name: Box::new(node::new!(Identifier("tail".to_string()))),
            declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                parameters: vec![node::new!(FunctionParameter {
                    id: SymbolId::new(2),
                    pattern: Box::new(node::new!(ListPattern(vec![
                        node::new!(Wildcard),
                        node::new!(UnaryOp(
                            UnaryOp::Rest,
                            Box::new(node::new!(IdentifierPattern {
                                id: SymbolId::new(1),
                                name: "xs".to_string()
                            }))
                        ))
                    ]))),
                    type_annotation: Box::new(None),
                })],
                guard: Box::new(None),
                return_type_annotation: Box::new(None),
                body: Box::new(node::new!(Block(
                    vec![],
                    Box::new(Some(node::new!(Identifier("xs".to_string()))))
                )))
            })))
        })]))
    );
}

#[test]
fn test_fn_expression_in_function_completion_position() {
    // TODO: This should work without parentheses.
    let program = parse!("fn foo() { (fn bar() {}) }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionSingleDeclaration {
            id: SymbolId::new(2),
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(node::new!(FunctionDeclaration(FunctionDeclaration {
                parameters: vec![],
                guard: Box::new(None),
                return_type_annotation: Box::new(None),
                body: Box::new(node::new!(Block(
                    vec![],
                    Box::new(Some(node::new!(FunctionExpression {
                        id: SymbolId::new(1),
                        name: Box::new(Some(node::new!(Identifier("bar".to_string())))),
                        declaration: Box::new(node::new!(FunctionDeclaration(
                            FunctionDeclaration {
                                parameters: vec![],
                                guard: Box::new(None),
                                return_type_annotation: Box::new(None),
                                body: Box::new(node::new!(Block(vec![], Box::new(None))))
                            }
                        )))
                    })))
                )))
            })))
        })]))
    );
}
