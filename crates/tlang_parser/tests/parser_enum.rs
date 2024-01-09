use indoc::indoc;
use pretty_assertions::assert_eq;

use tlang_ast::{
    node::{self, BinaryOp, FunctionDeclaration},
    symbols::SymbolId,
    token::Literal,
};

mod common;

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
                pattern: Box::new(node::new!(Identifier("x".to_string()))),
                expression: Box::new(node::new!(Call {
                    function: Box::new(node::new!(NestedIdentifier(vec![
                        "Option".to_string(),
                        "Some".to_string()
                    ]))),
                    arguments: vec![node::new!(Literal(Literal::Integer(42)))]
                })),
                type_annotation: None,
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
                pattern: Box::new(node::new!(Identifier("x".to_string()))),
                expression: Box::new(node::new!(Call {
                    function: Box::new(node::new!(NestedIdentifier(vec![
                        "Option".to_string(),
                        "Some".to_string()
                    ]))),
                    arguments: vec![node::new!(Dict(vec![(
                        node::new!(Identifier("x".to_string())),
                        node::new!(Literal(Literal::Integer(42))),
                    )]))],
                })),
                type_annotation: None,
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
                name: Box::new(node::new!(Identifier("maximum_depth".to_string()))),
                declarations: vec![
                    node::new!(FunctionDeclaration(FunctionDeclaration {
                        parameters: vec![node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            node: Box::new(node::new!(EnumExtraction {
                                identifier: Box::new(node::new!(NestedIdentifier(vec![
                                    "Tree".to_string(),
                                    "Leaf".to_string()
                                ]))),
                                elements: vec![node::new!(Wildcard)],
                                named_fields: false,
                            })),
                            type_annotation: None,
                        })],
                        guard: None,
                        return_type_annotation: None,
                        body: Box::new(node::new!(Block(
                            vec![],
                            Some(Box::new(node::new!(Literal(Literal::Integer(1)))))
                        )))
                    })),
                    node::new!(FunctionDeclaration(FunctionDeclaration {
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
                            })),
                            type_annotation: None,
                        })],
                        guard: None,
                        return_type_annotation: None,
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
                    }))
                ]
            }),
            node::new!(FunctionSingleDeclaration {
                id: SymbolId::new(6),
                name: Box::new(node::new!(Identifier("main".to_string()))),
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![],
                    guard: None,
                    return_type_annotation: None,
                    body: Box::new(node::new!(Block(
                        vec![node::new!(VariableDeclaration {
                            id: SymbolId::new(5),
                            pattern: Box::new(node::new!(Identifier("x".to_string()))),
                            expression: Box::new(node::new!(Call {
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
                            })),
                            type_annotation: None,
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
                name: Box::new(node::new!(Identifier("unwrap".to_string()))),
                declarations: vec![
                    node::new!(FunctionDeclaration(FunctionDeclaration {
                        parameters: vec![node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            node: Box::new(node::new!(EnumExtraction {
                                identifier: Box::new(node::new!(NestedIdentifier(vec![
                                    "Option".to_string(),
                                    "None".to_string()
                                ]))),
                                elements: vec![],
                                named_fields: false,
                            })),
                            type_annotation: None,
                        })],
                        guard: None,
                        return_type_annotation: None,
                        body: Box::new(node::new!(Block(
                            vec![],
                            Some(Box::new(node::new!(Call {
                                function: Box::new(node::new!(Identifier("panic".to_string()))),
                                arguments: vec![node::new!(Literal(Literal::String(
                                    "Cannot unwrap None".to_string()
                                )))]
                            })))
                        )))
                    })),
                    node::new!(FunctionDeclaration(FunctionDeclaration {
                        parameters: vec![node::new!(FunctionParameter {
                            id: SymbolId::new(3),
                            node: Box::new(node::new!(EnumExtraction {
                                identifier: Box::new(node::new!(NestedIdentifier(vec![
                                    "Option".to_string(),
                                    "Some".to_string()
                                ]))),
                                elements: vec![node::new!(Identifier("value".to_string()))],
                                named_fields: false,
                            })),
                            type_annotation: None,
                        })],
                        guard: None,
                        return_type_annotation: None,
                        body: Box::new(node::new!(Block(
                            vec![],
                            Some(Box::new(node::new!(Identifier("value".to_string()))))
                        )))
                    }))
                ]
            })
        ]))
    );
}
