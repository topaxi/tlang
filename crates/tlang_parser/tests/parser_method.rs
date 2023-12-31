use indoc::indoc;
use pretty_assertions::assert_eq;

use tlang_ast::{
    node::{self, FunctionDeclaration},
    symbols::SymbolId,
    token::Literal,
};

mod common;

#[test]
fn test_declare_methods_on_option_enum() {
    // Static functions.
    // fn Option::is_option(Option::Some(_)) { true }
    // Methods on enum.
    // fn Option.is_some(Option::Some(_)) { true }
    let program = parse!(indoc! {"
        enum Option {
            Some(x),
            None,
        }

        fn Option::is_option(Option::Some(_)) { true }
        fn Option::is_option(Option::None) { true }
        fn Option::is_option(_) { false }

        fn Option.is_some(Option::Some(_)) { true }
        fn Option.is_some(Option::None) { false }

        fn Option.map(Option::Some(x), f) { Option::Some(f(x)) }
        fn Option.map(Option::None, _) { Option::None }
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
            node::new!(FunctionDeclarations {
                id: SymbolId::new(5),
                name: Box::new(node::new!(NestedIdentifier(vec![
                    "Option".to_string(),
                    "is_option".to_string()
                ]))),
                declarations: vec![
                    node::new!(FunctionDeclaration(FunctionDeclaration {
                        parameters: vec![node::new!(FunctionParameter {
                            id: SymbolId::new(2),
                            node: Box::new(node::new!(EnumExtraction {
                                identifier: Box::new(node::new!(NestedIdentifier(vec![
                                    "Option".to_string(),
                                    "Some".to_string()
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
                            Some(Box::new(node::new!(Literal(Literal::Boolean(true)))))
                        ))),
                    })),
                    node::new!(FunctionDeclaration(FunctionDeclaration {
                        parameters: vec![node::new!(FunctionParameter {
                            id: SymbolId::new(3),
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
                            Some(Box::new(node::new!(Literal(Literal::Boolean(true)))))
                        ))),
                    })),
                    node::new!(FunctionDeclaration(FunctionDeclaration {
                        parameters: vec![node::new!(FunctionParameter {
                            id: SymbolId::new(4),
                            node: Box::new(node::new!(Wildcard)),
                            type_annotation: None,
                        })],
                        guard: None,
                        return_type_annotation: None,
                        body: Box::new(node::new!(Block(
                            vec![],
                            Some(Box::new(node::new!(Literal(Literal::Boolean(false)))))
                        ))),
                    }))
                ]
            }),
            node::new!(FunctionDeclarations {
                id: SymbolId::new(8),
                name: Box::new(node::new!(FieldExpression {
                    base: Box::new(node::new!(Identifier("Option".to_string()))),
                    field: Box::new(node::new!(Identifier("is_some".to_string()))),
                })),
                declarations: vec![
                    node::new!(FunctionDeclaration(FunctionDeclaration {
                        parameters: vec![node::new!(FunctionParameter {
                            id: SymbolId::new(6),
                            node: Box::new(node::new!(EnumExtraction {
                                identifier: Box::new(node::new!(NestedIdentifier(vec![
                                    "Option".to_string(),
                                    "Some".to_string()
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
                            Some(Box::new(node::new!(Literal(Literal::Boolean(true)))))
                        ))),
                    })),
                    node::new!(FunctionDeclaration(FunctionDeclaration {
                        parameters: vec![node::new!(FunctionParameter {
                            id: SymbolId::new(7),
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
                            Some(Box::new(node::new!(Literal(Literal::Boolean(false)))))
                        ))),
                    }))
                ]
            }),
            node::new!(FunctionDeclarations {
                id: SymbolId::new(13),
                name: Box::new(node::new!(FieldExpression {
                    base: Box::new(node::new!(Identifier("Option".to_string()))),
                    field: Box::new(node::new!(Identifier("map".to_string()))),
                })),
                declarations: vec![
                    node::new!(FunctionDeclaration(FunctionDeclaration {
                        parameters: vec![
                            node::new!(FunctionParameter {
                                id: SymbolId::new(9),
                                node: Box::new(node::new!(EnumExtraction {
                                    identifier: Box::new(node::new!(NestedIdentifier(vec![
                                        "Option".to_string(),
                                        "Some".to_string()
                                    ]))),
                                    elements: vec![node::new!(Identifier("x".to_string()))],
                                    named_fields: false,
                                })),
                                type_annotation: None,
                            }),
                            node::new!(FunctionParameter {
                                id: SymbolId::new(10),
                                node: Box::new(node::new!(Identifier("f".to_string()))),
                                type_annotation: None,
                            })
                        ],
                        guard: None,
                        return_type_annotation: None,
                        body: Box::new(node::new!(Block(
                            vec![],
                            Some(Box::new(node::new!(Call {
                                function: Box::new(node::new!(NestedIdentifier(vec![
                                    "Option".to_string(),
                                    "Some".to_string()
                                ]))),
                                arguments: vec![node::new!(Call {
                                    function: Box::new(node::new!(Identifier("f".to_string()))),
                                    arguments: vec![node::new!(Identifier("x".to_string()))]
                                })],
                            })))
                        ))),
                    })),
                    node::new!(FunctionDeclaration(FunctionDeclaration {
                        parameters: vec![
                            node::new!(FunctionParameter {
                                id: SymbolId::new(11),
                                node: Box::new(node::new!(EnumExtraction {
                                    identifier: Box::new(node::new!(NestedIdentifier(vec![
                                        "Option".to_string(),
                                        "None".to_string()
                                    ]))),
                                    elements: vec![],
                                    named_fields: false,
                                })),
                                type_annotation: None,
                            }),
                            node::new!(FunctionParameter {
                                id: SymbolId::new(12),
                                node: Box::new(node::new!(Wildcard)),
                                type_annotation: None,
                            })
                        ],
                        guard: None,
                        return_type_annotation: None,
                        body: Box::new(node::new!(Block(
                            vec![],
                            Some(Box::new(node::new!(NestedIdentifier(vec![
                                "Option".to_string(),
                                "None".to_string()
                            ])))),
                        ))),
                    }))
                ]
            }),
        ])),
    );
}
