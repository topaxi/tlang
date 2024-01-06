use pretty_assertions::assert_eq;

use tlang_ast::{
    node::{self, AstNode, FunctionDeclaration, Node},
    symbols::SymbolId,
    token::Literal,
};
use tlang_parser::parser::Parser;

macro_rules! parse {
    ($source:expr) => {{
        let mut parser = Parser::from_source($source);
        parser.parse()
    }};
}

#[test]
fn test_variable_declaration_type_annotation() {
    let program = parse!("let x: i64 = 1;");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            name: "x".to_string(),
            value: Box::new(node::new!(Literal(Literal::Integer(1)))),
            type_annotation: Some(Box::new(node::new!(TypeAnnotation {
                name: Box::new(node::new!(Identifier("i64".to_string()))),
                parameters: vec![]
            })))
        })]))
    );

    let program = parse!("let x: Option<i64> = 1;");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            name: "x".to_string(),
            value: Box::new(node::new!(Literal(Literal::Integer(1)))),
            type_annotation: Some(Box::new(node::new!(TypeAnnotation {
                name: Box::new(node::new!(Identifier("Option".to_string()))),
                parameters: vec![node::new!(TypeAnnotation {
                    name: Box::new(node::new!(Identifier("i64".to_string()))),
                    parameters: vec![]
                })]
            })))
        })]))
    );

    let program = parse!("let x: std::hash::Map<str, i64> = std::hash::Map();");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            name: "x".to_string(),
            value: Box::new(node::new!(Call {
                function: Box::new(node::new!(NestedIdentifier(vec![
                    "std".to_string(),
                    "hash".to_string(),
                    "Map".to_string()
                ]))),
                arguments: vec![]
            })),
            type_annotation: Some(Box::new(node::new!(TypeAnnotation {
                name: Box::new(node::new!(NestedIdentifier(vec![
                    "std".to_string(),
                    "hash".to_string(),
                    "Map".to_string()
                ]))),
                parameters: vec![
                    node::new!(TypeAnnotation {
                        name: Box::new(node::new!(Identifier("str".to_string()))),
                        parameters: vec![]
                    }),
                    node::new!(TypeAnnotation {
                        name: Box::new(node::new!(Identifier("i64".to_string()))),
                        parameters: vec![]
                    })
                ]
            })))
        })]))
    )
}

#[test]
fn test_return_type_annotation() {
    let program = parse!("fn foo() -> i64 { 1 }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclaration {
            id: SymbolId::new(1),
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![],
                guard: None,
                body: Box::new(node::new!(Block(
                    vec![],
                    Some(Box::new(node::new!(Literal(Literal::Integer(1)))))
                ))),
                return_type_annotation: Some(Box::new(node::new!(TypeAnnotation {
                    name: Box::new(node::new!(Identifier("i64".to_string()))),
                    parameters: vec![]
                })))
            })
        })]))
    );

    let program = parse!("fn foo() -> Option<i64> { 1 }");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(FunctionDeclaration {
            id: SymbolId::new(1),
            name: Box::new(node::new!(Identifier("foo".to_string()))),
            declaration: Box::new(FunctionDeclaration {
                parameters: vec![],
                guard: None,
                body: Box::new(node::new!(Block(
                    vec![],
                    Some(Box::new(node::new!(Literal(Literal::Integer(1)))))
                ))),
                return_type_annotation: Some(Box::new(node::new!(TypeAnnotation {
                    name: Box::new(node::new!(Identifier("Option".to_string()))),
                    parameters: vec![node::new!(TypeAnnotation {
                        name: Box::new(node::new!(Identifier("i64".to_string()))),
                        parameters: vec![]
                    })]
                })))
            })
        })]))
    );

    let program = parse!("let expr = fn foo() -> i64 { 1 };");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(2),
            name: "expr".to_string(),
            value: Box::new(node::new!(FunctionExpression {
                id: SymbolId::new(1),
                name: Some(Box::new(node::new!(Identifier("foo".to_string())))),
                declaration: Box::new(FunctionDeclaration {
                    parameters: vec![],
                    guard: None,
                    body: Box::new(node::new!(Block(
                        vec![],
                        Some(Box::new(node::new!(Literal(Literal::Integer(1)))))
                    ))),
                    return_type_annotation: Some(Box::new(node::new!(TypeAnnotation {
                        name: Box::new(node::new!(Identifier("i64".to_string()))),
                        parameters: vec![]
                    })))
                })
            })),
            type_annotation: None,
        })]))
    );
}

#[test]
fn test_function_param_types() {
    let program = parse!("fn foo(x: i64, y: i64) -> i64 { x }");

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
                        type_annotation: Some(Box::new(node::new!(TypeAnnotation {
                            name: Box::new(node::new!(Identifier("i64".to_string()))),
                            parameters: vec![]
                        })))
                    }),
                    node::new!(FunctionParameter {
                        id: SymbolId::new(2),
                        node: Box::new(node::new!(Identifier("y".to_string()))),
                        type_annotation: Some(Box::new(node::new!(TypeAnnotation {
                            name: Box::new(node::new!(Identifier("i64".to_string()))),
                            parameters: vec![]
                        })))
                    })
                ],
                guard: None,
                body: Box::new(node::new!(Block(
                    vec![],
                    Some(Box::new(node::new!(Identifier("x".to_string()))))
                ))),
                return_type_annotation: Some(Box::new(node::new!(TypeAnnotation {
                    name: Box::new(node::new!(Identifier("i64".to_string()))),
                    parameters: vec![]
                })))
            })
        })]))
    );
}
