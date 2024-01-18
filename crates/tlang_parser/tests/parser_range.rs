use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_ast::{node, symbols::SymbolId, token::Literal};

mod common;

#[test]
#[ignore = "Not implemented yet"]
fn test_range_notation() {
    let program = parse!(indoc! {"
        let range = 1..10;
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            pattern: Box::new(node::new!(Identifier("range".to_string()))),
            expression: Box::new(node::new!(Range {
                start: Box::new(node::new!(Literal(Literal::Integer(1)))),
                end: Box::new(node::new!(Literal(Literal::Integer(10)))),
                inclusive: false
            })),
            type_annotation: Box::new(None)
        })]))
    );
}

#[test]
#[ignore = "Not implemented yet"]
fn test_inclusive_range_notation() {
    let program = parse!(indoc! {"
        let range = 1..=10;
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            pattern: Box::new(node::new!(Identifier("range".to_string()))),
            expression: Box::new(node::new!(Range {
                start: Box::new(node::new!(Literal(Literal::Integer(1)))),
                end: Box::new(node::new!(Literal(Literal::Integer(10)))),
                inclusive: true
            })),
            type_annotation: Box::new(None)
        })]))
    );
}

#[test]
#[ignore = "Not implemented yet"]
fn test_range_as_index() {
    let program = parse!(indoc! {"
        let x = [1, 2, 3];
        let y = x[1..10];
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![
            node::new!(VariableDeclaration {
                id: SymbolId::new(1),
                pattern: Box::new(node::new!(Identifier("x".to_string()))),
                expression: Box::new(node::new!(List(vec![
                    node::new!(Literal(Literal::Integer(1))),
                    node::new!(Literal(Literal::Integer(2))),
                    node::new!(Literal(Literal::Integer(3)))
                ]))),
                type_annotation: Box::new(None)
            }),
            node::new!(VariableDeclaration {
                id: SymbolId::new(2),
                pattern: Box::new(node::new!(Identifier("y".to_string()))),
                expression: Box::new(node::new!(IndexExpression {
                    base: Box::new(node::new!(Identifier("x".to_string()))),
                    index: Box::new(node::new!(Range {
                        start: Box::new(node::new!(Literal(Literal::Integer(1)))),
                        end: Box::new(node::new!(Literal(Literal::Integer(10)))),
                        inclusive: false
                    }))
                })),
                type_annotation: Box::new(None)
            })
        ]))
    );
}

#[test]
#[ignore = "Not implemented yet"]
fn test_inclusive_range_as_index() {
    let program = parse!(indoc! {"
        let x = [1, 2, 3];
        let y = x[1..=10];
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![
            node::new!(VariableDeclaration {
                id: SymbolId::new(1),
                pattern: Box::new(node::new!(Identifier("x".to_string()))),
                expression: Box::new(node::new!(List(vec![
                    node::new!(Literal(Literal::Integer(1))),
                    node::new!(Literal(Literal::Integer(2))),
                    node::new!(Literal(Literal::Integer(3)))
                ]))),
                type_annotation: Box::new(None)
            }),
            node::new!(VariableDeclaration {
                id: SymbolId::new(2),
                pattern: Box::new(node::new!(Identifier("y".to_string()))),
                expression: Box::new(node::new!(IndexExpression {
                    base: Box::new(node::new!(Identifier("x".to_string()))),
                    index: Box::new(node::new!(Range {
                        start: Box::new(node::new!(Literal(Literal::Integer(1)))),
                        end: Box::new(node::new!(Literal(Literal::Integer(10)))),
                        inclusive: true
                    }))
                })),
                type_annotation: Box::new(None)
            })
        ]))
    );
}
