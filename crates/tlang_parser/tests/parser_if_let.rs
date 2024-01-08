use indoc::indoc;
use pretty_assertions::assert_eq;

use tlang_ast::{
    node::{self},
    symbols::SymbolId,
    token::Literal,
};

mod common;

#[test]
fn test_simple_if_let_statement() {
    let program = parse!(indoc! {"
        if let x = 1 {
            x
        }
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(IfElse {
                condition: Box::new(node::new!(VariableDeclaration {
                    id: SymbolId::new(1),
                    pattern: Box::new(node::new!(Identifier("x".to_string()))),
                    expression: Box::new(node::new!(Literal(Literal::Integer(1)))),
                    type_annotation: None,
                })),
                then_branch: Box::new(node::new!(Block(
                    vec![],
                    Some(Box::new(node::new!(Identifier("x".to_string()))))
                ))),
                else_branch: None,
            })
        )))]))
    );
}

#[test]
fn test_if_let_statement_with_enum_matching() {
    let program = parse!(indoc! {"
        if let Option::Some(y) = x; {
            y
        }
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(IfElse {
                condition: Box::new(node::new!(VariableDeclaration {
                    id: SymbolId::new(1),
                    pattern: Box::new(node::new!(EnumExtraction {
                        identifier: Box::new(node::new!(NestedIdentifier(vec![
                            "Option".to_string(),
                            "Some".to_string()
                        ]))),
                        elements: vec![node::new!(Identifier("y".to_string()))],
                        named_fields: false,
                    })),
                    expression: Box::new(node::new!(Identifier("x".to_string()))),
                    type_annotation: None,
                })),
                then_branch: Box::new(node::new!(Block(
                    vec![],
                    Some(Box::new(node::new!(Identifier("y".to_string()))))
                ))),
                else_branch: None,
            })
        )))]))
    );
}
