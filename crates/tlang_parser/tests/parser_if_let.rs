use indoc::indoc;
use pretty_assertions::assert_eq;

use tlang_ast::{
    node::{self},
    symbols::SymbolId,
    token::Literal,
};

mod common;

#[test]
fn test_simple_if_let() {
    let program = parse!(indoc! {"
        if let x = 1 {
            x
        };
    "});

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(ExpressionStatement(Box::new(
            node::new!(IfElse {
                condition: Box::new(node::new!(VariableDeclaration {
                    id: SymbolId::new(1),
                    name: "x".to_string(),
                    value: Box::new(node::new!(Literal(Literal::Integer(1)))),
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
