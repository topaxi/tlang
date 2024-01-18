use pretty_assertions::assert_eq;

use tlang_ast::{
    node::{self, BinaryOp, UnaryOp},
    symbols::SymbolId,
    token::Literal,
};

mod common;

#[test]
fn test_simple_variable_declaration() {
    let program = parse!("let x = 1 + 2;");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(1),
            pattern: Box::new(node::new!(Identifier("x".to_string()))),
            expression: Box::new(node::new!(BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(node::new!(Literal(Literal::Integer(1)))),
                rhs: Box::new(node::new!(Literal(Literal::Integer(2)))),
            })),
            type_annotation: Box::new(None),
        })]))
    );
}

#[test]
#[should_panic]
fn test_panic_on_keyword_as_identifier() {
    parse!("let fn = 1;");
}

#[test]
fn test_list_pattern() {
    let program = parse!("let [x, y] = [1, 2];");

    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(3),
            pattern: Box::new(node::new!(ListPattern(vec![
                node::new!(IdentifierPattern {
                    id: SymbolId::new(1),
                    name: "x".to_string()
                }),
                node::new!(IdentifierPattern {
                    id: SymbolId::new(2),
                    name: "y".to_string()
                }),
            ]))),
            expression: Box::new(node::new!(List(vec![
                node::new!(Literal(Literal::Integer(1))),
                node::new!(Literal(Literal::Integer(2))),
            ]))),
            type_annotation: Box::new(None),
        })]))
    );
}

#[test]
fn test_list_pattern_rest() {
    let program = parse!("let [x, y, ...z] = [1, 2, 3, 4];");
    assert_eq!(
        program,
        node::new!(Program(vec![node::new!(VariableDeclaration {
            id: SymbolId::new(4),
            pattern: Box::new(node::new!(ListPattern(vec![
                node::new!(IdentifierPattern {
                    id: SymbolId::new(1),
                    name: "x".to_string()
                }),
                node::new!(IdentifierPattern {
                    id: SymbolId::new(2),
                    name: "y".to_string()
                }),
                node::new!(UnaryOp(
                    UnaryOp::Rest,
                    Box::new(node::new!(IdentifierPattern {
                        id: SymbolId::new(3),
                        name: "z".to_string()
                    }))
                )),
            ]))),
            expression: Box::new(node::new!(List(vec![
                node::new!(Literal(Literal::Integer(1))),
                node::new!(Literal(Literal::Integer(2))),
                node::new!(Literal(Literal::Integer(3))),
                node::new!(Literal(Literal::Integer(4))),
            ]))),
            type_annotation: Box::new(None),
        })]))
    );
}
