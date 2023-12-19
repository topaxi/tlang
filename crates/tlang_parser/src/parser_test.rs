use crate::lexer::{Lexer, Literal};
use crate::parser::{BinaryOp, Node, Parser};

macro_rules! parse {
    ($source:expr) => {{
        let lexer = Lexer::new($source);
        let mut parser = Parser::new(lexer);
        parser.parse_program()
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
        Node::Program(vec![Node::BinaryOp {
            op: BinaryOp::Add,
            lhs: Box::new(Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }),
            rhs: Box::new(Node::Literal(Literal::Integer(3))),
        }])
    );

    let program = parse!("1 * 2 + 3;");
    assert_eq!(
        program,
        Node::Program(vec![Node::BinaryOp {
            op: BinaryOp::Add,
            lhs: Box::new(Node::BinaryOp {
                op: BinaryOp::Multiply,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }),
            rhs: Box::new(Node::Literal(Literal::Integer(3))),
        }])
    );
}

#[test]
fn test_simple_arithmetic_sum_mult_precedence() {
    let program = parse!("1 + 2 * 3;");
    assert_eq!(
        program,
        Node::Program(vec![Node::BinaryOp {
            op: BinaryOp::Add,
            lhs: Box::new(Node::Literal(Literal::Integer(1))),
            rhs: Box::new(Node::BinaryOp {
                op: BinaryOp::Multiply,
                lhs: Box::new(Node::Literal(Literal::Integer(2))),
                rhs: Box::new(Node::Literal(Literal::Integer(3))),
            }),
        }])
    );
}

#[test]
fn test_simple_arithmetic_sum_mult_precedence_parentheses() {
    let program = parse!("(1 + 2) * 3;");
    assert_eq!(
        program,
        Node::Program(vec![Node::BinaryOp {
            op: BinaryOp::Multiply,
            lhs: Box::new(Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }),
            rhs: Box::new(Node::Literal(Literal::Integer(3))),
        }])
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
            Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Identifier("x".to_string())),
                rhs: Box::new(Node::Identifier("y".to_string())),
            }
        ])
    );
}

#[test]
fn test_simple_call() {
    let program = parse!("foo(1, 2);");
    assert_eq!(
        program,
        Node::Program(vec![Node::Call {
            function: Box::new(Node::Identifier("foo".to_string())),
            arguments: vec![
                Node::Literal(Literal::Integer(1)),
                Node::Literal(Literal::Integer(2))
            ],
        }])
    );
}

#[test]
fn test_nested_call() {
    let program = parse!("foo(bar(1), 2);");
    assert_eq!(
        program,
        Node::Program(vec![Node::Call {
            function: Box::new(Node::Identifier("foo".to_string())),
            arguments: vec![
                Node::Call {
                    function: Box::new(Node::Identifier("bar".to_string())),
                    arguments: vec![Node::Literal(Literal::Integer(1))]
                },
                Node::Literal(Literal::Integer(2))
            ],
        }])
    );
}

#[test]
fn test_call_with_expression() {
    let program = parse!("foo(1 + 2, 3);");
    assert_eq!(
        program,
        Node::Program(vec![Node::Call {
            function: Box::new(Node::Identifier("foo".to_string())),
            arguments: vec![
                Node::BinaryOp {
                    op: BinaryOp::Add,
                    lhs: Box::new(Node::Literal(Literal::Integer(1))),
                    rhs: Box::new(Node::Literal(Literal::Integer(2))),
                },
                Node::Literal(Literal::Integer(3))
            ],
        }])
    );
}

#[test]
fn test_block_expression() {
    let program = parse!("let x = { 1 + 2; };");
    assert_eq!(
        program,
        Node::Program(vec![Node::VariableDeclaration {
            name: "x".to_string(),
            value: Box::new(Node::Program(vec![Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }]))
        }])
    );
}

#[test]
fn test_if_statement() {
    let program = parse!("if (1 + 2) { 3 + 4; }");
    assert_eq!(
        program,
        Node::Program(vec![Node::IfElse {
            condition: Box::new(Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }),
            then_branch: Box::new(Node::Program(vec![Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(3))),
                rhs: Box::new(Node::Literal(Literal::Integer(4))),
            }])),
            else_branch: None,
        }])
    );
}

#[test]
fn test_if_else_statement() {
    let program = parse!("if (1 + 2) { 3 + 4; } else { 5 + 6; }");
    assert_eq!(
        program,
        Node::Program(vec![Node::IfElse {
            condition: Box::new(Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }),
            then_branch: Box::new(Node::Program(vec![Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(3))),
                rhs: Box::new(Node::Literal(Literal::Integer(4))),
            }])),
            else_branch: Some(Box::new(Node::Program(vec![Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(5))),
                rhs: Box::new(Node::Literal(Literal::Integer(6))),
            }]))),
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
                then_branch: Box::new(Node::Program(vec![Node::Literal(Literal::Integer(1))])),
                else_branch: Some(Box::new(Node::Program(vec![Node::Literal(
                    Literal::Integer(2)
                )]))),
            })
        }])
    );
}

#[test]
fn test_function_declaration() {
    let program = parse!("fn foo() { 1 + 2; }");
    assert_eq!(
        program,
        Node::Program(vec![Node::FunctionDeclaration {
            name: "foo".to_string(),
            parameters: vec![],
            body: Box::new(Node::Program(vec![Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }]))
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
            parameters: vec!["x".to_string(), "y".to_string()],
            body: Box::new(Node::Program(vec![Node::BinaryOp {
                op: BinaryOp::Add,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }]))
        }])
    );
}

#[test]
fn test_stray_semicolon() {
    let program = parse!("1 + 2;;");
    assert_eq!(
        program,
        Node::Program(vec![Node::BinaryOp {
            op: BinaryOp::Add,
            lhs: Box::new(Node::Literal(Literal::Integer(1))),
            rhs: Box::new(Node::Literal(Literal::Integer(2))),
        }])
    );
}

#[test]
fn test_exponentiation() {
    let program = parse!("1 * 2 ** 3;");
    assert_eq!(
        program,
        Node::Program(vec![Node::BinaryOp {
            op: BinaryOp::Multiply,
            lhs: Box::new(Node::Literal(Literal::Integer(1))),
            rhs: Box::new(Node::BinaryOp {
                op: BinaryOp::Exponentiation,
                lhs: Box::new(Node::Literal(Literal::Integer(2))),
                rhs: Box::new(Node::Literal(Literal::Integer(3))),
            }),
        }])
    );

    let program = parse!("1 ** 2 * 3;");
    assert_eq!(
        program,
        Node::Program(vec![Node::BinaryOp {
            op: BinaryOp::Multiply,
            lhs: Box::new(Node::BinaryOp {
                op: BinaryOp::Exponentiation,
                lhs: Box::new(Node::Literal(Literal::Integer(1))),
                rhs: Box::new(Node::Literal(Literal::Integer(2))),
            }),
            rhs: Box::new(Node::Literal(Literal::Integer(3))),
        }])
    );

    let program = parse!("1 ** 2 ** 3;");
    assert_eq!(
        program,
        Node::Program(vec![Node::BinaryOp {
            op: BinaryOp::Exponentiation,
            lhs: Box::new(Node::Literal(Literal::Integer(1))),
            rhs: Box::new(Node::BinaryOp {
                op: BinaryOp::Exponentiation,
                lhs: Box::new(Node::Literal(Literal::Integer(2))),
                rhs: Box::new(Node::Literal(Literal::Integer(3))),
            }),
        }])
    );
}
