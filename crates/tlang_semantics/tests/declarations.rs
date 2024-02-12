use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_ast::{
    node::{AstNode, NodeKind},
    span::{LineColumn, Span},
    symbols::{SymbolId, SymbolInfo, SymbolType},
};
use tlang_parser::parser::Parser;
use tlang_semantics::analyzer::SemanticAnalyzer;

macro_rules! analyze {
    ($source:expr) => {{
        let mut parser = Parser::from_source($source);
        let mut ast = parser.parse().unwrap();
        let mut analyzer = SemanticAnalyzer::default();
        analyzer.add_builtin_symbols(&[("panic", SymbolType::Function)]);
        match analyzer.analyze(&mut ast) {
            Ok(_) => ast,
            Err(diagnostics) => panic!("Expected no diagnostics, got {:#?}", diagnostics),
        }
    }};
}

#[test]
fn test_analyze_variable_declaration() {
    let ast = analyze!("let a = 1;");

    assert_eq!(
        ast.symbol_table.unwrap().borrow().get_by_name("a"),
        Some(SymbolInfo {
            id: SymbolId::new(1),
            name: "a".to_string(),
            symbol_type: SymbolType::Variable,
            defined_at: Some(Span::new(
                LineColumn { line: 0, column: 4 },
                LineColumn { line: 0, column: 5 }
            )),
            ..Default::default()
        })
    );
}

#[test]
fn test_block_scope() {
    let ast = analyze!(
        "
        let a = 1;
        {
            let b = 2;
            {
                let c = 3;
            };
        };
        "
    );

    let program_symbols = ast
        .symbol_table
        .clone()
        .expect("Program to have a symbol_table");

    assert_eq!(
        program_symbols.borrow().get_by_name("a"),
        Some(SymbolInfo {
            id: SymbolId::new(1),
            name: "a".to_string(),
            symbol_type: SymbolType::Variable,
            defined_at: Some(Span::new(
                LineColumn {
                    line: 1,
                    column: 13
                },
                LineColumn {
                    line: 1,
                    column: 14
                }
            )),
            ..Default::default()
        })
    );
    assert_eq!(program_symbols.borrow().get_by_name("b"), None);
    assert_eq!(program_symbols.borrow().get_by_name("c"), None);

    let block1 = match ast.ast_node {
        NodeKind::Legacy(AstNode::Module(ref nodes)) => match nodes[1].ast_node {
            NodeKind::Legacy(AstNode::ExpressionStatement(ref node)) => match node.ast_node {
                NodeKind::Legacy(AstNode::Block(_, _)) => node,
                _ => panic!("Expected block {:?}", node.ast_node),
            },
            _ => panic!("Expected expression statement {:?}", nodes[1].ast_node),
        },
        _ => panic!("Expected program {:?}", ast.ast_node),
    };

    let block1_symbols = block1
        .symbol_table
        .clone()
        .expect("Expected block 1 to have a symbol_table");

    assert_eq!(
        block1_symbols.borrow().get_by_name("a"),
        Some(SymbolInfo {
            id: SymbolId::new(1),
            name: "a".to_string(),
            symbol_type: SymbolType::Variable,
            defined_at: Some(Span::new(
                LineColumn {
                    line: 1,
                    column: 13
                },
                LineColumn {
                    line: 1,
                    column: 14
                }
            )),
            ..Default::default()
        })
    );
    assert_eq!(
        block1_symbols.borrow().get_by_name("b"),
        Some(SymbolInfo {
            id: SymbolId::new(2),
            name: "b".to_string(),
            symbol_type: SymbolType::Variable,
            defined_at: Some(Span::new(
                LineColumn {
                    line: 3,
                    column: 17
                },
                LineColumn {
                    line: 3,
                    column: 18
                }
            )),
            ..Default::default()
        })
    );
    assert_eq!(block1_symbols.borrow().get_by_name("c"), None);

    let block2 = match block1.ast_node {
        NodeKind::Legacy(AstNode::Block(ref nodes, _)) => match nodes[1].ast_node {
            NodeKind::Legacy(AstNode::ExpressionStatement(ref node)) => match node.ast_node {
                NodeKind::Legacy(AstNode::Block(_, _)) => node,
                _ => panic!("Expected block {:?}", node.ast_node),
            },
            _ => panic!("Expected expression statement {:?}", nodes[1].ast_node),
        },
        _ => panic!("Expected program {:?}", ast.ast_node),
    };

    let block2_symbols = block2
        .symbol_table
        .clone()
        .expect("Expected block 2 to have a symbol_table");

    assert_eq!(
        block2_symbols.borrow().get_by_name("a"),
        Some(SymbolInfo {
            id: SymbolId::new(1),
            name: "a".to_string(),
            symbol_type: SymbolType::Variable,
            defined_at: Some(Span::new(
                LineColumn {
                    line: 1,
                    column: 13
                },
                LineColumn {
                    line: 1,
                    column: 14
                }
            )),
            ..Default::default()
        })
    );
    assert_eq!(
        block2_symbols.borrow().get_by_name("b"),
        Some(SymbolInfo {
            id: SymbolId::new(2),
            name: "b".to_string(),
            symbol_type: SymbolType::Variable,
            defined_at: Some(Span::new(
                LineColumn {
                    line: 3,
                    column: 17
                },
                LineColumn {
                    line: 3,
                    column: 18
                }
            )),
            ..Default::default()
        })
    );
    assert_eq!(
        block2_symbols.borrow().get_by_name("c"),
        Some(SymbolInfo {
            id: SymbolId::new(3),
            name: "c".to_string(),
            symbol_type: SymbolType::Variable,
            defined_at: Some(Span::new(
                LineColumn {
                    line: 5,
                    column: 21
                },
                LineColumn {
                    line: 5,
                    column: 22
                }
            )),
            ..Default::default()
        })
    );
}

#[test]
fn test_should_collect_function_definitions() {
    let ast = analyze!(indoc! {"
        fn add(a, b) {
            a + b
        }
    "});

    let program_symbols = ast
        .symbol_table
        .clone()
        .expect("Program to have a symbol_table");

    assert_eq!(
        program_symbols.borrow().get_by_name("add"),
        Some(SymbolInfo {
            id: SymbolId::new(3),
            name: "add".to_string(),
            symbol_type: SymbolType::Function,
            defined_at: Some(Span::new(
                LineColumn { line: 0, column: 3 },
                LineColumn { line: 0, column: 6 }
            )),
            ..Default::default()
        })
    );
}

#[test]
fn test_should_collect_list_destructuring_symbols_in_function_arguments() {
    let ast = analyze!(indoc! {"
        fn add([a, b]) {
            a + b
        }
    "});

    let program_symbols = ast
        .symbol_table
        .clone()
        .expect("Program to have a symbol_table");

    assert_eq!(
        program_symbols.borrow().get_by_name("add"),
        Some(SymbolInfo {
            id: SymbolId::new(4),
            name: "add".to_string(),
            symbol_type: SymbolType::Function,
            defined_at: Some(Span::new(
                LineColumn { line: 0, column: 3 },
                LineColumn { line: 0, column: 6 }
            )),
            ..Default::default()
        })
    );
}

#[test]
fn test_should_collect_list_destructuring_with_rest_symbols_in_function_arguments() {
    let ast = analyze!(indoc! {"
        fn sum([x, ...xs]) {
            x + sum(xs)
        }
    "});

    let program_symbols = ast
        .symbol_table
        .clone()
        .expect("Program to have a symbol_table");

    assert_eq!(
        program_symbols.borrow().get_by_name("sum"),
        Some(SymbolInfo {
            id: SymbolId::new(4),
            name: "sum".to_string(),
            symbol_type: SymbolType::Function,
            defined_at: Some(Span::new(
                LineColumn { line: 0, column: 3 },
                LineColumn { line: 0, column: 6 }
            )),
            used: true,
            ..Default::default()
        })
    );
}

#[test]
fn should_collect_function_arguments_of_multiple_fn_definitions() {
    let ast = analyze!(indoc! {"
        fn factorial(0, acc) { acc }
        fn factorial(n, acc) { return rec factorial(n - 1, n * acc); }
    "});

    let program_symbols = ast
        .symbol_table
        .clone()
        .expect("Program to have a symbol_table");

    assert_eq!(
        program_symbols.borrow().get_by_name("factorial"),
        Some(SymbolInfo {
            id: SymbolId::new(5),
            name: "factorial".to_string(),
            symbol_type: SymbolType::Function,
            defined_at: Some(Span::new(
                LineColumn { line: 1, column: 4 },
                LineColumn {
                    line: 1,
                    column: 13
                }
            )),
            used: true,
            ..Default::default()
        })
    );
}

#[test]
fn should_collect_function_arguments_with_enum_extraction() {
    let ast = analyze!(indoc! {"
        enum Option {
            Some(value),
            None,
        }

        fn unwrap(Option::None) { panic(\"Cannot unwrap None\"); }
        fn unwrap(Option::Some(value)) { value }
    "});

    let program_symbols = ast
        .symbol_table
        .clone()
        .expect("Program to have a symbol_table");

    assert_eq!(
        program_symbols.borrow().get_by_name("unwrap"),
        Some(SymbolInfo {
            id: SymbolId::new(5),
            name: "unwrap".to_string(),
            symbol_type: SymbolType::Function,
            defined_at: Some(Span::new(
                LineColumn { line: 6, column: 4 },
                LineColumn {
                    line: 6,
                    column: 10
                }
            )),
            ..Default::default()
        })
    );
}
