use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_ast::{
    NodeId,
    node::{ExprKind, StmtKind},
    symbols::{SymbolId, SymbolInfo, SymbolType},
};
use tlang_parser::Parser;
use tlang_semantics::analyzer::SemanticAnalyzer;
use tlang_span::{LineColumn, Span};

macro_rules! analyze {
    ($source:expr) => {{
        let mut parser = Parser::from_source($source);
        let mut ast = parser.parse().unwrap();
        let mut analyzer = SemanticAnalyzer::default();
        analyzer.add_builtin_symbols(&[("panic", SymbolType::Function(1))]);
        match analyzer.analyze(&mut ast) {
            Ok(_) => (analyzer, ast),
            Err(diagnostics) => panic!("Expected no diagnostics, got {:#?}", diagnostics),
        }
    }};
}

#[test]
fn test_analyze_variable_declaration() {
    let (analyzer, ast) = analyze!("let a = 1;");

    let symbol_info = analyzer
        .get_symbol_table(ast.id)
        .unwrap()
        .borrow()
        .get_by_name("a");

    assert_eq!(
        symbol_info,
        vec![SymbolInfo {
            node_id: Some(NodeId::new(2)),
            hir_id: None,
            id: SymbolId::new(2),
            name: "a".into(),
            symbol_type: SymbolType::Variable,
            defined_at: Span::new(
                LineColumn { line: 0, column: 4 },
                LineColumn { line: 0, column: 5 }
            ),
            temp: false,
            builtin: false,
            used: false,
        }]
    );
}

#[test]
fn test_block_scope() {
    let (analyzer, ast) = analyze!(
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

    let program_symbols = analyzer
        .get_symbol_table(ast.id)
        .clone()
        .expect("Program to have a symbol_table");

    assert_eq!(
        program_symbols.borrow().get_by_name("a"),
        vec![SymbolInfo {
            node_id: Some(NodeId::new(2)),
            hir_id: None,
            id: SymbolId::new(2),
            name: "a".into(),
            symbol_type: SymbolType::Variable,
            defined_at: Span::new(
                LineColumn {
                    line: 1,
                    column: 13
                },
                LineColumn {
                    line: 1,
                    column: 14
                }
            ),
            temp: false,
            builtin: false,
            used: false
        }]
    );
    assert!(program_symbols.borrow().get_by_name("b").is_empty());
    assert!(program_symbols.borrow().get_by_name("c").is_empty());

    let block1 = match ast.statements[1].kind {
        StmtKind::Expr(ref expr) => match &expr.kind {
            ExprKind::Block(block) => block,
            _ => panic!("Expected block {:?}", expr.kind),
        },
        _ => panic!("Expected expression statement {:?}", ast.statements[1].kind),
    };

    let block1_symbols = analyzer
        .get_symbol_table(block1.id)
        .expect("Expected block 1 to have a symbol_table")
        .clone();

    assert_eq!(
        block1_symbols.borrow().get_by_name("a"),
        vec![SymbolInfo {
            node_id: Some(NodeId::new(2)),
            hir_id: None,
            id: SymbolId::new(2),
            name: "a".into(),
            symbol_type: SymbolType::Variable,
            defined_at: Span::new(
                LineColumn {
                    line: 1,
                    column: 13
                },
                LineColumn {
                    line: 1,
                    column: 14
                }
            ),
            temp: false,
            builtin: false,
            used: false
        }]
    );
    assert_eq!(
        block1_symbols.borrow().get_by_name("b"),
        vec![SymbolInfo {
            node_id: Some(NodeId::new(7)),
            hir_id: None,
            id: SymbolId::new(3),
            name: "b".into(),
            symbol_type: SymbolType::Variable,
            defined_at: Span::new(
                LineColumn {
                    line: 3,
                    column: 17
                },
                LineColumn {
                    line: 3,
                    column: 18
                }
            ),
            temp: false,
            builtin: false,
            used: false
        }]
    );
    assert!(block1_symbols.borrow().get_by_name("c").is_empty());

    let block2 = match block1.statements[1].kind {
        StmtKind::Expr(ref expr) => match &expr.kind {
            ExprKind::Block(block) => block,
            _ => panic!("Expected block {:?}", expr.kind),
        },
        _ => panic!(
            "Expected expression statement {:?}",
            block1.statements[1].kind
        ),
    };

    let block2_symbols = analyzer
        .get_symbol_table(block2.id)
        .expect("Expected block 2 to have a symbol_table")
        .clone();

    assert_eq!(
        block2_symbols.borrow().get_by_name("a"),
        vec![SymbolInfo {
            node_id: Some(NodeId::new(2)),
            hir_id: None,
            id: SymbolId::new(2),
            name: "a".into(),
            symbol_type: SymbolType::Variable,
            defined_at: Span::new(
                LineColumn {
                    line: 1,
                    column: 13
                },
                LineColumn {
                    line: 1,
                    column: 14
                }
            ),
            temp: false,
            builtin: false,
            used: false
        }]
    );
    assert_eq!(
        block2_symbols.borrow().get_by_name("b"),
        vec![SymbolInfo {
            node_id: Some(NodeId::new(7)),
            hir_id: None,
            id: SymbolId::new(3),
            name: "b".into(),
            symbol_type: SymbolType::Variable,
            defined_at: Span::new(
                LineColumn {
                    line: 3,
                    column: 17
                },
                LineColumn {
                    line: 3,
                    column: 18
                }
            ),
            temp: false,
            builtin: false,
            used: false
        }]
    );
    assert_eq!(
        block2_symbols.borrow().get_by_name("c"),
        vec![SymbolInfo {
            node_id: Some(NodeId::new(12)),
            hir_id: None,
            id: SymbolId::new(4),
            name: "c".into(),
            symbol_type: SymbolType::Variable,
            defined_at: Span::new(
                LineColumn {
                    line: 5,
                    column: 21
                },
                LineColumn {
                    line: 5,
                    column: 22
                }
            ),
            temp: false,
            builtin: false,
            used: false
        }]
    );
}

#[test]
fn test_should_collect_function_definitions() {
    let (analyzer, ast) = analyze!(indoc! {"
        fn add(a, b) {
            a + b
        }
    "});

    let program_symbols = analyzer
        .get_symbol_table(ast.id)
        .expect("Program to have a symbol_table")
        .clone();

    assert_eq!(
        program_symbols.borrow().get_by_name("add"),
        vec![SymbolInfo {
            node_id: Some(NodeId::new(10)),
            hir_id: None,
            id: SymbolId::new(2),
            name: "add".into(),
            symbol_type: SymbolType::Function(2),
            defined_at: Span::new(
                LineColumn { line: 0, column: 3 },
                LineColumn { line: 0, column: 6 }
            ),
            temp: false,
            builtin: false,
            used: false
        }]
    );
}

#[test]
fn test_should_collect_list_destructuring_symbols_in_function_arguments() {
    let (analyzer, ast) = analyze!(indoc! {"
        fn add([a, b]) {
            a + b
        }
    "});

    let program_symbols = analyzer
        .get_symbol_table(ast.id)
        .expect("Program to have a symbol_table")
        .clone();

    assert_eq!(
        program_symbols.borrow().get_by_name("add"),
        vec![SymbolInfo {
            node_id: Some(NodeId::new(11)),
            hir_id: None,
            id: SymbolId::new(2),
            name: "add".into(),
            symbol_type: SymbolType::Function(1),
            defined_at: Span::new(
                LineColumn { line: 0, column: 3 },
                LineColumn { line: 0, column: 6 }
            ),
            temp: false,
            builtin: false,
            used: false
        }]
    );
}

#[test]
fn test_should_collect_list_destructuring_with_rest_symbols_in_function_arguments() {
    let (analyzer, ast) = analyze!(indoc! {"
        fn sum([x, ...xs]) {
            x + sum(xs)
        }
    "});

    let program_symbols = analyzer
        .get_symbol_table(ast.id)
        .expect("Program to have a symbol_table")
        .clone();

    assert_eq!(
        program_symbols.borrow().get_by_name("sum"),
        vec![SymbolInfo {
            node_id: Some(NodeId::new(14)),
            hir_id: None,
            id: SymbolId::new(2),
            name: "sum".into(),
            symbol_type: SymbolType::Function(1),
            defined_at: Span::new(
                LineColumn { line: 0, column: 3 },
                LineColumn { line: 0, column: 6 }
            ),
            temp: false,
            builtin: false,
            used: true,
        }]
    );
}

#[test]
fn should_collect_function_arguments_of_multiple_fn_definitions() {
    let (analyzer, ast) = analyze!(indoc! {"
        fn factorial(0, acc) { acc }
        fn factorial(n, acc) { return rec factorial(n - 1, n * acc); }
    "});

    let program_symbols = analyzer
        .get_symbol_table(ast.id)
        .expect("Program to have a symbol_table")
        .clone();

    assert_eq!(
        program_symbols.borrow().get_by_name("factorial"),
        vec![
            SymbolInfo {
                node_id: Some(NodeId::new(8)),
                hir_id: None,
                id: SymbolId::new(2),
                name: "factorial".into(),
                symbol_type: SymbolType::Function(2),
                defined_at: Span::new(
                    LineColumn { line: 0, column: 3 },
                    LineColumn {
                        line: 0,
                        column: 12
                    }
                ),
                temp: false,
                builtin: false,
                used: true,
            },
            SymbolInfo {
                node_id: Some(NodeId::new(23)),
                hir_id: None,
                id: SymbolId::new(4),
                name: "factorial".into(),
                symbol_type: SymbolType::Function(2),
                defined_at: Span::new(
                    LineColumn { line: 1, column: 4 },
                    LineColumn {
                        line: 1,
                        column: 13
                    }
                ),
                temp: false,
                builtin: false,
                used: true,
            }
        ]
    );
}

#[test]
fn should_collect_function_arguments_with_enum_extraction() {
    let (analyzer, ast) = analyze!(indoc! {"
        enum Option {
            Some(value),
            None,
        }

        fn unwrap(Option::None) { panic(\"Cannot unwrap None\"); }
        fn unwrap(Option::Some(value)) { value }
    "});

    let program_symbols = analyzer
        .get_symbol_table(ast.id)
        .expect("Program to have a symbol_table")
        .clone();

    assert_eq!(
        program_symbols.borrow().get_by_name("unwrap"),
        vec![
            SymbolInfo {
                node_id: Some(NodeId::new(14)),
                hir_id: None,
                id: SymbolId::new(5),
                name: "unwrap".into(),
                symbol_type: SymbolType::Function(1),
                defined_at: Span::new(
                    LineColumn { line: 5, column: 4 },
                    LineColumn {
                        line: 5,
                        column: 10
                    }
                ),
                temp: false,
                builtin: false,
                used: false
            },
            SymbolInfo {
                node_id: Some(NodeId::new(21)),
                hir_id: None,
                id: SymbolId::new(6),
                name: "unwrap".into(),
                symbol_type: SymbolType::Function(1),
                defined_at: Span::new(
                    LineColumn { line: 6, column: 4 },
                    LineColumn {
                        line: 6,
                        column: 10
                    }
                ),
                temp: false,
                builtin: false,
                used: false
            }
        ]
    );
}

#[test]
fn should_warn_if_multiple_functions_with_different_arity_are_unused() {
    let (analyzer, ast) = analyze!(indoc! {"
        fn used_fn() {}
        fn used_fn(not_used) {}

        used_fn();
    "});

    let program_symbols = analyzer
        .get_symbol_table(ast.id)
        .expect("Program to have a symbol_table")
        .clone();

    assert_eq!(
        program_symbols.borrow().get_by_name("used_fn"),
        vec![
            SymbolInfo {
                node_id: Some(NodeId::new(4)),
                hir_id: None,
                id: SymbolId::new(2),
                name: "used_fn".into(),
                symbol_type: SymbolType::Function(0),
                defined_at: Span::new(
                    LineColumn { line: 0, column: 3 },
                    LineColumn {
                        line: 0,
                        column: 10
                    }
                ),
                temp: false,
                builtin: false,
                used: true,
            },
            SymbolInfo {
                node_id: Some(NodeId::new(8)),
                hir_id: None,
                id: SymbolId::new(3),
                name: "used_fn".into(),
                symbol_type: SymbolType::Function(1),
                defined_at: Span::new(
                    LineColumn { line: 1, column: 4 },
                    LineColumn {
                        line: 1,
                        column: 11
                    }
                ),
                temp: false,
                builtin: false,
                used: false,
            }
        ]
    );
}
