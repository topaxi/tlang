use crate::analyzer::SemanticAnalyzer;
use indoc::indoc;
use tlang_ast::{
    node::AstNode,
    symbols::{SymbolId, SymbolInfo, SymbolType},
};
use tlang_parser::parser::Parser;

macro_rules! analyze {
    ($source:expr) => {{
        let mut parser = Parser::from_source($source);
        let mut ast = parser.parse();
        let mut analyzer = SemanticAnalyzer::default();
        analyzer.analyze(&mut ast);
        ast
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
        })
    );
    assert_eq!(program_symbols.borrow().get_by_name("b"), None);
    assert_eq!(program_symbols.borrow().get_by_name("c"), None);

    let block1 = match ast.ast_node {
        AstNode::Program(ref nodes) => match nodes[1].ast_node {
            AstNode::ExpressionStatement(ref node) => match node.ast_node {
                AstNode::Block(_, _) => node,
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
        })
    );
    assert_eq!(
        block1_symbols.borrow().get_by_name("b"),
        Some(SymbolInfo {
            id: SymbolId::new(2),
            name: "b".to_string(),
            symbol_type: SymbolType::Variable,
        })
    );
    assert_eq!(block1_symbols.borrow().get_by_name("c"), None);

    let block2 = match block1.ast_node {
        AstNode::Block(ref nodes, _) => match nodes[1].ast_node {
            AstNode::ExpressionStatement(ref node) => match node.ast_node {
                AstNode::Block(_, _) => node,
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
        })
    );
    assert_eq!(
        block2_symbols.borrow().get_by_name("b"),
        Some(SymbolInfo {
            id: SymbolId::new(2),
            name: "b".to_string(),
            symbol_type: SymbolType::Variable,
        })
    );
    assert_eq!(
        block2_symbols.borrow().get_by_name("c"),
        Some(SymbolInfo {
            id: SymbolId::new(3),
            name: "c".to_string(),
            symbol_type: SymbolType::Variable,
        })
    );
}

#[test]
#[should_panic(expected = "Undefined symbol: b")]
fn test_should_panic_on_undefined_symbol() {
    analyze!("let a = b;");
}

#[test]
#[should_panic(expected = "Undefined symbol: a")]
fn test_should_panic_on_self_referencing_symbol() {
    analyze!("let a = a;");
}

#[test]
fn test_should_allow_shadowing_of_single_variable() {
    let ast = analyze!(indoc! {"
        let a = 1;
        let a = 2;
    "});

    let program_symbols = ast
        .symbol_table
        .clone()
        .expect("Program to have a symbol_table");

    assert_eq!(
        program_symbols.borrow().get(SymbolId::new(1)),
        Some(SymbolInfo {
            id: SymbolId::new(1),
            name: "a".to_string(),
            symbol_type: SymbolType::Variable,
        })
    );
    assert_eq!(
        program_symbols.borrow().get(SymbolId::new(2)),
        Some(SymbolInfo {
            id: SymbolId::new(2),
            name: "a".to_string(),
            symbol_type: SymbolType::Variable,
        })
    );
}

#[test]
fn test_should_allow_shadowing_of_single_variable_with_self_reference() {
    let ast = analyze!(indoc! {"
        let a = 1;
        let a = a + 1;
    "});

    let program_symbols = ast
        .symbol_table
        .clone()
        .expect("Program to have a symbol_table");

    assert_eq!(
        program_symbols.borrow().get(SymbolId::new(1)),
        Some(SymbolInfo {
            id: SymbolId::new(1),
            name: "a".to_string(),
            symbol_type: SymbolType::Variable,
        })
    );
    assert_eq!(
        program_symbols.borrow().get(SymbolId::new(2)),
        Some(SymbolInfo {
            id: SymbolId::new(2),
            name: "a".to_string(),
            symbol_type: SymbolType::Variable,
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
        })
    );
}

#[test]
#[should_panic(expected = "Undefined symbol: c")]
fn test_should_panic_on_unused_identifier_in_function_definition() {
    analyze!(indoc! {"
        fn add(a, b) {
            a + b + c
        }
    "});
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
            id: SymbolId::new(2),
            name: "add".to_string(),
            symbol_type: SymbolType::Function,
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
            id: SymbolId::new(2),
            name: "sum".to_string(),
            symbol_type: SymbolType::Function,
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
            id: SymbolId::new(4),
            name: "unwrap".to_string(),
            symbol_type: SymbolType::Function,
        })
    );
}

#[test]
fn should_allow_using_variables_from_outer_function_scope_before_declaration() {
    let ast = analyze!(indoc! {"
        fn add(a, b) {
            c + a + b
        }

        let c = 1;
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
        })
    );

    let (function_node, function_declaration) = match ast.ast_node {
        AstNode::Program(ref nodes) => match &nodes[0].ast_node {
            AstNode::FunctionDeclaration {
                id: _,
                name: _,
                ref declaration,
            } => (nodes[0].clone(), declaration.clone()),
            _ => panic!("Expected function declaration {:?}", nodes[0].ast_node),
        },
        _ => panic!("Expected program {:?}", ast.ast_node),
    };

    // Verify that c is within the scope of the function arguments
    assert_eq!(
        function_node
            .symbol_table
            .clone()
            .unwrap()
            .borrow()
            .get_by_name("c"),
        Some(SymbolInfo {
            id: SymbolId::new(4),
            name: "c".to_string(),
            symbol_type: SymbolType::Variable,
        })
    );

    // Verify that c is within the scope of the function body
    assert_eq!(
        function_declaration
            .body
            .symbol_table
            .clone()
            .unwrap()
            .borrow()
            .get_by_name("c"),
        Some(SymbolInfo {
            id: SymbolId::new(4),
            name: "c".to_string(),
            symbol_type: SymbolType::Variable,
        })
    );
}
