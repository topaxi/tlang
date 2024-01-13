use indoc::indoc;
use tlang_ast::{
    node::AstNode,
    symbols::{SymbolId, SymbolInfo, SymbolType},
};
use tlang_parser::parser::Parser;
use tlang_semantics::{
    analyzer::SemanticAnalyzer,
    diagnostic::{Diagnostic, Severity},
};

macro_rules! analyze {
    ($source:expr) => {{
        let mut parser = Parser::from_source($source);
        let mut ast = parser.parse().unwrap();
        let mut analyzer = SemanticAnalyzer::default();
        match analyzer.analyze(&mut ast) {
            Ok(_) => ast,
            Err(diagnostics) => panic!("Expected no diagnostics, got {:#?}", diagnostics),
        }
    }};
}

macro_rules! analyze_diag {
    ($source:expr) => {{
        let mut parser = Parser::from_source($source);
        let mut ast = parser.parse().unwrap();
        let mut analyzer = SemanticAnalyzer::default();
        match analyzer.analyze(&mut ast) {
            Ok(_) => vec![],
            Err(diagnostics) => diagnostics,
        }
    }};
}

#[test]
fn test_should_error_on_undefined_symbol() {
    let diagnostics = analyze_diag!("a;");

    assert_eq!(
        diagnostics,
        vec![Diagnostic::new(
            "Use of undeclared variable `a`".to_string(),
            Severity::Error
        )]
    );
}

#[test]
fn test_should_error_on_undefined_symbol_in_variable_declaration() {
    let diagnostics = analyze_diag!("let a = b;");

    assert_eq!(
        diagnostics,
        vec![Diagnostic::new(
            "Use of undeclared variable `b`".to_string(),
            Severity::Error
        )]
    );
}

#[test]
fn test_should_error_on_undefined_function() {
    let diagnostics = analyze_diag!("b();");

    assert_eq!(
        diagnostics,
        vec![Diagnostic::new(
            "Use of undeclared variable `b`".to_string(),
            Severity::Error
        )]
    );
}

#[test]
fn test_should_error_on_self_referencing_symbol() {
    let diagnostics = analyze_diag!("let a = a;");

    assert_eq!(
        diagnostics,
        vec![Diagnostic::new(
            "Use of undeclared variable `a`".to_string(),
            Severity::Error
        )]
    );
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
fn test_should_error_on_unused_identifier_in_function_definition() {
    let diagnostics = analyze_diag!(indoc! {"
        fn add(a, b) {
            a + b + c
        }
    "});

    assert_eq!(
        diagnostics,
        vec![Diagnostic::new(
            "Use of undeclared variable `c`".to_string(),
            Severity::Error
        )]
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
            AstNode::FunctionSingleDeclaration {
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
