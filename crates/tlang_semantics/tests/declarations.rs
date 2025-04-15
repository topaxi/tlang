use indoc::indoc;
use pretty_assertions::assert_matches;
use tlang_ast::{
    node::{ExprKind, StmtKind},
    symbols::{SymbolInfo, SymbolType},
};
use tlang_parser::Parser;
use tlang_semantics::analyzer::SemanticAnalyzer;

macro_rules! analyze {
    ($source:expr) => {{
        let mut parser = Parser::from_source($source);
        let mut ast = parser.parse().unwrap();
        let mut analyzer = SemanticAnalyzer::default();
        analyzer.add_builtin_symbols(&[("panic", SymbolType::Function)]);
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

    assert_matches!(
        symbol_info,
        Some(SymbolInfo {
            symbol_type: SymbolType::Variable,
            used: false,
            ..
        })
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

    assert_matches!(
        program_symbols.borrow().get_by_name("a"),
        Some(SymbolInfo {
            symbol_type: SymbolType::Variable,
            used: false,
            ..
        })
    );
    assert_matches!(program_symbols.borrow().get_by_name("b"), None);
    assert_matches!(program_symbols.borrow().get_by_name("c"), None);

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

    assert_matches!(
        block1_symbols.borrow().get_by_name("a"),
        Some(SymbolInfo {
            symbol_type: SymbolType::Variable,
            used: false,
            ..
        })
    );
    assert_matches!(
        block1_symbols.borrow().get_by_name("b"),
        Some(SymbolInfo {
            symbol_type: SymbolType::Variable,
            used: false,
            ..
        })
    );
    assert_matches!(block1_symbols.borrow().get_by_name("c"), None);

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

    assert_matches!(
        block2_symbols.borrow().get_by_name("a"),
        Some(SymbolInfo {
            symbol_type: SymbolType::Variable,
            used: false,
            ..
        })
    );
    assert_matches!(
        block2_symbols.borrow().get_by_name("b"),
        Some(SymbolInfo {
            symbol_type: SymbolType::Variable,
            used: false,
            ..
        })
    );
    assert_matches!(
        block2_symbols.borrow().get_by_name("c"),
        Some(SymbolInfo {
            symbol_type: SymbolType::Variable,
            used: false,
            ..
        })
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

    assert_matches!(
        program_symbols.borrow().get_by_name("add"),
        Some(SymbolInfo {
            symbol_type: SymbolType::Function,
            used: false,
            ..
        })
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

    assert_matches!(
        program_symbols.borrow().get_by_name("add"),
        Some(SymbolInfo {
            symbol_type: SymbolType::Function,
            used: false,
            ..
        })
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

    assert_matches!(
        program_symbols.borrow().get_by_name("sum"),
        Some(SymbolInfo {
            symbol_type: SymbolType::Function,
            used: true,
            ..
        })
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

    assert_matches!(
        program_symbols.borrow().get_by_name("factorial"),
        Some(SymbolInfo {
            symbol_type: SymbolType::Function,
            used: true,
            ..
        })
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

    assert_matches!(
        program_symbols.borrow().get_by_name("unwrap"),
        Some(SymbolInfo {
            symbol_type: SymbolType::Function,
            used: false,
            ..
        })
    );
}
