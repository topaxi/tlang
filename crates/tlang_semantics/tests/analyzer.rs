use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_ast::{
    node::StmtKind,
    span::{LineColumn, Span},
    symbols::{SymbolId, SymbolInfo, SymbolType},
};
use tlang_parser::parser::Parser;
use tlang_semantics::{
    analyzer::SemanticAnalyzer,
    diagnostic::{Diagnostic, Severity},
};

fn create_analyzer(builtin_symbols: &[(&str, SymbolType)]) -> SemanticAnalyzer {
    let mut analyzer = SemanticAnalyzer::default();
    analyzer.add_builtin_symbols(builtin_symbols);
    analyzer
}

macro_rules! analyze {
    ($source:expr) => {{
        let mut parser = Parser::from_source($source);
        let mut ast = parser.parse().unwrap();
        let mut analyzer = $crate::create_analyzer(&[]);
        match analyzer.analyze(&mut ast) {
            Ok(_) => ast,
            Err(diagnostics) => panic!("Expected no error diagnostics, got {:#?}", diagnostics),
        }
    }};
}

macro_rules! analyze_diag {
    ($source:expr) => {{
        analyze_diag!($source, &vec![])
    }};

    ($source:expr, $builtin_symbols:expr) => {{
        let mut parser = Parser::from_source($source);
        let mut ast = parser.parse().unwrap();
        let mut analyzer = $crate::create_analyzer(&$builtin_symbols);
        let _ = analyzer.analyze(&mut ast);
        analyzer.get_diagnostics().to_owned()
    }};
}

#[test]
fn test_should_error_on_undefined_symbol() {
    let diagnostics = analyze_diag!("a;");

    assert_eq!(
        diagnostics,
        vec![Diagnostic::new(
            "Use of undeclared variable `a`".to_string(),
            Severity::Error,
            Span::new(
                LineColumn { line: 0, column: 0 },
                LineColumn { line: 0, column: 1 }
            ),
        )]
    );
}

#[test]
fn test_should_error_on_undefined_symbol_in_variable_declaration() {
    let diagnostics = analyze_diag!("let a = b;");

    assert_eq!(
        diagnostics[..1],
        vec![Diagnostic::new(
            "Use of undeclared variable `b`".to_string(),
            Severity::Error,
            Span::new(
                LineColumn { line: 0, column: 8 },
                LineColumn { line: 0, column: 9 }
            ),
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
            Severity::Error,
            Span::new(
                LineColumn { line: 0, column: 0 },
                LineColumn { line: 0, column: 1 }
            ),
        )]
    );
}

#[test]
fn test_should_error_on_self_referencing_symbol() {
    let diagnostics = analyze_diag!("let a = a;");

    assert_eq!(
        diagnostics[..1],
        vec![Diagnostic::new(
            "Use of undeclared variable `a`".to_string(),
            Severity::Error,
            Span::new(
                LineColumn { line: 0, column: 8 },
                LineColumn { line: 0, column: 9 }
            ),
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
            defined_at: Some(Span::new(
                LineColumn { line: 0, column: 4 },
                LineColumn { line: 0, column: 5 }
            )),
            ..Default::default()
        })
    );
    assert_eq!(
        program_symbols.borrow().get(SymbolId::new(2)),
        Some(SymbolInfo {
            id: SymbolId::new(2),
            name: "a".to_string(),
            symbol_type: SymbolType::Variable,
            defined_at: Some(Span::new(
                LineColumn { line: 1, column: 5 },
                LineColumn { line: 1, column: 6 }
            )),
            ..Default::default()
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
            defined_at: Some(Span::new(
                LineColumn { line: 0, column: 4 },
                LineColumn { line: 0, column: 5 }
            )),
            used: true,
            ..Default::default()
        })
    );
    assert_eq!(
        program_symbols.borrow().get(SymbolId::new(2)),
        Some(SymbolInfo {
            id: SymbolId::new(2),
            name: "a".to_string(),
            symbol_type: SymbolType::Variable,
            defined_at: Some(Span::new(
                LineColumn { line: 1, column: 5 },
                LineColumn { line: 1, column: 6 }
            )),
            used: false,
            ..Default::default()
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
        diagnostics[..1],
        vec![Diagnostic::new(
            "Use of undeclared variable `c`, did you mean the parameter `a`".to_string(),
            Severity::Error,
            Span::new(
                LineColumn {
                    line: 1,
                    column: 13
                },
                LineColumn {
                    line: 1,
                    column: 14
                }
            ),
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
            id: SymbolId::new(1),
            name: "add".to_string(),
            symbol_type: SymbolType::Function,
            defined_at: Some(Span::new(
                LineColumn { line: 0, column: 3 },
                LineColumn { line: 0, column: 6 }
            )),
            ..Default::default()
        })
    );

    let function_declaration = match &ast.statements[0].kind {
        StmtKind::FunctionDeclaration(decl) => decl,
        _ => panic!("Expected function declaration {:?}", ast.statements[0].kind),
    };

    // Verify that c is within the scope of the function arguments
    assert_eq!(
        function_declaration
            .symbol_table
            .clone()
            .unwrap()
            .borrow()
            .get_by_name("c"),
        Some(SymbolInfo {
            id: SymbolId::new(4),
            name: "c".to_string(),
            symbol_type: SymbolType::Variable,
            defined_at: Some(Span::new(
                LineColumn { line: 4, column: 5 },
                LineColumn { line: 4, column: 6 }
            )),
            used: true,
            ..Default::default()
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
            defined_at: Some(Span::new(
                LineColumn { line: 4, column: 5 },
                LineColumn { line: 4, column: 6 }
            )),
            used: true,
            ..Default::default()
        })
    );
}

#[test]
fn should_not_warn_about_used_variables() {
    let diagnostics = analyze_diag!(indoc! {"
        let a = 1;
        a + 1;
    "});
    assert_eq!(diagnostics, vec![]);
}

#[test]
fn should_warn_about_unused_variables() {
    let diagnostics = analyze_diag!(indoc! {"
        let a = 1;
        let b = 2;
    "});
    assert_eq!(
        diagnostics,
        vec![
            // TODO: Might be nicer to have them report in order. This currently happens
            //       due to us reinserting variables in the beginning of the symbol table.
            Diagnostic::new(
                "Unused variable `b`, if this is intentional, prefix the name with an underscore: `_b`".to_string(),
                Severity::Warning,
                Span::new(LineColumn { line: 1, column: 5 }, LineColumn { line: 1, column: 6 }),
            ),
            Diagnostic::new(
                "Unused variable `a`, if this is intentional, prefix the name with an underscore: `_a`".to_string(),
                Severity::Warning,
                Span::new(LineColumn { line: 0, column: 4 }, LineColumn { line: 0, column: 5 }),
            ),
        ],
    );
}

#[test]
fn should_warn_about_unused_function_and_parameters() {
    let diagnostics = analyze_diag!(indoc! {"
        fn add(a, b) {
            let c = 1;
        }
    "});
    assert_eq!(
        diagnostics,
        vec![
            Diagnostic::new(
                "Unused variable `c`, if this is intentional, prefix the name with an underscore: `_c`".to_string(), Severity::Warning,
                Span::new(LineColumn { line: 1, column: 9 }, LineColumn { line: 1, column: 10 }),
            ),
            Diagnostic::new(
                "Unused parameter `a`, if this is intentional, prefix the name with an underscore: `_a`".to_string(),
                Severity::Warning,
                Span::new(LineColumn { line: 0, column: 7 }, LineColumn { line: 0, column: 8 }),
            ),
            Diagnostic::new(
                "Unused parameter `b`, if this is intentional, prefix the name with an underscore: `_b`".to_string(),
                Severity::Warning,
                Span::new(LineColumn { line: 0, column: 10 }, LineColumn { line: 0, column: 11 }),
            ),
            Diagnostic::new(
                "Unused function `add`, if this is intentional, prefix the name with an underscore: `_add`".to_string(),
                Severity::Warning,
                Span::new(LineColumn { line: 0, column: 3 }, LineColumn { line: 0, column: 6 }),
            ),
        ]
    );
}

#[test]
fn should_not_warn_about_used_function_and_parameters() {
    let diagnostics = analyze_diag!(indoc! {"
        fn add(a, b) {
            a + b
        }

        add(1, 2);
    "});
    assert_eq!(diagnostics, vec![]);

    let diagnostic = analyze_diag!(indoc! {"
        fn fib(n) {
            if n < 2 { n }
            else { fib(n - 1) + fib(n - 2) }
        }

        fib(5);
    "});
    assert_eq!(diagnostic, vec![]);

    let diagnostics = analyze_diag!(indoc! {"
        fn factorial(n) { factorial(n, 1) }
        fn factorial(0, acc) { acc }
        fn factorial(n, acc) { rec factorial(n - 1, n * acc) }

        factorial(5);
    "});
    assert_eq!(diagnostics, vec![]);

    let diagnostics = analyze_diag!(indoc! {"
        fn sum([]) { 0 }
        fn sum([head, ...tail]) { head + sum(tail) }

        sum([1, 2, 3]);
    "});
    assert_eq!(diagnostics, vec![]);
}

#[test]
fn should_not_warn_about_unused_variables_prefixed_with_underscore() {
    let diagnostics = analyze_diag!(indoc! {"
        let _a = 1;
        let _b = 2;
    "});
    assert_eq!(diagnostics, vec![]);
}

#[test]
fn should_handle_fn_guard_variables() {
    let diagnostics = analyze_diag!(indoc! {"
        fn fib(n) if n < 2 { n }
        fn fib(n) { fib(n - 1) + fib(n - 2) }
    "});
    assert_eq!(diagnostics, vec![]);

    let diagnostics = analyze_diag!(
        indoc! {"
        fn binary_search(list, target) { binary_search(list, target, 0, len(list) - 1) }
        fn binary_search(_, _, low, high) if low > high; { -1 }
        fn binary_search(list, target, low, high) {
            let mid = floor((low + high) / 2);
            let midValue = list[mid];

            if midValue == target; {
                mid
            } else if midValue < target; {
                binary_search(list, target, mid + 1, high)
            } else {
                binary_search(list, target, low, mid - 1)
            }
        }
    "},
        [
            ("len", SymbolType::Function),
            ("floor", SymbolType::Function)
        ]
    );
    assert_eq!(diagnostics, vec![]);
}
