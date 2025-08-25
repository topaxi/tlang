use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_parser::Parser;
use tlang_semantics::{analyzer::SemanticAnalyzer, diagnostic::Diagnostic};
use tlang_span::{LineColumn, NodeId, Span};
use tlang_symbols::{SymbolId, SymbolInfo, SymbolType};

mod common;

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
            Ok(_) => (analyzer, ast),
            Err(diagnostics) => panic!("Expected no error diagnostics, got {:#?}", diagnostics),
        }
    }};
}

macro_rules! analyze_diag {
    ($source:expr) => {{ analyze_diag!($source, &vec![]) }};

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
        vec![Diagnostic::error(
            "Use of undeclared variable `a`",
            Span::new((0, 0), (0, 1)),
        )]
    );
}

#[test]
fn test_should_error_on_undefined_symbol_in_variable_declaration() {
    let diagnostics = analyze_diag!("let a = b;");

    assert_eq!(
        diagnostics[..1],
        vec![Diagnostic::error(
            "Use of undeclared variable `b`",
            Span::new((0, 8), (0, 9)),
        )]
    );
}

#[test]
fn test_should_error_on_undefined_functions() {
    let diagnostics = analyze_diag!(indoc! {"
        b();
        b(1);
    "});

    assert_eq!(
        diagnostics,
        vec![
            Diagnostic::error(
                "Use of undeclared function `b` with arity 0",
                Span::new((0, 0), (0, 1)),
            ),
            Diagnostic::error(
                "Use of undeclared function `b` with arity 1",
                Span::new((1, 1), (1, 2)),
            )
        ]
    );
}

#[test]
fn test_should_error_on_self_referencing_symbol() {
    let diagnostics = analyze_diag!("let a = a;");

    assert_eq!(
        diagnostics[..1],
        vec![Diagnostic::error(
            "Use of undeclared variable `a`",
            Span::new((0, 8), (0, 9)),
        )]
    );
}

#[test]
fn test_should_allow_shadowing_of_single_variable() {
    let (analyzer, ast) = analyze!(indoc! {"
        let a = 1;
        let a = 2;
    "});

    let program_symbols = analyzer
        .get_symbol_table(ast.id)
        .expect("Program to have a symbol_table")
        .clone();

    assert_eq!(
        program_symbols.borrow().get_by_name("a"),
        vec![
            SymbolInfo {
                node_id: Some(NodeId::new(3)),
                hir_id: None,
                id: SymbolId::new(1),
                name: "a".into(),
                symbol_type: SymbolType::Variable,
                defined_at: Span::new((0, 4), (0, 5)),
                scope_start: LineColumn {
                    line: 0,
                    column: 10
                },
                declared: true,
                temp: false,
                builtin: false,
                used: false,
            },
            SymbolInfo {
                node_id: Some(NodeId::new(6)),
                hir_id: None,
                id: SymbolId::new(2),
                name: "a".into(),
                symbol_type: SymbolType::Variable,
                defined_at: Span::new((1, 5), (1, 6)),
                scope_start: LineColumn {
                    line: 1,
                    column: 11
                },
                declared: true,
                temp: false,
                builtin: false,
                used: false,
            },
        ]
    );
}

#[test]
fn test_should_allow_shadowing_of_single_variable_with_self_reference() {
    let (analyzer, ast) = analyze!(indoc! {"
        let a = 1;
        let a = a + 1;
    "});

    let program_symbols = analyzer
        .get_symbol_table(ast.id)
        .expect("Program to have a symbol_table")
        .clone();

    assert_eq!(
        program_symbols
            .borrow()
            .get_by_name("a")
            .iter()
            .find(|s| s.id == SymbolId::new(1))
            .cloned(),
        Some(SymbolInfo {
            node_id: Some(NodeId::new(3)),
            hir_id: None,
            id: SymbolId::new(1),
            name: "a".into(),
            symbol_type: SymbolType::Variable,
            defined_at: Span::new((0, 4), (0, 5)),
            scope_start: LineColumn {
                line: 0,
                column: 10
            },
            declared: true,
            temp: false,
            builtin: false,
            used: true,
        })
    );
    assert_eq!(
        program_symbols
            .borrow()
            .get_by_name("a")
            .iter()
            .find(|s| s.id == SymbolId::new(2))
            .cloned(),
        Some(SymbolInfo {
            node_id: Some(NodeId::new(6)),
            hir_id: None,
            id: SymbolId::new(2),
            name: "a".into(),
            symbol_type: SymbolType::Variable,
            defined_at: Span::new((1, 5), (1, 6)),
            scope_start: LineColumn {
                line: 1,
                column: 15
            },
            declared: true,
            temp: false,
            builtin: false,
            used: false,
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
        vec![Diagnostic::error(
            "Use of undeclared variable `c`, did you mean the parameter `a`?",
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
fn should_not_allow_using_variables_from_outer_function_scope_before_declaration() {
    let diagnostics = analyze_diag!(indoc! {"
        fn add(a, b) {
            c + a + b
        }

        let c = 1;
    "});

    assert_eq!(
        diagnostics,
        vec![
            Diagnostic::error(
                "Use of undeclared variable `c`, did you mean the variable `c`?",
                Span::new((1, 5), (1, 6)),
            ),
            Diagnostic::warn("Unused function `add/2`", Span::new((0, 3), (0, 6)),),
            Diagnostic::warn(
                "Unused variable `c`, if this is intentional, prefix the name with an underscore: `_c`",
                Span::new((4, 5), (4, 6))
            ),
        ]
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
            Diagnostic::warn(
                "Unused variable `a`, if this is intentional, prefix the name with an underscore: `_a`",
                Span::new((0, 4), (0, 5)),
            ),
            Diagnostic::warn(
                "Unused variable `b`, if this is intentional, prefix the name with an underscore: `_b`",
                Span::new((1, 5), (1, 6)),
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
            Diagnostic::warn(
                "Unused parameter `a`, if this is intentional, prefix the name with an underscore: `_a`",
                Span::new((0, 7), (0, 8)),
            ),
            Diagnostic::warn(
                "Unused parameter `b`, if this is intentional, prefix the name with an underscore: `_b`",
                Span::new((0, 10), (0, 11)),
            ),
            Diagnostic::warn(
                "Unused variable `c`, if this is intentional, prefix the name with an underscore: `_c`",
                Span::new((1, 9), (1, 10)),
            ),
            Diagnostic::warn("Unused function `add/2`", Span::new((0, 3), (0, 6)),),
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

        fib(5);
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

        binary_search([1, 2, 3, 4, 5], 3);
    "},
        [
            ("len", SymbolType::Function(1)),
            ("floor", SymbolType::Function(1))
        ]
    );
    assert_eq!(diagnostics, vec![]);
}
