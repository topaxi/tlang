use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_parser::Parser;
use tlang_semantics::{
    SemanticAnalysisContext, SemanticAnalysisPass,
    analyzer::SemanticAnalyzer,
    diagnostic::Diagnostic,
    passes::{DeclarationAnalyzer, VariableUsageValidator},
};
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

#[test]
fn test_should_warn_on_invalid_escape_sequences() {
    let diagnostics = analyze_diag!(
        r#"
        let invalid_string = "Unknown escape: \q";
        let another_invalid = "Bad char: \x";
        let valid_string = "Good escape: \n";
        let invalid_char = '\z';
    "#
    );

    // Filter for only escape sequence warnings
    let escape_warnings: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.message().contains("Unknown escape sequence"))
        .collect();

    // Should get warnings for the invalid escape sequences
    assert_eq!(escape_warnings.len(), 3);

    // Check that the warnings are for unknown escape sequences
    assert!(
        escape_warnings[0]
            .message()
            .contains("Unknown escape sequence")
    );
    assert!(escape_warnings[0].message().contains("\\q"));
    assert!(escape_warnings[0].is_warning());

    assert!(
        escape_warnings[1]
            .message()
            .contains("Unknown escape sequence")
    );
    assert!(escape_warnings[1].message().contains("\\x"));
    assert!(escape_warnings[1].is_warning());

    assert!(
        escape_warnings[2]
            .message()
            .contains("Unknown escape sequence")
    );
    assert!(escape_warnings[2].message().contains("\\z"));
    assert!(escape_warnings[2].is_warning());
}

#[test]
fn test_should_not_warn_on_valid_escape_sequences() {
    let diagnostics = analyze_diag!(
        r#"
        let string1 = "Quote: \"Hello\"";
        let string2 = "Backslash: \\";
        let string3 = "Newline: \n";
        let string4 = "Tab: \t";
        let string5 = "Carriage: \r";
        let string6 = "Null: \0";
        let char1 = '\'';
        let char2 = '\\';
    "#
    );

    // Filter for only escape sequence warnings
    let escape_warnings: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.message().contains("Unknown escape sequence")
                || d.message().contains("Backslash at end")
        })
        .collect();

    // Should not get any warnings for valid escape sequences
    assert_eq!(escape_warnings.len(), 0);
}

/// Test that demonstrates the VariableUsageValidator can be used independently
#[test]
fn test_variable_usage_validator_independent_usage() {
    // Parse a simple code snippet with unused variables
    let source = indoc! {"
        let unused_var = 42;
        let used_var = 24;
        let _unused_var = 99; // Should not be reported due to underscore prefix
        used_var;
    "};

    let mut parser = Parser::from_source(source);
    let ast = parser.parse().unwrap();

    // First run declaration analysis to collect declarations
    let mut declaration_analyzer = DeclarationAnalyzer::default();
    let mut context = SemanticAnalysisContext::default();
    declaration_analyzer.analyze(&ast, &mut context, true);

    // Now run the VariableUsageValidator independently
    let mut validator = VariableUsageValidator::default();
    validator.analyze(&ast, &mut context, true);

    let diagnostics = context.get_diagnostics();

    // Should only report the unused variable (not the used one or the prefixed one)
    assert_eq!(diagnostics.len(), 1);
    assert!(
        diagnostics[0]
            .message()
            .contains("Unused variable `unused_var`")
    );
    assert!(diagnostics[0].is_warning());
}

/// Test that demonstrates undeclared variable reporting works independently
#[test]
fn test_variable_usage_validator_undeclared_variable() {
    // Parse a simple code snippet with an undeclared variable
    let source = indoc! {"
        let similar_name = 42;
        similar_name; // Use the variable properly
        similar_nam; // Missing 'e' - should trigger did_you_mean
    "};

    let mut parser = Parser::from_source(source);
    let ast = parser.parse().unwrap();

    // First run declaration analysis to collect declarations
    let mut declaration_analyzer = DeclarationAnalyzer::default();
    let mut context = SemanticAnalysisContext::default();
    declaration_analyzer.analyze(&ast, &mut context, true);

    // Now run the VariableUsageValidator independently
    let mut validator = VariableUsageValidator::default();
    validator.analyze(&ast, &mut context, true);

    let diagnostics = context.get_diagnostics();

    // Should report with suggestion
    assert_eq!(diagnostics.len(), 1);
    assert!(
        diagnostics[0]
            .message()
            .contains("Use of undeclared variable `similar_nam`")
    );
    assert!(diagnostics[0].message().contains("did you mean"));
    assert!(diagnostics[0].message().contains("similar_name"));
    assert!(diagnostics[0].is_error());
}

#[test]
fn test_tail_call_position_validator_debug_let_binding() {
    let source = indoc! {r#"
        fn test(x) {
            let y = rec test(x - 1);
            y
        }
    "#};
    
    let diagnostics = analyze_diag!(source);
    
    println!("All diagnostics: {:?}", diagnostics);
    
    // Filter for tail call position errors
    let tail_call_errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.message().contains("not in tail position"))
        .collect();
    
    assert!(
        !tail_call_errors.is_empty(),
        "Should have tail call position errors for let binding case, but got none"
    );
}

#[test]
fn test_tail_call_position_validator_debug_not_at_end() {
    let source = indoc! {r#"
        fn test(x) {
            rec test(x - 1);
            42
        }
    "#};
    
    let diagnostics = analyze_diag!(source);
    
    println!("All diagnostics: {:?}", diagnostics);
    
    // Filter for tail call position errors
    let tail_call_errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.message().contains("not in tail position"))
        .collect();
    
    assert!(
        !tail_call_errors.is_empty(),
        "Should have tail call position errors for non-tail position case, but got none"
    );
}

#[test]
fn test_tail_call_position_validator_valid_cases() {
    // Test valid tail call positions
    let test_cases = vec![
        // Valid: tail call at end of function
        indoc! {r#"
            fn fib(n) { rec fib(n - 1) }
        "#},
        // Valid: tail call in if-else expression
        indoc! {r#"
            fn test(x) { if x > 0 { rec test(x - 1) } else { 0 } }
        "#},
        // Valid: tail call in match arm
        indoc! {r#"
            fn test(0) { 0 }
            fn test(n) { rec test(n - 1) }
        "#},
        // Valid: tail call after return
        indoc! {r#"
            fn test(x) { return rec test(x - 1); }
        "#},
        // Valid: tail call in block at end of function
        indoc! {r#"
            fn test(x) {
                let y = x + 1;
                rec test(y)
            }
        "#},
    ];

    for (i, test_case) in test_cases.iter().enumerate() {
        let diagnostics = analyze_diag!(test_case);
        
        // Filter for tail call position errors
        let tail_call_errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message().contains("not in tail position"))
            .collect();
        
        assert!(
            tail_call_errors.is_empty(),
            "Test case {} should not have tail call position errors, but got: {:?}",
            i,
            tail_call_errors
        );
    }
}

#[test]
fn test_tail_call_position_validator_invalid_cases() {
    // Test invalid tail call positions
    let test_cases = vec![
        // Invalid: tail call in binary operation
        indoc! {r#"
            fn test(x) { 1 + rec test(x - 1) }
        "#},
        // Invalid: tail call as function argument  
        indoc! {r#"
            fn test(x) { test(rec test(x - 1)) }
        "#},
        // Invalid: tail call in let binding
        indoc! {r#"
            fn test(x) {
                let y = rec test(x - 1);
                y
            }
        "#},
        // Invalid: tail call not at end of block
        indoc! {r#"
            fn test(x) {
                rec test(x - 1);
                42
            }
        "#},
    ];

    for (i, test_case) in test_cases.iter().enumerate() {
        let diagnostics = analyze_diag!(test_case);
        
        // Filter for tail call position errors
        let tail_call_errors: Vec<_> = diagnostics
            .iter()
            .filter(|d| d.message().contains("not in tail position"))
            .collect();
        
        assert!(
            !tail_call_errors.is_empty(),
            "Test case {} should have tail call position errors, but got none. All diagnostics: {:?}",
            i,
            diagnostics
        );
        
        assert!(
            tail_call_errors[0].is_error(),
            "Tail call position validation should be an error"
        );
    }
}
