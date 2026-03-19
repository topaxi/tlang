use indoc::indoc;
use tlang_parser::Parser;
use tlang_semantics::analyzer::SemanticAnalyzer;

mod common;

fn create_analyzer() -> SemanticAnalyzer {
    SemanticAnalyzer::default()
}

macro_rules! analyze_diag {
    ($source:expr) => {{
        let mut parser = Parser::from_source($source);
        let (mut ast, _) = parser.parse().unwrap();
        let mut analyzer = $crate::create_analyzer();
        let _ = analyzer.analyze(&mut ast);
        analyzer.get_diagnostics().to_owned()
    }};
}

#[test]
fn test_consistent_pub_visibility_same_arity() {
    let diagnostics = analyze_diag!(indoc! {"
        pub fn foo(1) { 1 }
        pub fn foo(n) { n }
    "});

    let errors: Vec<_> = diagnostics.iter().filter(|d| d.is_error()).collect();
    assert!(errors.is_empty(), "Expected no errors, got: {errors:#?}");
}

#[test]
fn test_consistent_private_visibility_same_arity() {
    let diagnostics = analyze_diag!(indoc! {"
        fn foo(1) { 1 }
        fn foo(n) { n }
    "});

    let errors: Vec<_> = diagnostics.iter().filter(|d| d.is_error()).collect();
    assert!(errors.is_empty(), "Expected no errors, got: {errors:#?}");
}

#[test]
fn test_inconsistent_visibility_same_arity() {
    let diagnostics = analyze_diag!(indoc! {"
        pub fn bar(1) { 1 }
        fn bar(n) { n }
    "});

    let errors: Vec<_> = diagnostics.iter().filter(|d| d.is_error()).collect();
    assert_eq!(errors.len(), 1, "Expected 1 error, got: {errors:#?}");
    assert!(
        errors[0].message().contains("Inconsistent visibility"),
        "Error should mention inconsistent visibility: {}",
        errors[0].message()
    );
}

#[test]
fn test_different_visibility_different_arity_is_valid() {
    let diagnostics = analyze_diag!(indoc! {"
        pub fn foo() { foo(1) }
        fn foo(x) { x }
    "});

    let errors: Vec<_> = diagnostics.iter().filter(|d| d.is_error()).collect();
    assert!(errors.is_empty(), "Expected no errors, got: {errors:#?}");
}
