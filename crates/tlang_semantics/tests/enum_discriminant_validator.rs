use tlang_parser::Parser;
use tlang_semantics::analyzer::SemanticAnalyzer;

mod common;

fn analyze(source: &str) -> (Vec<String>, Vec<String>) {
    let mut parser = Parser::from_source(source);
    let (mut ast, _) = parser.parse().unwrap();
    let mut analyzer = SemanticAnalyzer::default();
    let _ = analyzer.analyze(&mut ast);
    let diagnostics = analyzer.get_diagnostics();
    let warnings = diagnostics
        .iter()
        .filter(|d| d.is_warning())
        .map(|d| d.message().to_string())
        .collect();
    let errors = diagnostics
        .iter()
        .filter(|d| d.is_error())
        .map(|d| d.message().to_string())
        .collect();
    (warnings, errors)
}

// ── Valid discriminant expressions (no diagnostics) ─────────────────────────

#[test]
fn test_literal_integer_discriminant_ok() {
    let (warnings, errors) = analyze("enum Status { Ok = 200, NotFound = 404, }");
    let relevant_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.contains("discriminant"))
        .collect();
    let relevant_errors: Vec<_> = errors
        .iter()
        .filter(|e| e.contains("discriminant"))
        .collect();
    assert!(
        relevant_warnings.is_empty(),
        "Unexpected warnings: {relevant_warnings:?}"
    );
    assert!(
        relevant_errors.is_empty(),
        "Unexpected errors: {relevant_errors:?}"
    );
}

#[test]
fn test_negative_integer_discriminant_ok() {
    let (warnings, errors) = analyze("enum Temp { Cold = -10, Hot = 40, }");
    let relevant_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.contains("discriminant"))
        .collect();
    let relevant_errors: Vec<_> = errors
        .iter()
        .filter(|e| e.contains("discriminant"))
        .collect();
    assert!(
        relevant_warnings.is_empty(),
        "Unexpected warnings: {relevant_warnings:?}"
    );
    assert!(
        relevant_errors.is_empty(),
        "Unexpected errors: {relevant_errors:?}"
    );
}

#[test]
fn test_string_discriminant_ok() {
    let (warnings, errors) = analyze(r#"enum Dir { North = "N", South = "S", }"#);
    let relevant_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.contains("discriminant"))
        .collect();
    let relevant_errors: Vec<_> = errors
        .iter()
        .filter(|e| e.contains("discriminant"))
        .collect();
    assert!(
        relevant_warnings.is_empty(),
        "Unexpected warnings: {relevant_warnings:?}"
    );
    assert!(
        relevant_errors.is_empty(),
        "Unexpected errors: {relevant_errors:?}"
    );
}

// ── Duplicate discriminant values within the same enum ──────────────────────

#[test]
fn test_duplicate_integer_discriminant_warns() {
    let (warnings, _errors) = analyze("enum E { A = 1, B = 1, }");
    let dup_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.contains("discriminant"))
        .collect();
    assert_eq!(
        dup_warnings.len(),
        1,
        "Expected exactly one duplicate-discriminant warning, got: {dup_warnings:?}"
    );
    assert!(
        dup_warnings[0].contains("E::B"),
        "Warning should mention the shadowed variant B, got: {}",
        dup_warnings[0]
    );
    assert!(
        dup_warnings[0].contains("E::A"),
        "Warning should mention the previous variant A, got: {}",
        dup_warnings[0]
    );
}

#[test]
fn test_three_duplicate_discriminants_warns_twice() {
    let (warnings, _errors) = analyze("enum E { A = 5, B = 5, C = 5, }");
    let dup_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.contains("discriminant"))
        .collect();
    assert_eq!(
        dup_warnings.len(),
        2,
        "Expected two duplicate-discriminant warnings (B and C), got: {dup_warnings:?}"
    );
}

// ── Non-constant discriminant expressions (error) ───────────────────────────

#[test]
fn test_non_literal_discriminant_errors() {
    let (_warnings, errors) = analyze("enum E { A = 1, B = some_fn(), }");
    let const_errors: Vec<_> = errors
        .iter()
        .filter(|e| e.contains("constant literal"))
        .collect();
    assert_eq!(
        const_errors.len(),
        1,
        "Expected one non-constant-discriminant error, got: {const_errors:?}"
    );
    assert!(
        const_errors[0].contains("E::B"),
        "Error should mention the invalid variant B, got: {}",
        const_errors[0]
    );
}

#[test]
fn test_path_discriminant_errors() {
    let (_warnings, errors) = analyze("enum E { A = 1, B = OTHER_CONST, }");
    let const_errors: Vec<_> = errors
        .iter()
        .filter(|e| e.contains("constant literal"))
        .collect();
    assert_eq!(
        const_errors.len(),
        1,
        "Expected one non-constant-discriminant error, got: {const_errors:?}"
    );
}

#[test]
fn test_binary_expr_discriminant_errors() {
    let (_warnings, errors) = analyze("enum E { A = 1, B = 1 + 2, }");
    let const_errors: Vec<_> = errors
        .iter()
        .filter(|e| e.contains("constant literal"))
        .collect();
    assert_eq!(
        const_errors.len(),
        1,
        "Expected one non-constant-discriminant error for binary expr, got: {const_errors:?}"
    );
    assert!(
        const_errors[0].contains("E::B"),
        "Error should mention the invalid variant B, got: {}",
        const_errors[0]
    );
}
