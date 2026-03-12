use tlang_parser::Parser;
use tlang_semantics::analyzer::SemanticAnalyzer;

mod common;

fn analyze_for_warnings(source: &str) -> Vec<String> {
    let mut parser = Parser::from_source(source);
    let mut ast = parser.parse().unwrap();
    let mut analyzer = SemanticAnalyzer::default();
    let _ = analyzer.analyze(&mut ast);
    analyzer
        .get_diagnostics()
        .iter()
        .filter(|d| d.is_warning())
        .map(|d| d.message().to_string())
        .collect()
}

fn analyze_has_error(source: &str) -> bool {
    let mut parser = Parser::from_source(source);
    let mut ast = parser.parse().unwrap();
    let mut analyzer = SemanticAnalyzer::default();
    analyzer.analyze(&mut ast).is_err()
}

// ── Valid escape sequences (should produce no warnings) ─────────────────────

#[test]
fn test_valid_double_quote_escape() {
    let warnings = analyze_for_warnings(r#"let s = "hello \"world\"";"#);
    let escape_warnings: Vec<_> = warnings.iter().filter(|w| w.contains("escape")).collect();
    assert!(
        escape_warnings.is_empty(),
        "Unexpected warnings: {escape_warnings:?}"
    );
}

#[test]
fn test_valid_newline_escape() {
    let warnings = analyze_for_warnings(r#"let s = "line1\nline2";"#);
    let escape_warnings: Vec<_> = warnings.iter().filter(|w| w.contains("escape")).collect();
    assert!(
        escape_warnings.is_empty(),
        "Unexpected warnings: {escape_warnings:?}"
    );
}

#[test]
fn test_valid_tab_escape() {
    let warnings = analyze_for_warnings(r#"let s = "col1\tcol2";"#);
    let escape_warnings: Vec<_> = warnings.iter().filter(|w| w.contains("escape")).collect();
    assert!(
        escape_warnings.is_empty(),
        "Unexpected warnings: {escape_warnings:?}"
    );
}

#[test]
fn test_valid_backslash_escape() {
    // "a\\" in source → lexer unescapes to "a\" → validator sees trailing \ with no next char → no warning
    let warnings = analyze_for_warnings(r#"let s = "a\\";"#);
    let escape_warnings: Vec<_> = warnings.iter().filter(|w| w.contains("escape")).collect();
    assert!(
        escape_warnings.is_empty(),
        "Unexpected warnings: {escape_warnings:?}"
    );
}

#[test]
fn test_valid_null_escape() {
    let warnings = analyze_for_warnings(r#"let s = "null\0char";"#);
    let escape_warnings: Vec<_> = warnings.iter().filter(|w| w.contains("escape")).collect();
    assert!(
        escape_warnings.is_empty(),
        "Unexpected warnings: {escape_warnings:?}"
    );
}

#[test]
fn test_valid_carriage_return_escape() {
    let warnings = analyze_for_warnings(r#"let s = "cr\rtest";"#);
    let escape_warnings: Vec<_> = warnings.iter().filter(|w| w.contains("escape")).collect();
    assert!(
        escape_warnings.is_empty(),
        "Unexpected warnings: {escape_warnings:?}"
    );
}

#[test]
fn test_valid_unicode_escape() {
    let warnings = analyze_for_warnings(r#"let s = "smiley \u{1F600}";"#);
    let escape_warnings: Vec<_> = warnings.iter().filter(|w| w.contains("escape")).collect();
    assert!(
        escape_warnings.is_empty(),
        "Unexpected warnings: {escape_warnings:?}"
    );
}

#[test]
fn test_valid_unicode_escape_letter() {
    let warnings = analyze_for_warnings(r#"let s = "\u{41}";"#);
    let escape_warnings: Vec<_> = warnings.iter().filter(|w| w.contains("escape")).collect();
    assert!(
        escape_warnings.is_empty(),
        "Unexpected warnings: {escape_warnings:?}"
    );
}

// ── Unknown escape sequences (should produce warnings) ───────────────────────

#[test]
fn test_unknown_escape_sequence_produces_warning() {
    let warnings = analyze_for_warnings(r#"let s = "hello \w world";"#);
    let escape_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.contains("escape") || w.contains("Unknown"))
        .collect();
    assert!(
        !escape_warnings.is_empty(),
        "Expected warning for unknown \\w escape"
    );
}

#[test]
fn test_unknown_escape_sequence_d() {
    let warnings = analyze_for_warnings(r#"let s = "digit \d+";"#);
    let escape_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.contains("escape") || w.contains("Unknown"))
        .collect();
    assert!(
        !escape_warnings.is_empty(),
        "Expected warning for unknown \\d escape"
    );
}

// ── Malformed Unicode escapes ─────────────────────────────────────────────────

#[test]
fn test_unicode_escape_without_braces_produces_warning() {
    let warnings = analyze_for_warnings(r#"let s = "\u41";"#);
    let escape_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.contains("escape") || w.contains("Unknown") || w.contains("Unicode"))
        .collect();
    assert!(
        !escape_warnings.is_empty(),
        "Expected warning for \\u without braces"
    );
}

#[test]
fn test_unicode_escape_empty_braces_produces_warning() {
    let warnings = analyze_for_warnings(r#"let s = "\u{}";"#);
    let escape_warnings: Vec<_> = warnings
        .iter()
        .filter(|w| w.contains("Empty") || w.contains("Unicode") || w.contains("escape"))
        .collect();
    assert!(
        !escape_warnings.is_empty(),
        "Expected warning for \\u{{}} empty Unicode"
    );
}

// ── No false positives on plain strings ──────────────────────────────────────

#[test]
fn test_plain_string_no_warnings() {
    let warnings = analyze_for_warnings(r#"let s = "hello world";"#);
    let escape_warnings: Vec<_> = warnings.iter().filter(|w| w.contains("escape")).collect();
    assert!(
        escape_warnings.is_empty(),
        "Unexpected warnings: {escape_warnings:?}"
    );
}

#[test]
fn test_empty_string_no_warnings() {
    let warnings = analyze_for_warnings(r#"let s = "";"#);
    let escape_warnings: Vec<_> = warnings.iter().filter(|w| w.contains("escape")).collect();
    assert!(
        escape_warnings.is_empty(),
        "Unexpected escape warnings: {escape_warnings:?}"
    );
}
