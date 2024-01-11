use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_ast::symbols::SymbolType;

mod common;

#[test]
fn test_codegen_variable_declaration() {
    let output = compile!("let x = 42;");
    let expected_output = "let x = 42;\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_variable_declaration_with_expression() {
    let output = compile!("let x = 42 + 1;");
    let expected_output = "let x = 42 + 1;\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_variable_shadowing() {
    let output = compile!("let x = 42; let x = 43; x;");
    let expected_output = "let x = 42;\nlet x$a = 43;\nx$a;\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_list_pattern() {
    let output = compile!("let [x, y] = [1, 2];");
    let expected_output = "let [x, y] = [1, 2];\n";

    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_list_pattern_repetition() {
    // TODO: Should this be allowed?
    let output = compile!("let [x, x] = [1, 2]; x;");
    let expected_output = "let [x, x$a] = [1, 2];\nx$a;\n";

    assert_eq!(output, expected_output);
}
