use pretty_assertions::assert_eq;

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
    let expected_output = "let x = 42;\nlet x$0 = 43;\nx$0;\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_list_pattern() {
    let output = compile!("let [x, y] = [1, 2];");
    let expected_output = "let [x, y] = [1, 2];\n";

    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_list_pattern_with_wildcard() {
    let output = compile!("let [_, y] = [1, 2];");
    let expected_output = "let [, y] = [1, 2];\n";

    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_list_pattern_repetition() {
    // TODO: Should this be allowed?
    let output = compile!("let [x, x] = [1, 2]; x;");
    let expected_output = "let [x, x$0] = [1, 2];\nx$0;\n";

    assert_eq!(output, expected_output);
}
