mod common;

#[test]
fn test_variable_declaration_type_annotation() {
    assert_parser_snapshot!("let x: i64 = 1;");
    assert_parser_snapshot!("let x: Option<i64> = 1;");
    assert_parser_snapshot!("let x: std::hash::Map<str, i64> = std::hash::Map();");
}

#[test]
fn test_return_type_annotation() {
    assert_parser_snapshot!("fn foo() -> i64 { 1 }");
    assert_parser_snapshot!("fn foo() -> Option<i64> { 1 }");
    assert_parser_snapshot!("let expr = fn foo() -> i64 { 1 };");
}

#[test]
fn test_function_param_types() {
    assert_parser_snapshot!("fn foo(x: i64, y: i64) -> i64 { x }");
}
