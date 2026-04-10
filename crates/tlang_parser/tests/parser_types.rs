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

#[test]
fn test_function_type_annotation() {
    // Basic function type with named parameters
    assert_parser_snapshot!("let f: fn(x: i64) -> i64 = fn(x: i64) { x };");
    // Function type with multiple named parameters
    assert_parser_snapshot!("let f: fn(a: i64, b: i64) -> i64 = fn(a: i64, b: i64) { a + b };");
    // Function type without parameter names (bare types)
    assert_parser_snapshot!("let f: fn(i64) -> i64 = fn(x: i64) { x };");
    // Function type with no parameters
    assert_parser_snapshot!("let f: fn() -> i64 = fn() { 42 };");
    // Function type as a function parameter
    assert_parser_snapshot!("fn higher(f: fn(i64) -> i64, x: i64) -> i64 { f(x) }");
    // Nested function type
    assert_parser_snapshot!("let f: fn(fn(i64) -> i64) -> i64 = fn(g: fn(i64) -> i64) { g(1) };");
    // Function type with no return type
    assert_parser_snapshot!("let f: fn(i64) = fn(x: i64) { log(x) };");
}
