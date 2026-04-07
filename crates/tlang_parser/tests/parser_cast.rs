mod common;

#[test]
fn test_cast_expression() {
    assert_parser_snapshot!("let x = value as i64;");
}

#[test]
fn test_try_cast_expression() {
    assert_parser_snapshot!("let x = value as? Foo;");
}

#[test]
fn test_cast_with_addition() {
    assert_parser_snapshot!("let x = 1 + 2 as i64;");
}

#[test]
fn test_cast_with_comparison() {
    assert_parser_snapshot!("let x = a == b as i64;");
}

#[test]
fn test_cast_in_let_binding() {
    assert_parser_snapshot!("let x = value as i64;");
}

#[test]
fn test_try_cast_in_let_binding() {
    assert_parser_snapshot!("let y = input as? MyStruct;");
}
