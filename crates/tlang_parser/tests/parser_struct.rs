mod common;

use tlang_parser::Parser;

#[test]
fn test_parse_struct_definition() {
    assert_parser_snapshot!("struct Foo { x: Int, y: Int }");
}

#[test]
fn test_incomplete_struct_does_not_crash() {
    let mut parser = Parser::from_source("struct ");
    let result = parser.parse();
    assert!(
        result.is_err(),
        "Incomplete struct should produce a parse error, not crash"
    );
}

#[test]
fn test_incomplete_struct_with_open_brace_does_not_crash() {
    let mut parser = Parser::from_source("struct Foo {");
    let result = parser.parse();
    assert!(
        result.is_err(),
        "Struct with unclosed brace should produce a parse error, not crash"
    );
}
