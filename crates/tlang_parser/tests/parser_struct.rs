mod common;

#[test]
fn test_parse_struct_definition() {
    assert_parser_snapshot!("struct Foo { x: Int, y: Int }");
}
