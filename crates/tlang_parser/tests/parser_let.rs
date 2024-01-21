mod common;

#[test]
fn test_simple_variable_declaration() {
    assert_parses!("let x = 1 + 2;");
}

#[test]
#[should_panic]
fn test_panic_on_keyword_as_identifier() {
    parse!("let fn = 1;");
}

#[test]
fn test_list_pattern() {
    assert_parses!("let [x, y] = [1, 2];");
    assert_parser_snapshot!("let [x, y, ...z] = [1, 2, 3, 4];");
}
