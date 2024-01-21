use indoc::indoc;

mod common;

#[test]
fn test_simple_if_let_statement() {
    assert_parser_snapshot!(indoc! {"
        if let x = 1 {
            x
        }
    "});
}

#[test]
fn test_if_let_statement_with_enum_matching() {
    assert_parser_snapshot!(indoc! {"
        if let Option::Some(y) = x; {
            y
        }
    "});
}
