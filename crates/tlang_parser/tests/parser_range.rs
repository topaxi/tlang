use indoc::indoc;

mod common;

#[test]
#[ignore = "Not implemented yet"]
fn test_range_notation() {
    assert_parser_snapshot!(indoc! {"
        let range = 1..10;
    "});
}

#[test]
#[ignore = "Not implemented yet"]
fn test_inclusive_range_notation() {
    assert_parser_snapshot!(indoc! {"
        let range = 1..=10;
    "});
}

#[test]
#[ignore = "Not implemented yet"]
fn test_range_as_index() {
    assert_parser_snapshot!(indoc! {"
        let x = [1, 2, 3];
        let y = x[1..10];
    "});
}

#[test]
#[ignore = "Not implemented yet"]
fn test_inclusive_range_as_index() {
    assert_parser_snapshot!(indoc! {"
        let x = [1, 2, 3];
        let y = x[1..=10];
    "});
}
