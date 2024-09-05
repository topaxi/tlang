use indoc::indoc;
use pretty_assertions::assert_eq;

mod common;

#[test]
fn test_struct_definition() {
    let output = compile!(indoc! {"
        struct Point {
            x: i32,
            y: i32,
        }
    "});
    let expected_output = indoc! {"
        function Point(x, y) {
            this.x = x;
            this.y = y;
        }
    "};
    assert_eq!(output, expected_output);
}