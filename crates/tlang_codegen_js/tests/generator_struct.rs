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
        function Point() {
            if (new.target == null) return Object.assign(new Point, arguments[0]);

            this.x = undefined;
            this.y = undefined;
        }
    "};
    assert_eq!(output, expected_output);
}
