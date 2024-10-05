use indoc::indoc;
use pretty_assertions::assert_eq;

mod common;

#[ignore]
#[test]
fn test_function_definition_on_struct() {
    let output = compile!(indoc! {"
        struct Test {
            field: int,
        }

        fn Test::new() -> Test {
            Test {
                field: 0,
            }
        }

        fn Test.get_field(self) -> int {
            self.field
        }
    "});
    let expected_output = indoc! {"
        function Test(field) {
            this.field = field;
        }
        Test.new = function Text__new() {
            return new Test(0);
        };
        Test.prototype.get_field = function Test__get_field() {
            return this.field;
        };
    "};
    assert_eq!(output, expected_output);
}
