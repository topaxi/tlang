use indoc::indoc;
use pretty_assertions::assert_eq;

mod common;

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
        function TestConstructor(field) {
            this.field = field;
        }
        function Test(props) {
            return new TestConstructor(props.field);
        }
        Test.new = function Test__new() {
            return Test({
                field: 0,
            });
        };
        TestConstructor.prototype.get_field = function get_field() {
            return this.field;
        };
    "};
    assert_eq!(output, expected_output);
}
