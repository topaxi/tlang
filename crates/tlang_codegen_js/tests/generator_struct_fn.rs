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

        fn Test.set_field(self, field: int) {
            self.field = field;
        }
    "});
    let expected_output = indoc! {"
        function Test() {
            if (new.target == null) return Object.assign(new Test, arguments[0]);

            this.field = undefined;
        }
        Test.new = function Test__new() {
            return Test({
                field: 0,
            });
        }
        Test.prototype.get_field = function get_field() {
            return this.field;
        }
        Test.prototype.set_field = function set_field(field) {
            this.field = field;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_variadic_function_definition_on_struct() {
    let output = compile!(indoc! {"
        struct Test {
            field: int,
        }

        fn Test::new() -> Test {
            Test {
                field: 0,
            }
        }

        fn Test.value(self) -> int {
            self.field
        }
        fn Test.value(self, value: int) {
            self.field = value;
        }
    "});
    let expected_output = indoc! {"
        function Test() {
            if (new.target == null) return Object.assign(new Test, arguments[0]);

            this.field = undefined;
        }
        Test.new = function Test__new() {
            return Test({
                field: 0,
            });
        }
        Test.prototype.value$$1 = function value$$1() {
            return this.field;
        }
        Test.prototype.value$$2 = function value$$2(value) {
            this.field = value;
        }
        Test.prototype.value = function value() {
            if (arguments.length === 0) {
                return this.value$$1();
            } else if (arguments.length === 1) {
                return this.value$$2(arguments[0]);
            }
        }
    "};
    assert_eq!(output, expected_output);
}
