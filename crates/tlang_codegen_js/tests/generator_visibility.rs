use indoc::indoc;

mod common;

#[test]
fn test_pub_function_generates_export() {
    let output = compile!("pub fn foo() { 1 }");
    let expected_output = indoc! {"
        export function foo() {
            return 1;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_private_function_no_export() {
    let output = compile!("fn foo() { 1 }");
    let expected_output = indoc! {"
        function foo() {
            return 1;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_pub_enum_generates_export() {
    let output = compile!(indoc! {"
        pub enum Color {
            Red,
            Green,
            Blue,
        }
    "});
    assert!(
        output.starts_with("export class Color"),
        "Expected export class, got: {output}"
    );
}

#[test]
fn test_private_enum_no_export() {
    let output = compile!(indoc! {"
        enum Color {
            Red,
            Green,
            Blue,
        }
    "});
    assert!(
        output.starts_with("class Color"),
        "Expected class without export, got: {output}"
    );
}

#[test]
fn test_pub_struct_generates_export() {
    let output = compile!("pub struct Point { x: Int, y: Int }");
    let expected_output = indoc! {"
        export function Point() {
            if (new.target == null) return Object.assign(new Point(), arguments[0]);
            this.x = undefined;
            this.y = undefined;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_private_struct_no_export() {
    let output = compile!("struct Point { x: Int, y: Int }");
    let expected_output = indoc! {"
        function Point() {
            if (new.target == null) return Object.assign(new Point(), arguments[0]);
            this.x = undefined;
            this.y = undefined;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_pub_multi_clause_function_generates_export() {
    let output = compile!(indoc! {"
        pub fn factorial(0) { 1 }
        pub fn factorial(n) { n * factorial(n - 1) }
    "});
    assert!(
        output.starts_with("export function factorial"),
        "Expected export function, got: {output}"
    );
}
