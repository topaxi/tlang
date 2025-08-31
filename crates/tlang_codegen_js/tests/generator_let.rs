use indoc::indoc;
use pretty_assertions::assert_eq;

use self::common::CodegenOptions;

mod common;

#[test]
fn test_codegen_variable_declaration() {
    let output = compile!("let x = 42;");
    let expected_output = "let x = 42;\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_variable_declaration_with_expression() {
    let output = compile!("let x = 42 + 1;", CodegenOptions::default().optimize(false));
    let expected_output = "let x = 42 + 1;\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_variable_shadowing() {
    let output = compile!(
        "let x = 42; let x = 43; x;",
        CodegenOptions::default().optimize(false)
    );
    let expected_output = "let x = 42;\nlet x$0 = 43;\nx$0;\n";
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_list_pattern() {
    let output = compile!("let [x, y] = [1, 2];");
    let expected_output = "let [x, y] = [1, 2];\n";

    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_list_pattern_with_wildcard() {
    let output = compile!("let [_, y] = [1, 2];");
    let expected_output = "let [, y] = [1, 2];\n";

    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_list_pattern_repetition() {
    // TODO: Should this be allowed?
    let output = compile!("let [x, x] = [1, 2]; x;");
    let expected_output = "let [x, x$0] = [1, 2];\nx$0;\n";

    assert_eq!(output, expected_output);
}

#[test]
#[ignore = "Not implemented yet"]
fn test_codegen_refutable_pattern() {
    let output = compile!("let Some(x) = Some(42);");
    let expected_output = indoc! {"
        let x = Some(42);if (x.tag === 'Some') {
            x = x[0];
        } else {
            throw new TypeError('Pattern match failed');
        }
    "};
    assert_eq!(output, expected_output);

    let output = compile!("let Some(x) = Some(42) else { return; };");
    let expected_output = indoc! {"
        let x = Some(42);if (x.tag === 'Some') {
            x = x[0];
        } else {
            return;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_if_let() {
    // TODO: This does not work yet in module body.
    let output = compile!("fn main() { if let Some(x) = Some(42) { x } }");
    let expected_output = indoc! {"
        function main() {
            let $tmp$0 = Option.Some(42);
            let x;if ($tmp$0.tag === Option.Some && (x = $tmp$0[0], true)) {
                return x;
            }
        }
    "};
    assert_eq!(output, expected_output);

    let output = compile!("fn main() { if let Some(x) = Some(42) { x } else { 0 } }");
    let expected_output = indoc! {"
        function main() {
            let $tmp$0 = Option.Some(42);
            let x;if ($tmp$0.tag === Option.Some && (x = $tmp$0[0], true)) {
                return x;
            } else {
                return 0;
            }
        }
    "};
    assert_eq!(output, expected_output);

    let output = compile!(indoc! {"
        fn main() {
            let value = Some(42);

            if let Some(42) = value; {
                9000
            } else if let Some(x) = value; {
                x
            } else if value == Some(100) {
                100
            } else {
                5
            }
        }
    "});
    let expected_output = indoc! {"
        function main() {
            let value = Option.Some(42);
            let $tmp$0;
            let x;if (value.tag === Option.Some && value[0] === 42) {
                return 9000;
            } else if (($tmp$0 = value, true) && $tmp$0.tag === Option.Some && (x = $tmp$0[0], true)) {
                return x;
            } else if (value === Option.Some(100)) {
                return 100;
            } else {
                return 5;
            }
        }
    "};
    assert_eq!(output, expected_output);
}
