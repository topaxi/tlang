use indoc::indoc;
use pretty_assertions::assert_eq;

mod common;

#[test]
fn test_codegen_pattern_match_expressions() {
    let output = compile!(indoc! {"
        let x = 42;
        let y = match (x) {
            42 => 1,
            _ => 0,
        };
    "});
    let expected_output = indoc! {"
        let x = 42;
        let $tmp$a = x,$tmp$b;if ($tmp$a === 42) {
            $tmp$b = 1;
        } else {
            $tmp$b = 0;
        }
        let y = $tmp$b;
    "};

    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_pattern_match_blocks() {
    let output = compile!(indoc! {"
        let x = 42;
        let y = match (x) {
            42 => { 1 },
            _ => { 0 },
        };
    "});
    let expected_output = indoc! {"
        let x = 42;
        let $tmp$a = x,$tmp$b;if ($tmp$a === 42) {
            $tmp$b = 1;
        } else {
            $tmp$b = 0;
        }
        let y = $tmp$b;
    "};

    assert_eq!(output, expected_output);
}
