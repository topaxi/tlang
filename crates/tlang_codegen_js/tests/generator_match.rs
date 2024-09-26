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
        let $tmp$0 = x,$tmp$1;if ($tmp$0 === 42) {
            $tmp$1 = 1;
        } else {
            $tmp$1 = 0;
        }
        let y = $tmp$1;
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
        let $tmp$0 = x,$tmp$1;if ($tmp$0 === 42) {
            $tmp$1 = 1;
        } else {
            $tmp$1 = 0;
        }
        let y = $tmp$1;
    "};

    assert_eq!(output, expected_output);
}
