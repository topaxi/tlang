use indoc::indoc;
use pretty_assertions::assert_eq;

mod common;

#[test]
fn test_cast_passthrough() {
    // `as` casts are erased in JS — they just pass the inner value through.
    let output = compile!(indoc! {"
        fn convert(x) { x as i64 }
    "});
    let expected_output = indoc! {"
        function convert(x) {
            return x;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_try_cast_wraps_in_result_ok() {
    // `as?` wraps the inner expression in Result.Ok(...)
    let output = compile!(indoc! {"
        fn try_convert(x) { x as? i64 }
    "});
    let expected_output = indoc! {"
        function try_convert(x) {
            return Result.Ok(x);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_cast_in_let_binding() {
    let output = compile!(indoc! {"
        fn f(x) {
            let y = x as i64;
            y
        }
    "});
    let expected_output = indoc! {"
        function f(x) {
            let y = x;
            return y;
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_try_cast_in_let_binding() {
    let output = compile!(indoc! {"
        fn f(x) {
            let y = x as? i64;
            y
        }
    "});
    let expected_output = indoc! {"
        function f(x) {
            let y = Result.Ok(x);
            return y;
        }
    "};
    assert_eq!(output, expected_output);
}
