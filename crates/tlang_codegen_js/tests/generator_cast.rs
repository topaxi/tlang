use indoc::indoc;
use pretty_assertions::assert_eq;

mod common;

#[test]
fn test_cast_dispatches_through_into_protocol() {
    // `as` dispatches through the Into protocol: Into::into(value)
    let output = compile!(indoc! {"
        fn convert(x) { x as i64 }
    "});
    let expected_output = indoc! {"
        function convert(x) {
            return $Into.into(x);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_try_cast_dispatches_through_try_into_protocol() {
    // `as?` dispatches through the TryInto protocol: TryInto::try_into(value)
    let output = compile!(indoc! {"
        fn try_convert(x) { x as? i64 }
    "});
    let expected_output = indoc! {"
        function try_convert(x) {
            return $TryInto.try_into(x);
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
            let y = $Into.into(x);
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
            let y = $TryInto.try_into(x);
            return y;
        }
    "};
    assert_eq!(output, expected_output);
}
