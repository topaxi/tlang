use indoc::indoc;
use pretty_assertions::assert_eq;

mod common;

#[ignore = "TODO"]
#[test]
fn test_codegen_let_pattern_declaration() {
    let output = compile!(indoc! {"
        let [x, y] = [42, 43];
    "});
    let expected_output = indoc! {"
        let x, y, $tmp$0 = [42, 43];
        if ($tmp$0.length >= 2) { // test all patterns
            // then get and assign values to binding patterns
            x = $tmp$0[0];
            y = $tmp$0[1];
        } else throw new TypeError('Pattern match failed');
    "};
    assert_eq!(output, expected_output);
}

#[ignore = "TODO"]
#[test]
fn test_codegen_let_pattern_declaration_dupe() {
    let output = compile!(indoc! {"
        let [x, x] = [42, 43];
    "});
    let expected_output = indoc! {"
        let x, $tmp$0 = [42, 43];
        if ($tmp$0.length >= 2 && tmp$0[0] === tmp$0[1]) {
            x = $tmp$0[0];
        } else throw new TypeError('Pattern match failed');
    "};
    assert_eq!(output, expected_output);
}

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

#[test]
fn test_codegen_pattern_match_list_bindings() {
    let output = compile!(indoc! {"
        let x = 42;
        let y = match ([42, 43]) {
            [x, y] => x + y,
            _ => 0,
        };
    "});
    let expected_output = indoc! {"
        let x = 42;
        let $tmp$0 = [42, 43],x$0,y$0,$tmp$1;if ($tmp$0.length >= 2 && (x$0 = $tmp$0[0], true) && (y$0 = $tmp$0[1], true)) {
            $tmp$1 = x$0 + y$0;
        } else {
            $tmp$1 = 0;
        }
        let y = $tmp$1;
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_pattern_match_list_empty() {
    let output = compile!(indoc! {"
        let x = match ([]) {
            [] => 1,
            _ => 0,
        };
    "});
    let expected_output = indoc! {"
        let $tmp$0 = [],$tmp$1;if ($tmp$0.length === 0) {
            $tmp$1 = 1;
        } else {
            $tmp$1 = 0;
        }
        let x = $tmp$1;
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_pattern_match_list_rest_elements() {
    let output = compile!(indoc! {"
        let x = match ([1, 2, 3, 4, 5]) {
            [n, ...rest] => n + rest.length,
            _ => 0,
        };
    "});
    let expected_output = indoc! {"
        let $tmp$0 = [1, 2, 3, 4, 5],n,rest,$tmp$1;if ($tmp$0.length >= 1 && (n = $tmp$0[0], true) && (rest = $tmp$0.slice(1), true)) {
            $tmp$1 = n + rest.length;
        } else {
            $tmp$1 = 0;
        }
        let x = $tmp$1;
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_pattern_match_enum_bindings() {
    let output = compile!(indoc! {"
        let y = match (Some(42)) {
            Some(x) => x,
            _ => 0,
        };
    "});
    let expected_output = indoc! {"
        let $tmp$0 = Option.Some(42),x,$tmp$1;if ($tmp$0.tag === \"Some\" && (x = $tmp$0[0], true)) {
            $tmp$1 = x;
        } else {
            $tmp$1 = 0;
        }
        let y = $tmp$1;
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_pattern_match_positional_enum() {
    let output = compile!(indoc! {"
        enum Expr {
            Value(int),
            Add(lhs, rhs),
        }

        let expr = Expr::Value(42);

        let z = match (expr) {
            Expr::Value(x) => x,
            Expr::Add(x, y) => x + y,
            _ => 0,
        };
    "});
    let expected_output = indoc! {"
        const Expr = {
            Value(int) {
                return {
                    tag: \"Value\",
                    [0]: int,
                };
            },
            Add(lhs, rhs) {
                return {
                    tag: \"Add\",
                    [0]: lhs,
                    [1]: rhs,
                };
            },
        };
        let expr = Expr.Value(42);
        let $tmp$0 = expr,x,y,$tmp$1;if ($tmp$0.tag === \"Value\" && (x = $tmp$0[0], true)) {
            $tmp$1 = x;
        } else if ($tmp$0.tag === \"Add\" && (x = $tmp$0[0], true) && (y = $tmp$0[1], true)) {
            $tmp$1 = x + y;
        } else {
            $tmp$1 = 0;
        }
        let z = $tmp$1;
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_pattern_match_nested_enum() {
    // TODO: EnumVariants should also be able to be matched without their fully qualified path/name
    let output = compile!(indoc! {"
        let x = 42;
        let y = match Some(Some(42)) {
            Some(Some(x)) => x,
            Some(Option::None) => 0,
            Option::None => 0,
        };
    "});
    let expected_output = indoc! {"
        let x = 42;
        let $tmp$0 = Option.Some(Option.Some(42)),x$0,$tmp$1;if ($tmp$0.tag === \"Some\" && $tmp$0[0].tag === \"Some\" && (x$0 = $tmp$0[0][0], true)) {
            $tmp$1 = x$0;
        } else if ($tmp$0.tag === \"Some\" && $tmp$0[0].tag === \"None\") {
            $tmp$1 = 0;
        } else if ($tmp$0.tag === \"None\") {
            $tmp$1 = 0;
        }
        let y = $tmp$1;
    "};
    assert_eq!(output, expected_output);
}
