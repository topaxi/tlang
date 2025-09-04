use indoc::indoc;
use pretty_assertions::assert_eq;

use self::common::CodegenOptions;

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
    let output = compile!(
        indoc! {"
            let x = 42;
            let y = match (x) {
                42 => 1,
                _ => 0,
            };
        "},
        CodegenOptions::default().optimize(false)
    );
    let expected_output = indoc! {"
        let x = 42;
        let $hir$0 = undefined;
        if (x === 42) {
            $hir$0 = 1;
        } else {
            $hir$0 = 0;
        }
        let y = $hir$0;
    "};

    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_pattern_match_blocks() {
    let output = compile!(
        indoc! {"
            let x = 42;
            let y = match (x) {
                42 => { 1 },
                _ => { 0 },
            };
        "},
        CodegenOptions::default().optimize(false)
    );
    let expected_output = indoc! {"
        let x = 42;
        let $tmp$0;if (x === 42) {
            $tmp$0 = 1;
        } else {
            $tmp$0 = 0;
        };
        let y = $tmp$0;
    "};

    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_pattern_match_list_bindings() {
    let output = compile!(indoc! {"
        let x = 42;
        let y = match [42, 43] {
            [x, y] => x + y,
            _ => 0,
        };
    "});
    let expected_output = indoc! {"
        let x = 42;
        let $tmp$0;
        let $tmp$1 = [42, 43];
        let x$0;
        let y;if ($tmp$1.length >= 2 && (x$0 = $tmp$1[0], true) && (y = $tmp$1[1], true)) {
            $tmp$0 = x$0 + y;
        } else {
            $tmp$0 = 0;
        };
        let y$0 = $tmp$0;
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
        let $tmp$0;
        let $tmp$1 = [];if ($tmp$1.length === 0) {
            $tmp$0 = 1;
        } else {
            $tmp$0 = 0;
        };
        let x = $tmp$0;
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
        let $tmp$0;
        let $tmp$1 = [1, 2, 3, 4, 5];
        let n;
        let rest;if ($tmp$1.length >= 1 && (n = $tmp$1[0], true) && (rest = $tmp$1.slice(1), true)) {
            $tmp$0 = n + rest.length;
        } else {
            $tmp$0 = 0;
        };
        let x = $tmp$0;
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
        let $tmp$0;
        let $tmp$1 = Option.Some(42);
        let x;if ($tmp$1.tag === Option.Some && (x = $tmp$1[0], true)) {
            $tmp$0 = x;
        } else {
            $tmp$0 = 0;
        };
        let y = $tmp$0;
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
        class Expr {
            tag = this;
            [0];
            [1];
            static Value = (int) => Object.assign(new this, { tag: this.Value, [0]: int });
            static Add = (lhs, rhs) => Object.assign(new this, { tag: this.Add, [0]: lhs, [1]: rhs });
        }
        let expr = Expr.Value(42);
        let $tmp$0;
        let x;
        let y;if (expr.tag === Expr.Value && (x = expr[0], true)) {
            $tmp$0 = x;
        } else if (expr.tag === Expr.Add && (x = expr[0], true) && (y = expr[1], true)) {
            $tmp$0 = x + y;
        } else {
            $tmp$0 = 0;
        };
        let z = $tmp$0;
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
        let $tmp$0;
        let $tmp$1 = Option.Some(Option.Some(42));
        let x$0;if ($tmp$1.tag === Option.Some && $tmp$1[0].tag === Option.Some && (x$0 = $tmp$1[0][0], true)) {
            $tmp$0 = x$0;
        } else if ($tmp$1.tag === Option.Some && $tmp$1[0].tag === Option.None) {
            $tmp$0 = 0;
        } else if ($tmp$1.tag === Option.None) {
            $tmp$0 = 0;
        };
        let y = $tmp$0;
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_pattern_match_guards() {
    let output = compile!(indoc! {"
        let x = match 42 {
            n if n > 0 => 1,
            _ => 0,
        };
    "});
    let expected_output = indoc! {"
        let $tmp$0;
        let $tmp$1 = 42;
        let n;if ((n = $tmp$1, true) && n > 0) {
            $tmp$0 = 1;
        } else {
            $tmp$0 = 0;
        };
        let x = $tmp$0;
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_codegen_pattern_match_let_guards() {
    let output = compile!(indoc! {"
        let x = match Some(42) {
            n if let Some(y) = n * 2 => y,
            _ => 0,
        };
    "});
    let expected_output = indoc! {"
        let $tmp$0;
        let $tmp$1 = Option.Some(42);
        let $tmp$2;
        let n;
        let y;if ((n = $tmp$1, true) && ($tmp$2 = n * 2, true) && $tmp$2.tag === Option.Some && (y = $tmp$2[0], true)) {
            $tmp$0 = y;
        } else {
            $tmp$0 = 0;
        };
        let x = $tmp$0;
    "};
    assert_eq!(output, expected_output);
}
