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
        let $hir$0 = undefined;
        let $tmp$0 = [42, 43],x$0,y;if ($tmp$0.length >= 2 && (x$0 = $tmp$0[0], true) && (y = $tmp$0[1], true)) {
            $hir$0 = x$0 + y;
        } else {
            $hir$0 = 0;
        }
        let y$0 = $hir$0;
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
        let $hir$0 = undefined;
        let $tmp$0 = [];if ($tmp$0.length === 0) {
            $hir$0 = 1;
        } else {
            $hir$0 = 0;
        }
        let x = $hir$0;
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
        let $hir$0 = undefined;
        let $tmp$0 = [1, 2, 3, 4, 5],n,rest;if ($tmp$0.length >= 1 && (n = $tmp$0[0], true) && (rest = $tmp$0.slice(1), true)) {
            $hir$0 = n + rest.length;
        } else {
            $hir$0 = 0;
        }
        let x = $hir$0;
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
        let $hir$0 = undefined;
        let $tmp$0 = Option.Some(42),x;if ($tmp$0.tag === Option.Some && (x = $tmp$0[0], true)) {
            $hir$0 = x;
        } else {
            $hir$0 = 0;
        }
        let y = $hir$0;
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
        let $hir$0 = undefined;
        let x,y;if (expr.tag === Expr.Value && (x = expr[0], true)) {
            $hir$0 = x;
        } else if (expr.tag === Expr.Add && (x = expr[0], true) && (y = expr[1], true)) {
            $hir$0 = x + y;
        } else {
            $hir$0 = 0;
        }
        let z = $hir$0;
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
        let $hir$0 = undefined;
        let $tmp$0 = Option.Some(Option.Some(42)),x$0;if ($tmp$0.tag === Option.Some && $tmp$0[0].tag === Option.Some && (x$0 = $tmp$0[0][0], true)) {
            $hir$0 = x$0;
        } else if ($tmp$0.tag === Option.Some && $tmp$0[0].tag === Option.None) {
            $hir$0 = 0;
        } else if ($tmp$0.tag === Option.None) {
            $hir$0 = 0;
        }
        let y = $hir$0;
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
        let $hir$0 = undefined;
        let $tmp$0 = 42,n;if ((n = $tmp$0, true) && n > 0) {
            $hir$0 = 1;
        } else {
            $hir$0 = 0;
        }
        let x = $hir$0;
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
        let $hir$0 = undefined;
        let $tmp$0 = Option.Some(42),$tmp$1,n,y;if ((n = $tmp$0, true) && ($tmp$1 = n * 2, true) && $tmp$1.tag === Option.Some && (y = $tmp$1[0], true)) {
            $hir$0 = y;
        } else {
            $hir$0 = 0;
        }
        let x = $hir$0;
    "};
    assert_eq!(output, expected_output);
}
