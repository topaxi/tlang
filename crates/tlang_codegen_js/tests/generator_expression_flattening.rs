use indoc::indoc;
use insta::assert_snapshot;

mod common;

#[test]
fn test_expression_flattening_function_arguments() {
    let source = indoc! {r#"
        fn some_function(a, b) {
            return a + b;
        }

        fn test_complex_function_args() {
            let result = some_function(
                if true {
                    let x = 42;
                    x * 2
                } else {
                    let y = 10;
                    y + 5
                },
                if false {
                    let a = 1;
                    a + 1
                } else {
                    let b = 2;
                    b * 3
                }
            );
            return result;
        }
    "#};

    let output = compile!(source);
    assert_snapshot!(output);
}

#[test]
fn test_expression_flattening_return_statement() {
    let source = indoc! {r#"
        fn test_return_complex() {
            return if true {
                let x = 42;
                x > 0
            } else {
                let y = 0;
                y == 0
            };
        }
    "#};

    let output = compile!(source);
    assert_snapshot!(output);
}

#[test]
fn test_expression_flattening_binary_operations() {
    let source = indoc! {r#"
        fn test_complex_binary_operations() {
            return (if true {
                let x = 10;
                x + 5
            } else {
                let y = 20;
                y - 5
            }) + (if false {
                let a = 3;
                a * 2
            } else {
                let b = 4;
                b / 2
            });
        }
    "#};

    let output = compile!(source);
    assert_snapshot!(output);
}

#[test]
fn test_expression_flattening_nested_conditions() {
    let source = indoc! {r#"
        fn test_nested_complex_expression() {
            return if (if true {
                let x = 42;
                x > 0
            } else {
                let y = 0;
                y == 0
            }) { "positive" } else { "negative" };
        }
    "#};

    let output = compile!(source);
    assert_snapshot!(output);
}

#[test]
fn test_expression_flattening_let_statements() {
    let source = indoc! {r#"
        fn test_let_complex() {
            let result = if true {
                let x = 42;
                x * 2
            } else {
                let y = 10;
                y + 5
            };
            return result;
        }
    "#};

    let output = compile!(source);
    assert_snapshot!(output);
}

#[test]
fn test_simple_expressions_preserved() {
    let source = indoc! {r#"
        fn test_simple_if() {
            return if true { 42 } else { 24 };
        }

        fn test_simple_nested() {
            return if (if true { true } else { false }) { "yes" } else { "no" };
        }
    "#};

    let output = compile!(source);
    assert_snapshot!(output);
}
