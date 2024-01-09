use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_ast::symbols::SymbolType;

mod common;

#[test]
fn test_recursive_function_definition() {
    let output = compile!(indoc! {"
        fn factorial(0) { 1 }
        fn factorial(n) { n * factorial(n - 1) }
    "});
    let expected_output = indoc! {"
        function factorial(...args) {
            if (args[0] === 0) {
                return 1;
            } else {
                let n = args[0];
                return n * factorial(n - 1);
            }
        }
    "};
    assert_eq!(output, expected_output);

    let output = compile!(indoc! {"
        fn fibonacci(0) { 0 }
        fn fibonacci(1) { 1 }
        fn fibonacci(n) { fibonacci(n - 1) + fibonacci(n - 2) }
    "});
    let expected_output = indoc! {"
        function fibonacci(...args) {
            if (args[0] === 0) {
                return 0;
            } else if (args[0] === 1) {
                return 1;
            } else {
                let n = args[0];
                return fibonacci(n - 1) + fibonacci(n - 2);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_recursive_function_definition_multiple_with_multiple_args() {
    let output = compile!(indoc! {"
        fn gcd(0, n) { n }
        fn gcd(m, 0) { m }
        fn gcd(m, n) { gcd(n, m % n) }
    "});
    let expected_output = indoc! {"
        function gcd(...args) {
            if (args[0] === 0) {
                let n = args[1];
                return n;
            } else if (args[1] === 0) {
                let m = args[0];
                return m;
            } else {
                let m = args[0];
                let n = args[1];
                return gcd(n, m % n);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_tail_recursive_factorial_nested() {
    let output = compile!(indoc! {"
        fn factorial(n) {
            fn factorial_rec(0, acc) { acc }
            fn factorial_rec(n, acc) { factorial_rec(n - 1, n * acc) }

            factorial_rec(n, 1)
        }
    "});
    let expected_output = indoc! {"
        function factorial(n) {
            function factorial_rec(...args) {
                if (args[0] === 0) {
                    let acc = args[1];
                    return acc;
                } else {
                    let n = args[0];
                    let acc = args[1];
                    return factorial_rec(n - 1, n * acc);
                }
            }
            return factorial_rec(n, 1);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_tail_recursive_factorial_idiomatic() {
    let output = compile!(indoc! {"
        fn factorial(n) { factorial(n, 1) }
        fn factorial(0, acc) { acc }
        fn factorial(n, acc) { factorial(n - 1, n * acc) }
    "});
    let expected_output = indoc! {"
        function factorial(...args) {
            if (args.length === 1) {
                let n = args[0];
                return factorial(n, 1);
            } else if (args.length === 2 && args[0] === 0) {
                let acc = args[1];
                return acc;
            } else if (args.length === 2) {
                let n = args[0];
                let acc = args[1];
                return factorial(n - 1, n * acc);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_recursive_sum() {
    let output = compile!(indoc! {"
        fn sum([]) { 0 }
        fn sum([x, ...xs]) { x + sum(xs) }
    "});
    let expected_output = indoc! {"
        function sum(...args) {
            if (args[0].length === 0) {
                return 0;
            } else if (args[0].length >= 1) {
                let x = args[0][0];
                let xs = args[0].slice(1);
                return x + sum(xs);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_recursive_map() {
    let output = compile!(indoc! {"
        fn map([], f) { [] }
        fn map([x, ...xs], f) { [f(x), ...map(xs, f)] }
    "});
    let expected_output = indoc! {"
        function map(...args) {
            if (args[0].length === 0) {
                let f = args[1];
                return [];
            } else if (args[0].length >= 1) {
                let f = args[1];
                let x = args[0][0];
                let xs = args[0].slice(1);
                return [f(x), ...map(xs, f)];
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
#[ignore = "TODO"]
fn test_function_declarations_args_redefinition_should_not_collide() {
    let output = compile!(indoc! {"
        fn foo(0) { 0 }
        fn foo(args) { args }
    "});
    let expected_output = indoc! {"
        function foo(...args) {
            if (args[0] === 0) {
                return 0;
            } else {
                let args$a = args[0];
                return args$a;
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_function_declarations_with_guard() {
    let output = compile!(indoc! {"
        fn filter([], f) { [] }
        fn filter([x, ...xs], f) if f(x) { [x, ...filter(xs, f)] }
        fn filter([x, ...xs], f) { filter(xs, f) }
    "});
    let expected_output = indoc! {"
        function filter(...args) {
            if (args[0].length === 0) {
                let f = args[1];
                return [];
            } else if (args[0].length >= 1 && args[1](args[0][0])) {
                let f = args[1];
                let x = args[0][0];
                let xs = args[0].slice(1);
                return [x, ...filter(xs, f)];
            } else if (args[0].length >= 1) {
                let f = args[1];
                let x = args[0][0];
                let xs = args[0].slice(1);
                return filter(xs, f);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_function_declarations_with_if_let_guard() {
    let output = compile!(indoc! {"
        fn filter_map([], f) { [] }
        fn filter_map([x, ...xs], f) if let y = f(x) { [y, ...filter_map(xs, f)] }
        fn filter_map([x, ...xs], f) { filter_map(xs, f) }
    "});
    let expected_output = indoc! {"
        function filter_map(...args) {
            let $tmp$a;
            if (args[0].length === 0) {
                let f = args[1];
                return [];
            } else if (args[0].length >= 1 && ($tmp$a = args[1](args[0][0]))) {
                let f = args[1];
                let x = args[0][0];
                let xs = args[0].slice(1);
                return [$tmp$a, ...filter_map(xs, f)];
            } else if (args[0].length >= 1) {
                let f = args[1];
                let x = args[0][0];
                let xs = args[0].slice(1);
                return filter_map(xs, f);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_function_declarations_with_if_let_guard_enum() {
    let output = compile!(indoc! {"
        fn filter_map([], f) { [] }
        fn filter_map([x, ...xs], f) if let Some(y) = f(x) { [y, ...filter_map(xs, f)] }
        fn filter_map([x, ...xs], f) { filter_map(xs, f) }
    "});
    let expected_output = indoc! {"
        function filter_map(...args) {
            let $tmp$a;
            let $tmp$b;
            if (args[0].length === 0) {
                let f = args[1];
                return [];
            } else if (args[0].length >= 1 && ($tmp$a = args[1](args[0][0])) && $tmp$a.tag === \"Some\" && (($tmp$b = $tmp$a[0]), true)) {
                let f = args[1];
                let x = args[0][0];
                let xs = args[0].slice(1);
                return [$tmp$b, ...filter_map(xs, f)];
            } else if (args[0].length >= 1) {
                let f = args[1];
                let x = args[0][0];
                let xs = args[0].slice(1);
                return filter_map(xs, f);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_function_declarations_with_if_let_guard_named_fields_enum() {
    let output = compile!(indoc! {"
        fn filter_map([], f) { [] }
        fn filter_map([x, ...xs], f) if let Some { value } = f(x) { [value, ...filter_map(xs, f)] }
        fn filter_map([x, ...xs], f) { filter_map(xs, f) }
    "});
    let expected_output = indoc! {"
        function filter_map(...args) {
            let $tmp$a;
            let $tmp$b;
            if (args[0].length === 0) {
                let f = args[1];
                return [];
            } else if (args[0].length >= 1 && ($tmp$a = args[1](args[0][0])) && $tmp$a.tag === \"Some\" && (($tmp$b = $tmp$a.value), true)) {
                let f = args[1];
                let x = args[0][0];
                let xs = args[0].slice(1);
                return [$tmp$b, ...filter_map(xs, f)];
            } else if (args[0].length >= 1) {
                let f = args[1];
                let x = args[0][0];
                let xs = args[0].slice(1);
                return filter_map(xs, f);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_function_declarations_with_comments_inbetween() {
    let output = compile!(indoc! {"
        fn filter_map([], f) { [] }
        fn filter_map([x, ...xs], f) if let Some { value } = f(x) { [value, ...filter_map(xs, f)] }
        // Comment
        fn filter_map([x, ...xs], f) { filter_map(xs, f) }
    "});
    let expected_output = indoc! {"
        // Comment
        function filter_map(...args) {
            let $tmp$a;
            let $tmp$b;
            if (args[0].length === 0) {
                let f = args[1];
                return [];
            } else if (args[0].length >= 1 && ($tmp$a = args[1](args[0][0])) && $tmp$a.tag === \"Some\" && (($tmp$b = $tmp$a.value), true)) {
                let f = args[1];
                let x = args[0][0];
                let xs = args[0].slice(1);
                return [$tmp$b, ...filter_map(xs, f)];
            } else if (args[0].length >= 1) {
                let f = args[1];
                let x = args[0][0];
                let xs = args[0].slice(1);
                return filter_map(xs, f);
            }
        }
    "};
    assert_eq!(output, expected_output);
}
