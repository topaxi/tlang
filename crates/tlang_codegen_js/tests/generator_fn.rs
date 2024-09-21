use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_ast::symbols::SymbolType;

mod common;

#[test]
fn test_recursive_function_definition() {
    let output = compile!(indoc! {"
        // factorial(int) -> int
        fn factorial(0) { 1 }
        // factorial(int) -> int
        fn factorial(n) { n * factorial(n - 1) }
    "});
    let expected_output = indoc! {"
        // factorial(int) -> int
        function factorial(n) {
            if (n === 0) {
                // factorial(int) -> int
                return 1;
            } else {
                // factorial(int) -> int
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
        function fibonacci(n) {
            if (n === 0) {
                return 0;
            } else if (n === 1) {
                return 1;
            } else {
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
        function gcd(m, n) {
            if (m === 0) {
                return n;
            } else if (n === 0) {
                return m;
            } else {
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
            function factorial_rec(n, acc) {
                if (n === 0) {
                    return acc;
                } else {
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
        function sum(arg0) {
            if (arg0.length === 0) {
                return 0;
            } else if (arg0.length >= 1) {
                let x = arg0[0];
                let xs = arg0.slice(1);
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
        function map(arg0, f) {
            if (arg0.length === 0) {
                return [];
            } else if (arg0.length >= 1) {
                let x = arg0[0];
                let xs = arg0.slice(1);
                return [f(x), ...map(xs, f)];
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_function_declarations_args_redefinition_should_not_collide() {
    let output = compile!(indoc! {"
        fn bar(0) { 0 }
        fn bar(0, args) { 0 }
    "});
    let expected_output = indoc! {"
        function bar(...args) {
            if (args.length === 1 && args[0] === 0) {
                return 0;
            } else if (args.length === 2 && args[0] === 0) {
                let args$a = args[1];
                return 0;
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
        fn filter([_, ...xs], f) { filter(xs, f) }
    "});
    let expected_output = indoc! {"
        function filter(arg0, f) {
            if (arg0.length === 0) {
                return [];
            } else if (arg0.length >= 1 && f(arg0[0])) {
                let x = arg0[0];
                let xs = arg0.slice(1);
                return [x, ...filter(xs, f)];
            } else if (arg0.length >= 1) {
                let xs = arg0.slice(1);
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
        function filter_map(arg0, f) {
            let $tmp$a;
            if (arg0.length === 0) {
                return [];
            } else if (arg0.length >= 1 && ($tmp$a = f(arg0[0]))) {
                let x = arg0[0];
                let xs = arg0.slice(1);
                return [$tmp$a, ...filter_map(xs, f)];
            } else if (arg0.length >= 1) {
                let x = arg0[0];
                let xs = arg0.slice(1);
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
        function filter_map(arg0, f) {
            let $tmp$a;
            let $tmp$b;
            if (arg0.length === 0) {
                return [];
            } else if (arg0.length >= 1 && ($tmp$a = f(arg0[0])) && $tmp$a.tag === \"Some\" && (($tmp$b = $tmp$a[0]), true)) {
                let x = arg0[0];
                let xs = arg0.slice(1);
                return [$tmp$b, ...filter_map(xs, f)];
            } else if (arg0.length >= 1) {
                let x = arg0[0];
                let xs = arg0.slice(1);
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
        function filter_map(arg0, f) {
            let $tmp$a;
            let $tmp$b;
            if (arg0.length === 0) {
                return [];
            } else if (arg0.length >= 1 && ($tmp$a = f(arg0[0])) && $tmp$a.tag === \"Some\" && (($tmp$b = $tmp$a.value), true)) {
                let x = arg0[0];
                let xs = arg0.slice(1);
                return [$tmp$b, ...filter_map(xs, f)];
            } else if (arg0.length >= 1) {
                let x = arg0[0];
                let xs = arg0.slice(1);
                return filter_map(xs, f);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_function_declarations_with_comments_inbetween() {
    let output = compile!(
        indoc! {"
        // Comment 1
        fn filter_map([], f) { [] }
        // Comment 2
        fn filter_map([x, ...xs], f) if let Some { value } = f(x) { [value, ...filter_map(xs, f)] }
        // Comment 3
        fn filter_map([_, ...xs], f) { filter_map(xs, f) }
    "},
        &[("Some", SymbolType::Variable)]
    );
    let expected_output = indoc! {"
        // Comment 1
        // Comment 2
        // Comment 3
        function filter_map(arg0, f) {
            let $tmp$a;
            let $tmp$b;
            if (arg0.length === 0) {
                // Comment 1
                return [];
            } else if (arg0.length >= 1 && ($tmp$a = f(arg0[0])) && $tmp$a.tag === \"Some\" && (($tmp$b = $tmp$a.value), true)) {
                // Comment 2
                let x = arg0[0];
                let xs = arg0.slice(1);
                return [$tmp$b, ...filter_map(xs, f)];
            } else if (arg0.length >= 1) {
                // Comment 3
                let xs = arg0.slice(1);
                return filter_map(xs, f);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_function_declarations_with_guard_in_first_declaration() {
    let output = compile!(indoc! {"
        // gcd(int, int) -> int
        fn gcd(a, b) if b == 0 { a }
        fn gcd(a, b) { gcd(b, a % b) }
    "});
    let expected_output = indoc! {"
        // gcd(int, int) -> int
        function gcd(a, b) {
            if (b === 0) {
                // gcd(int, int) -> int
                return a;
            } else {
                return gcd(b, a % b);
            }
        }
    "};

    assert_eq!(output, expected_output);
}

// TODO: This is not consistent with multiple declarations bodies, where this would only match a
//       call with a list with at least one element. But the current implementation for a single
//       function body will match an empty list as well.
#[test]
fn test_function_list_match_with_wildcard() {
    let output = compile!("fn tail([_, ...xs]) { xs }");
    let expected_output = indoc! {"
        function tail([, ...xs]) {
            return xs;
        }
    "};
    assert_eq!(output, expected_output);
}
