use indoc::indoc;
use pretty_assertions::assert_eq;

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
        function factorial$$1(n) {
            return factorial$$2(n, 1);
        }
        function factorial$$2(n, acc) {
            if (n === 0) {
                return acc;
            } else {
                return factorial$$2(n - 1, n * acc);
            }
        }
        function factorial() {
            if (arguments.length === 1) {
                return factorial$$1(arguments[0]);
            } else if (arguments.length === 2) {
                return factorial$$2(arguments[0], arguments[1]);
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
            let x,xs;if (arg0.length === 0) {
                return 0;
            } else if (arg0.length >= 1 && (x = arg0[0], true) && (xs = arg0.slice(1), true)) {
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
            let x,xs;if (arg0.length === 0) {
                return [];
            } else if (arg0.length >= 1 && (x = arg0[0], true) && (xs = arg0.slice(1), true)) {
                return [f(x), ...map(xs, f)];
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
            let x,xs;if (arg0.length === 0) {
                return [];
            } else if (arg0.length >= 1 && (x = arg0[0], true) && (xs = arg0.slice(1), true) && f(x)) {
                return [x, ...filter(xs, f)];
            } else if (arg0.length >= 1 && (xs = arg0.slice(1), true)) {
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
        fn filter_map([_, ...xs], f) { filter_map(xs, f) }
    "});
    let expected_output = indoc! {"
        function filter_map(arg0, f) {
            let $tmp$0,x,xs,y;if (arg0.length === 0) {
                return [];
            } else if (arg0.length >= 1 && (x = arg0[0], true) && (xs = arg0.slice(1), true) && ($tmp$0 = f(x), true) && (y = $tmp$0, true)) {
                return [y, ...filter_map(xs, f)];
            } else if (arg0.length >= 1 && (xs = arg0.slice(1), true)) {
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
        fn filter_map([_, ...xs], f) { filter_map(xs, f) }
    "});
    let expected_output = indoc! {"
        function filter_map(arg0, f) {
            let $tmp$0,x,xs,y;if (arg0.length === 0) {
                return [];
            } else if (arg0.length >= 1 && (x = arg0[0], true) && (xs = arg0.slice(1), true) && ($tmp$0 = f(x), true) && $tmp$0.tag === Option.Some && (y = $tmp$0[0], true)) {
                return [y, ...filter_map(xs, f)];
            } else if (arg0.length >= 1 && (xs = arg0.slice(1), true)) {
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
        fn filter_map([_, ...xs], f) { filter_map(xs, f) }
    "});
    let expected_output = indoc! {"
        function filter_map(arg0, f) {
            let $tmp$0,x,xs,value;if (arg0.length === 0) {
                return [];
            } else if (arg0.length >= 1 && (x = arg0[0], true) && (xs = arg0.slice(1), true) && ($tmp$0 = f(x), true) && $tmp$0.tag === Option.Some && (value = $tmp$0.value, true)) {
                return [value, ...filter_map(xs, f)];
            } else if (arg0.length >= 1 && (xs = arg0.slice(1), true)) {
                return filter_map(xs, f);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_function_declarations_with_comments_inbetween() {
    let output = compile!(indoc! {"
        // Comment 1
        fn filter_map([], _) { [] }
        // Comment 2
        fn filter_map([x, ...xs], f) if let Some { value } = f(x) { [value, ...filter_map(xs, f)] }
        // Comment 3
        fn filter_map([_, ...xs], f) { filter_map(xs, f) }
    "});
    let expected_output = indoc! {"
        // Comment 1
        // Comment 2
        // Comment 3
        function filter_map(arg0, f) {
            let $tmp$0,x,xs,value;if (arg0.length === 0) {
                // Comment 1
                return [];
            } else if (arg0.length >= 1 && (x = arg0[0], true) && (xs = arg0.slice(1), true) && ($tmp$0 = f(x), true) && $tmp$0.tag === Option.Some && (value = $tmp$0.value, true)) {
                // Comment 2
                return [value, ...filter_map(xs, f)];
            } else if (arg0.length >= 1 && (xs = arg0.slice(1), true)) {
                // Comment 3
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

// TODO: This should transform into a match statement within the body as well.
#[ignore]
#[test]
fn test_function_list_match_with_wildcard() {
    let output = compile!("fn tail([_, ...xs]) { xs }");
    let expected_output = indoc! {"
        function tail(arg0) {
            let xs;if (arg0.length >= 1 && (xs = arg0.slice(1), true)) {
                return xs;
            } else {
                throw new TypeError(\"Pattern match failed\");
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_function_reuse_param_name_with_pattern() {
    let output = compile!(indoc! {"
        // quicksort(a[]) -> a[]
        fn quicksort([]) { [] }
        fn quicksort(list) {
          let pivotIndex = random_int(len(list));
          let pivot = list[pivotIndex];
          let list = [...list.slice(0, pivotIndex), ...list.slice(pivotIndex+1)];
          let smaller = list |> filter(fn(y) { y <= pivot });
          let greater = list |> filter(fn(y) { y > pivot });
          [...quicksort(smaller), pivot, ...quicksort(greater)]
        }
    "});
    let expected_output = indoc! {"
        // quicksort(a[]) -> a[]
        function quicksort(list) {
            if (list.length === 0) {
                // quicksort(a[]) -> a[]
                return [];
            } else {
                let pivotIndex = random_int(len(list));
                let pivot = list[pivotIndex];
                let list$0 = [...list.slice(0, pivotIndex), ...list.slice(pivotIndex + 1)];
                let smaller = filter(list$0, (y) => y <= pivot);
                let greater = filter(list$0, (y) => y > pivot);
                return [...quicksort(smaller), pivot, ...quicksort(greater)];
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_function_name_with_reserved_keyword() {
    let output = compile!(indoc! {"
        fn delete() { 1 }
    "});
    let expected_output = indoc! {"
        function $delete() {
            return 1;
        }
    "};
    assert_eq!(output, expected_output);
}
