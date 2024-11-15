use indoc::indoc;
use pretty_assertions::assert_eq;

mod common;

#[test]
fn test_simple_self_recursive_tail_call_converted_to_loop() {
    let output = compile!(indoc! {"
        fn factorial(n, acc) {
            if n == 0 {
                acc
            } else {
                rec factorial(n - 1, n * acc)
            }
        }
    "});
    let expected_output = indoc! {"
        function factorial(n, acc) {
            rec:while (true) {
                if (n === 0) {
                    return acc;
                } else {
                    let $tmp$0 = n - 1;
                    let $tmp$1 = n * acc;
                    n = $tmp$0;
                    acc = $tmp$1;
                    continue rec;
                }
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_fn_expression_explicit_tail_recursive_call_converted_to_loop() {
    let output = compile!(indoc! {"
        fn factorial(n) {
            let factorial_rec = fn rec_helper(n, acc) {
                if n == 0 {
                    acc
                } else {
                    rec rec_helper(n - 1, n * acc)
                }
            };

            factorial_rec(n, 1)
        }
    "});
    let expected_output = indoc! {"
        function factorial(n) {
            let factorial_rec = function rec_helper(n, acc) {
                rec:while (true) {
                    if (n === 0) {
                        return acc;
                    } else {
                        let $tmp$0 = n - 1;
                        let $tmp$1 = n * acc;
                        n = $tmp$0;
                        acc = $tmp$1;
                        continue rec;
                    }
                }
            };
            return factorial_rec(n, 1);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_explicit_tail_recursive_call_converted_to_loop_factorial_simple() {
    let output = compile!(indoc! {"
        fn factorial(0, acc) { acc }
        fn factorial(n, acc) { return rec factorial(n - 1, n * acc); }
    "});
    let expected_output = indoc! {"
        function factorial(n, acc) {
            rec:while (true) {
                if (n === 0) {
                    return acc;
                } else {
                    let $tmp$0 = n - 1;
                    let $tmp$1 = n * acc;
                    n = $tmp$0;
                    acc = $tmp$1;
                    continue rec;
                }
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_explicit_tail_recursive_call_converted_to_loop_factorial_convenient() {
    let output = compile!(indoc! {"
        fn factorial(n) { factorial(n, 1) }
        fn factorial(0, acc) { acc }
        fn factorial(n, acc) { rec factorial(n - 1, n * acc) }
    "});
    let expected_output = indoc! {"
        function factorial$$1(n) {
            return factorial$$2(n, 1);
        }
        function factorial$$2(n, acc) {
            rec:while (true) {
                if (n === 0) {
                    return acc;
                } else {
                    let $tmp$0 = n - 1;
                    let $tmp$1 = n * acc;
                    n = $tmp$0;
                    acc = $tmp$1;
                    continue rec;
                }
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
fn test_tail_recursive_fibonacci() {
    let output = compile!(indoc! {"
        fn fibonacci(n) { fibonacci(n, 0, 1) }
        fn fibonacci(0, a, b) { a }
        fn fibonacci(1, a, b) { b }
        fn fibonacci(n, a, b) { rec fibonacci(n - 1, b, a + b) }
    "});
    let expected_output = indoc! {"
        function fibonacci$$1(n) {
            return fibonacci$$3(n, 0, 1);
        }
        function fibonacci$$3(n, a, b) {
            rec:while (true) {
                if (n === 0) {
                    return a;
                } else if (n === 1) {
                    return b;
                } else {
                    let $tmp$0 = n - 1;
                    let $tmp$1 = b;
                    let $tmp$2 = a + b;
                    n = $tmp$0;
                    a = $tmp$1;
                    b = $tmp$2;
                    continue rec;
                }
            }
        }
        function fibonacci() {
            if (arguments.length === 1) {
                return fibonacci$$1(arguments[0]);
            } else if (arguments.length === 3) {
                return fibonacci$$3(arguments[0], arguments[1], arguments[2]);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_foldl_impl() {
    let output = compile!(indoc! {"
        fn foldl([], acc, _) { acc }
        fn foldl([x, ...xs], acc, f) { rec foldl(xs, f(acc, x), f) }
    "});
    let expected_output = indoc! {"
        function foldl(arg0, acc, f) {
            rec:while (true) {
                let x,xs;if (arg0.length === 0) {
                    return acc;
                } else if (arg0.length >= 1 && (x = arg0[0], true) && (xs = arg0.slice(1), true)) {
                    let $tmp$0 = xs;
                    let $tmp$1 = f(acc, x);
                    let $tmp$2 = f;
                    arg0 = $tmp$0;
                    acc = $tmp$1;
                    f = $tmp$2;
                    continue rec;
                }
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_partition_impl() {
    let output = compile!(indoc! {"
        // partition(a[], fn(a) -> bool) -> (a[], a[])
        fn partition([], _) { [[], []] }
        fn partition(list, predicate) { partition(list, predicate, [], []) }
        // partition(a[], fn(a) -> bool, a[], a[]) -> (a[], a[])
        fn partition([], _, satisfies, doesNotSatisfy) { [satisfies, doesNotSatisfy] }
        fn partition([x, ...xs], predicate, satisfies, doesNotSatisfy) {
            let partitionedSatisfies = if predicate(x) { [...satisfies, x] } else { satisfies };
            let partitionedDoesNotSatisfy = if predicate(x) { doesNotSatisfy } else { [...doesNotSatisfy, x] };

            rec partition(xs, predicate, partitionedSatisfies, partitionedDoesNotSatisfy)
        }
    "});
    let expected_output = indoc! {"
        // partition(a[], fn(a) -> bool) -> (a[], a[])
        function partition$$2(list, predicate) {
            if (list.length === 0) {
                // partition(a[], fn(a) -> bool) -> (a[], a[])
                return [[], []];
            } else {
                return partition$$4(list, predicate, [], []);
            }
        }
        // partition(a[], fn(a) -> bool, a[], a[]) -> (a[], a[])
        function partition$$4(list, predicate, satisfies, doesNotSatisfy) {
            rec:while (true) {
                let x,xs;if (list.length === 0) {
                    // partition(a[], fn(a) -> bool, a[], a[]) -> (a[], a[])
                    return [satisfies, doesNotSatisfy];
                } else if (list.length >= 1 && (x = list[0], true) && (xs = list.slice(1), true)) {
                    let $tmp$0;if (predicate(x)) {
                        $tmp$0 = [...satisfies, x];
                    } else {
                        $tmp$0 = satisfies;
                    }
                    let partitionedSatisfies = $tmp$0;
                    let $tmp$1;if (predicate(x)) {
                        $tmp$1 = doesNotSatisfy;
                    } else {
                        $tmp$1 = [...doesNotSatisfy, x];
                    }
                    let partitionedDoesNotSatisfy = $tmp$1;
                    let $tmp$2 = xs;
                    let $tmp$3 = predicate;
                    let $tmp$4 = partitionedSatisfies;
                    let $tmp$5 = partitionedDoesNotSatisfy;
                    list = $tmp$2;
                    predicate = $tmp$3;
                    satisfies = $tmp$4;
                    doesNotSatisfy = $tmp$5;
                    continue rec;
                }
            }
        }
        function partition() {
            if (arguments.length === 2) {
                return partition$$2(arguments[0], arguments[1]);
            } else if (arguments.length === 4) {
                return partition$$4(arguments[0], arguments[1], arguments[2], arguments[3]);
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_all_impl() {
    let output = compile!(indoc! {"
        // all(a[], fn(a) -> bool) -> bool
        fn all(list, predicate) { all(list, predicate, true) }
        // all(a[], fn(a) -> bool, bool) -> bool
        fn all([], _, acc) { acc }
        fn all([x], predicate, _) if not predicate(x) { false }
        fn all([_, ...xs], predicate, acc) { rec all(xs, predicate, acc) }
    "});
    let expected_output = indoc! {"
        // all(a[], fn(a) -> bool) -> bool
        function all$$2(list, predicate) {
            return all$$3(list, predicate, true);
        }
        // all(a[], fn(a) -> bool, bool) -> bool
        function all$$3(list, predicate, acc) {
            rec:while (true) {
                let x,xs;if (list.length === 0) {
                    // all(a[], fn(a) -> bool, bool) -> bool
                    return acc;
                } else if (list.length >= 1 && (x = list[0], true) && !predicate(x)) {
                    return false;
                } else if (list.length >= 1 && (xs = list.slice(1), true)) {
                    let $tmp$0 = xs;
                    let $tmp$1 = predicate;
                    let $tmp$2 = acc;
                    list = $tmp$0;
                    predicate = $tmp$1;
                    acc = $tmp$2;
                    continue rec;
                }
            }
        }
        function all() {
            if (arguments.length === 2) {
                return all$$2(arguments[0], arguments[1]);
            } else if (arguments.length === 3) {
                return all$$3(arguments[0], arguments[1], arguments[2]);
            }
        }
    "};

    assert_eq!(output, expected_output);
}

#[test]
#[ignore]
fn test_reduce_impl() {
    // TODO: Implement some kind of exception or panic handling,
    //       or find a different test case for a tail recursion with different
    //       arity.
    // TODO: This currently will fail as `rec ...` expects all implementations
    //       have the same arity.
    let output = compile!(indoc! {"
        // reduce(a[], fn(b, a) -> b) -> b
        fn reduce([], _) { panic(\"Called reduce on empty list without accumulator\") }
        fn reduce([x, y, ...xs], f) { rec reduce(xs, f, f(x, y)) }
        fn reduce([x], _) { x }
        // reduce(a[], fn(b, a) -> b, b) -> b
        fn reduce([], _, acc) { acc }
        fn reduce([x, ...xs], f, acc) { rec reduce(xs, f, f(acc, x)) }
    "});
    let expected_output = indoc! {"
    "};

    assert_eq!(output, expected_output);
}
