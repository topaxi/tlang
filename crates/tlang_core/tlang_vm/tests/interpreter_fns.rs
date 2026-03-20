#![cfg(feature = "binary")]

use tlang_memory::prelude::*;

mod common;

// ── Simple functions ─────────────────────────────────────────────────────────

#[test]
fn test_simple_function() {
    let value = common::eval("fn add(a, b) { a + b } add(3, 4)");
    assert_eq!(value, TlangValue::U64(7));
}

#[test]
fn test_function_with_no_args() {
    let value = common::eval("fn answer() { 42 } answer()");
    assert_eq!(value, TlangValue::U64(42));
}

#[test]
fn test_higher_order_function() {
    let value = common::eval(
        "fn call_fn(f, x) { f(x) }
         fn double(x) { x * 2 }
         call_fn(double, 7)",
    );
    assert_eq!(value, TlangValue::U64(14));
}

#[test]
fn test_anonymous_function() {
    let value = common::eval("let f = fn(x) { x + 1 }; f(10)");
    assert_eq!(value, TlangValue::U64(11));
}

#[test]
fn test_closure_captures_env() {
    let value = common::eval(
        "fn make_adder(n) { fn(x) { x + n } }
         let add5 = make_adder(5);
         add5(3)",
    );
    assert_eq!(value, TlangValue::U64(8));
}

// ── Recursion ────────────────────────────────────────────────────────────────

#[test]
fn test_recursive_factorial() {
    let value = common::eval(
        "fn factorial(0) { 1 }
         fn factorial(n) { n * factorial(n - 1) }
         factorial(5)",
    );
    assert_eq!(value, TlangValue::U64(120));
}

#[test]
fn test_recursive_fibonacci() {
    let value = common::eval(
        "fn fibonacci(0) { 0 }
         fn fibonacci(1) { 1 }
         fn fibonacci(n) { fibonacci(n - 1) + fibonacci(n - 2) }
         fibonacci(10)",
    );
    assert_eq!(value, TlangValue::U64(55));
}

// ── Tail-call-optimised recursion ────────────────────────────────────────────

#[test]
fn test_tail_recursive_sum() {
    let value = common::eval(
        "fn sum(0, acc) { acc }
         fn sum(n, acc) { rec sum(n - 1, acc + n) }
         sum(100, 0)",
    );
    assert_eq!(value, TlangValue::U64(5050));
}

#[test]
fn test_tail_recursive_factorial() {
    let value = common::eval(
        "fn factorial_iter(0, acc) { acc }
         fn factorial_iter(n, acc) { rec factorial_iter(n - 1, n * acc) }
         factorial_iter(10, 1)",
    );
    assert_eq!(value, TlangValue::U64(3628800));
}

// ── Pattern matching in function args ────────────────────────────────────────

#[test]
fn test_list_head() {
    let value = common::eval(
        "fn head(xs) { xs[0] }
         head([10, 20, 30])",
    );
    assert_eq!(value, TlangValue::U64(10));
}

#[test]
fn test_empty_list_pattern_in_fn() {
    let value = common::eval(
        "fn length([]) { 0 }
         fn length([_, ...rest]) { 1 + length(rest) }
         length([1, 2, 3, 4, 5])",
    );
    assert_eq!(value, TlangValue::U64(5));
}

// ── Map / filter / fold (functional patterns) ────────────────────────────────

#[test]
fn test_map() {
    let s = common::eval_to_string(
        "fn map([], _) { [] }
         fn map([x, ...xs], f) { [f(x), ...map(xs, f)] }
         map([1, 2, 3, 4], fn(x) { x * x })",
    );
    assert_eq!(s, "[1, 4, 9, 16]");
}

#[test]
fn test_filter() {
    let s = common::eval_to_string(
        "fn filter([], _) { [] }
         fn filter([x, ...xs], pred) {
             if pred(x) { [x, ...filter(xs, pred)] }
             else { filter(xs, pred) }
         }
         filter([1, 2, 3, 4, 5, 6], fn(x) { x % 2 == 0 })",
    );
    assert_eq!(s, "[2, 4, 6]");
}

#[test]
fn test_foldl() {
    let value = common::eval(
        "fn foldl([], acc, _) { acc }
         fn foldl([x, ...xs], acc, f) { foldl(xs, f(acc, x), f) }
         foldl([1, 2, 3, 4, 5], 0, fn(a, b) { a + b })",
    );
    assert_eq!(value, TlangValue::U64(15));
}

#[test]
fn test_pipeline_with_stdlib_map_filter_fold() {
    let value = common::eval(
        r#"
        fn map([], _) { [] }
        fn map([x, ...xs], f) { [f(x), ...map(xs, f)] }
        fn filter([], _) { [] }
        fn filter([x, ...xs], pred) {
            if pred(x) { [x, ...filter(xs, pred)] } else { filter(xs, pred) }
        }
        fn foldl([], acc, _) { acc }
        fn foldl([x, ...xs], acc, f) { foldl(xs, f(acc, x), f) }
        [0, 1, 2, 3, 4, 5, 6, 7, 8, 9]
        |> map(fn(x) { x ** 2 })
        |> filter(fn(x) { x % 2 == 0 })
        |> foldl(0, fn(acc, x) { acc + x })
        "#,
    );
    // 0^2=0, 2^2=4, 4^2=16, 6^2=36, 8^2=64  → sum = 120
    assert_eq!(value, TlangValue::U64(120));
}

// ── Partial application ───────────────────────────────────────────────────────

#[test]
fn test_partial_application() {
    let value = common::eval(
        "fn add(a, b) { a + b }
         let add10 = add(10, _);
         add10(5)",
    );
    assert_eq!(value, TlangValue::U64(15));
}
