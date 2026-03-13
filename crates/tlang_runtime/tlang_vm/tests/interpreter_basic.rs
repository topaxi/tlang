#![cfg(feature = "binary")]

use tlang_memory::prelude::*;

mod common;

// ── Literals ────────────────────────────────────────────────────────────────

#[test]
fn test_integer_literal() {
    let value = common::eval("42");
    assert_eq!(value, TlangValue::U64(42));
}

#[test]
fn test_float_literal() {
    let value = common::eval("3.14");
    assert!(matches!(value, TlangValue::F64(f) if (f - 3.14).abs() < 1e-10));
}

#[test]
fn test_bool_true() {
    let value = common::eval("true");
    assert_eq!(value, TlangValue::Bool(true));
}

#[test]
fn test_bool_false() {
    let value = common::eval("false");
    assert_eq!(value, TlangValue::Bool(false));
}

#[test]
fn test_string_literal() {
    let s = common::eval_to_string(r#""hello world""#);
    assert_eq!(s, "hello world");
}

// ── Arithmetic ──────────────────────────────────────────────────────────────

#[test]
fn test_addition() {
    let value = common::eval("1 + 2");
    assert_eq!(value, TlangValue::U64(3));
}

#[test]
fn test_subtraction() {
    let value = common::eval("10 - 4");
    assert_eq!(value, TlangValue::U64(6));
}

#[test]
fn test_multiplication() {
    let value = common::eval("3 * 7");
    assert_eq!(value, TlangValue::U64(21));
}

#[test]
fn test_division() {
    let value = common::eval("10 / 2");
    assert_eq!(value, TlangValue::U64(5));
}

#[test]
fn test_modulo() {
    let value = common::eval("17 % 5");
    assert_eq!(value, TlangValue::U64(2));
}

#[test]
fn test_power() {
    let value = common::eval("2 ** 10");
    assert_eq!(value, TlangValue::U64(1024));
}

#[test]
fn test_float_arithmetic() {
    let value = common::eval("1.5 + 2.5");
    assert!(matches!(value, TlangValue::F64(f) if (f - 4.0).abs() < 1e-10));
}

#[test]
fn test_integer_negation() {
    let value = common::eval("-42");
    assert_eq!(value, TlangValue::I64(-42));
}

// ── Comparison ──────────────────────────────────────────────────────────────

#[test]
fn test_equality_true() {
    let value = common::eval("1 == 1");
    assert_eq!(value, TlangValue::Bool(true));
}

#[test]
fn test_equality_false() {
    let value = common::eval("1 == 2");
    assert_eq!(value, TlangValue::Bool(false));
}

#[test]
fn test_inequality() {
    let value = common::eval("1 != 2");
    assert_eq!(value, TlangValue::Bool(true));
}

#[test]
fn test_less_than() {
    let value = common::eval("3 < 5");
    assert_eq!(value, TlangValue::Bool(true));
}

#[test]
fn test_greater_than() {
    let value = common::eval("5 > 3");
    assert_eq!(value, TlangValue::Bool(true));
}

#[test]
fn test_less_than_or_equal() {
    let value = common::eval("3 <= 3");
    assert_eq!(value, TlangValue::Bool(true));
}

#[test]
fn test_greater_than_or_equal() {
    let value = common::eval("4 >= 5");
    assert_eq!(value, TlangValue::Bool(false));
}

// ── Logic ───────────────────────────────────────────────────────────────────

#[test]
fn test_logical_and_true() {
    let value = common::eval("true && true");
    assert_eq!(value, TlangValue::Bool(true));
}

#[test]
fn test_logical_and_false() {
    let value = common::eval("true && false");
    assert_eq!(value, TlangValue::Bool(false));
}

#[test]
fn test_logical_or_true() {
    let value = common::eval("false || true");
    assert_eq!(value, TlangValue::Bool(true));
}

#[test]
fn test_logical_not() {
    let value = common::eval("!true");
    assert_eq!(value, TlangValue::Bool(false));
}

// ── Variables ───────────────────────────────────────────────────────────────

#[test]
fn test_let_binding() {
    let value = common::eval("let x = 10; x");
    assert_eq!(value, TlangValue::U64(10));
}

#[test]
fn test_let_shadowing() {
    let value = common::eval("let x = 1; let x = 2; x");
    assert_eq!(value, TlangValue::U64(2));
}

#[test]
fn test_let_list_pattern() {
    let value = common::eval("let [a, b] = [10, 20]; a + b");
    assert_eq!(value, TlangValue::U64(30));
}

// ── If/else ─────────────────────────────────────────────────────────────────

#[test]
fn test_if_true_branch() {
    let value = common::eval("if true { 1 } else { 2 }");
    assert_eq!(value, TlangValue::U64(1));
}

#[test]
fn test_if_false_branch() {
    let value = common::eval("if false { 1 } else { 2 }");
    assert_eq!(value, TlangValue::U64(2));
}

#[test]
fn test_if_else_if() {
    let value = common::eval("let x = 5; if x < 0 { -1 } else if x == 0 { 0 } else { 1 }");
    assert_eq!(value, TlangValue::U64(1));
}

#[test]
fn test_if_as_expression_in_let() {
    let value = common::eval("let x = if true { 42 } else { 0 }; x");
    assert_eq!(value, TlangValue::U64(42));
}

// ── Lists ───────────────────────────────────────────────────────────────────

#[test]
fn test_empty_list() {
    let s = common::eval_to_string("[]");
    assert_eq!(s, "[]");
}

#[test]
fn test_list_literal() {
    let s = common::eval_to_string("[1, 2, 3]");
    assert_eq!(s, "[1, 2, 3]");
}

#[test]
fn test_list_index_access() {
    let value = common::eval("let xs = [10, 20, 30]; xs[1]");
    assert_eq!(value, TlangValue::U64(20));
}

#[test]
fn test_list_spread() {
    let s = common::eval_to_string("let xs = [1, 2]; [0, ...xs, 3]");
    assert_eq!(s, "[0, 1, 2, 3]");
}

// ── Pipeline ─────────────────────────────────────────────────────────────────

#[test]
fn test_pipeline_operator() {
    let value = common::eval(
        "fn double(x) { x * 2 }
         10 |> double()",
    );
    assert_eq!(value, TlangValue::U64(20));
}

#[test]
fn test_pipeline_chain() {
    let value = common::eval(
        "fn add1(x) { x + 1 }
         fn double(x) { x * 2 }
         5 |> add1() |> double()",
    );
    assert_eq!(value, TlangValue::U64(12));
}
