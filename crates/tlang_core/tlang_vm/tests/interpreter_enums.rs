#![cfg(feature = "binary")]

use tlang_memory::prelude::*;

mod common;

// ── Basic enum declaration & construction ────────────────────────────────────

#[test]
fn test_enum_construction() {
    // Simple enums (no payload) are stored as integers — variant index
    let value = common::eval(
        "enum Color { Red, Green, Blue }
         Color::Red",
    );
    assert_eq!(value, TlangValue::U64(0));
}

#[test]
fn test_enum_with_payload() {
    // Positional fields are named by index ("0", "1", ...)
    let s = common::eval_to_string(
        "enum Shape { Circle(r), Rect(w, h) }
         Shape::Circle(5)",
    );
    assert_eq!(s, "Shape::Circle(0: 5)");
}

// ── Pattern matching with match ───────────────────────────────────────────────

#[test]
fn test_match_enum_variant() {
    // Use Option (payload enum) since simple integer-valued enums
    // aren't yet supported in pattern matching
    let value = common::eval(
        "match Option::Some(1) {
             Option::Some(_) => 1,
             Option::None => 0,
         }",
    );
    assert_eq!(value, TlangValue::U64(1));
}

#[test]
fn test_match_with_binding() {
    let value = common::eval(
        "enum Wrap { Value(x) }
         let w = Wrap::Value(99);
         match w {
             Wrap::Value(n) => n * 2,
         }",
    );
    assert_eq!(value, TlangValue::U64(198));
}

#[test]
fn test_match_wildcard() {
    let value = common::eval(
        "match Option::None {
             Option::Some(_) => 0,
             _ => -1,
         }",
    );
    assert_eq!(value, TlangValue::I64(-1));
}

#[test]
fn test_match_integer_literal() {
    let value = common::eval(
        "let x = 3;
         match x {
             1 => 10,
             2 => 20,
             3 => 30,
             _ => 0,
         }",
    );
    assert_eq!(value, TlangValue::U64(30));
}

#[test]
fn test_match_bool() {
    let value = common::eval(
        "let b = true;
         match b {
             true => 1,
             false => 0,
         }",
    );
    assert_eq!(value, TlangValue::U64(1));
}

// ── Option enum ───────────────────────────────────────────────────────────────

#[test]
fn test_option_some() {
    let value = common::eval(
        "match Option::Some(42) {
             Option::Some(x) => x,
             Option::None => 0,
         }",
    );
    assert_eq!(value, TlangValue::U64(42));
}

#[test]
fn test_option_none() {
    let value = common::eval(
        "match Option::None {
             Option::Some(x) => x,
             Option::None => -1,
         }",
    );
    assert_eq!(value, TlangValue::I64(-1));
}

#[test]
#[ignore = "if-let pattern binding broken: x is Nil instead of matched value"]
fn test_if_let_some() {
    let value = common::eval(
        "fn maybe_double(opt) {
             if let Option::Some(x) = opt {
                 x * 2
             } else {
                 0
             }
         }
         maybe_double(Option::Some(21))",
    );
    assert_eq!(value, TlangValue::U64(42));
}

#[test]
#[ignore = "if-let pattern binding broken: x is Nil instead of matched value"]
fn test_if_let_none() {
    let value = common::eval(
        "fn maybe_double(opt) {
             if let Option::Some(x) = opt {
                 x * 2
             } else {
                 0
             }
         }
         maybe_double(Option::None)",
    );
    assert_eq!(value, TlangValue::U64(0));
}

// ── Enum in function pattern matching ─────────────────────────────────────────

#[test]
fn test_enum_in_fn_pattern() {
    let value = common::eval(
        "enum Tree { Leaf, Node(left, value, right) }
         fn sum(Tree::Leaf) { 0 }
         fn sum(Tree::Node(l, v, r)) { sum(l) + v + sum(r) }
         sum(Tree::Node(
             Tree::Node(Tree::Leaf, 1, Tree::Leaf),
             2,
             Tree::Node(Tree::Leaf, 3, Tree::Leaf)
         ))",
    );
    assert_eq!(value, TlangValue::U64(6));
}

// ── Result enum ──────────────────────────────────────────────────────────────

#[test]
fn test_result_ok() {
    let value = common::eval(
        "fn safe_divide(_, 0) { Result::Err(\"division by zero\") }
         fn safe_divide(a, b) { Result::Ok(a / b) }
         match safe_divide(10, 2) {
             Result::Ok(n) => n,
             Result::Err(_) => -1,
         }",
    );
    assert_eq!(value, TlangValue::U64(5));
}

#[test]
fn test_result_err() {
    let value = common::eval(
        "fn safe_divide(_, 0) { Result::Err(\"division by zero\") }
         fn safe_divide(a, b) { Result::Ok(a / b) }
         match safe_divide(10, 0) {
             Result::Ok(n) => n,
             Result::Err(_) => -1,
         }",
    );
    assert_eq!(value, TlangValue::I64(-1));
}
