#![cfg(feature = "binary")]

use tlang_memory::prelude::*;

mod common;

// ── Struct declaration & construction ────────────────────────────────────────

#[test]
fn test_struct_construction() {
    let s = common::eval_to_string(
        "struct Point { x: isize, y: isize }
         Point { x: 3, y: 4 }",
    );
    assert_eq!(s, "Point { x: 3, y: 4 }");
}

#[test]
fn test_struct_field_access() {
    let value = common::eval(
        "struct Point { x: isize, y: isize }
         let p = Point { x: 10, y: 20 };
         p.x + p.y",
    );
    assert_eq!(value, TlangValue::U64(30));
}

#[test]
fn test_struct_with_method() {
    let value = common::eval(
        "struct Point { x: isize, y: isize }
         fn Point.distance_sq(self) { self.x * self.x + self.y * self.y }
         let p = Point { x: 3, y: 4 };
         p.distance_sq()",
    );
    assert_eq!(value, TlangValue::U64(25));
}

#[test]
fn test_struct_nested() {
    let value = common::eval(
        "struct Vec2 { x: isize, y: isize }
         struct Rect { origin: Vec2, size: Vec2 }
         let r = Rect {
             origin: Vec2 { x: 0, y: 0 },
             size: Vec2 { x: 100, y: 50 },
         };
         r.size.x * r.size.y",
    );
    assert_eq!(value, TlangValue::U64(5000));
}

// ── Struct in pattern matching ────────────────────────────────────────────────

#[test]
fn test_struct_method_chaining() {
    let s = common::eval_to_string(
        "struct Counter { count: isize }
         fn Counter.increment(self) { Counter { count: self.count + 1 } }
         fn Counter.value(self) { self.count }
         let c = Counter { count: 0 };
         c.increment().increment().increment().value()",
    );
    assert_eq!(s, "3");
}

// ── Math stdlib ──────────────────────────────────────────────────────────────

#[test]
fn test_math_floor() {
    let value = common::eval("math::floor(3.7)");
    assert!(matches!(value, TlangValue::F64(f) if (f - 3.0).abs() < 1e-10));
}

#[test]
fn test_math_floor_integer() {
    let value = common::eval("math::floor(5)");
    assert_eq!(value, TlangValue::U64(5));
}

#[test]
fn test_math_sqrt() {
    let value = common::eval("math::sqrt(9.0)");
    assert!(matches!(value, TlangValue::F64(f) if (f - 3.0).abs() < 1e-10));
}

#[test]
fn test_math_max_integers() {
    let value = common::eval("math::max(10, 20)");
    assert_eq!(value, TlangValue::U64(20));
}

#[test]
fn test_math_max_floats() {
    let value = common::eval("math::max(1.5, 2.5)");
    assert!(matches!(value, TlangValue::F64(f) if (f - 2.5).abs() < 1e-10));
}

#[test]
fn test_math_pi() {
    let value = common::eval("math::pi");
    assert!(matches!(value, TlangValue::F64(f) if (f - std::f64::consts::PI).abs() < 1e-10));
}

// ── String operations ─────────────────────────────────────────────────────────

#[test]
fn test_string_concatenation() {
    let s = common::eval_to_string(r#""hello" + " world""#);
    assert_eq!(s, "hello world");
}

#[test]
fn test_string_from_char_code() {
    let s = common::eval_to_string("string::from_char_code(65)");
    assert_eq!(s, "A");
}

// ── For loops ────────────────────────────────────────────────────────────────

#[test]
fn test_for_loop_with_accumulator() {
    let value = common::eval(
        "let total = for x in [1, 2, 3, 4, 5]; with sum = 0 {
             sum + x
         };
         total",
    );
    assert_eq!(value, TlangValue::U64(15));
}

#[test]
fn test_for_loop_max_value() {
    let value = common::eval(
        "let result = for x in [3, 1, 4, 1, 5, 9, 2, 6]; with max_val = 0 {
             if x > max_val { x } else { max_val }
         };
         result",
    );
    assert_eq!(value, TlangValue::U64(9));
}

#[test]
fn test_for_loop_collect() {
    let s = common::eval_to_string(
        "let doubled = for x in [1, 2, 3]; with acc = [] {
             [...acc, x * 2]
         };
         doubled",
    );
    assert_eq!(s, "[2, 4, 6]");
}

#[test]
fn test_nested_for_loop() {
    let value = common::eval(
        "let result = for row in [[1, 2], [3, 4], [5, 6]]; with total = 0 {
             let row_sum = for x in row; with s = 0 { s + x };
             total + row_sum
         };
         result",
    );
    assert_eq!(value, TlangValue::U64(21));
}
