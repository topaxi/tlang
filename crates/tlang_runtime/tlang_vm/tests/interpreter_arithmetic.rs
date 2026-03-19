#![cfg(feature = "binary")]

// The constant folder optimizes pure literal arithmetic at compile time
// (e.g. `10 / 3` → integer 3, `5 - 10` → saturated 0). To test the runtime
// arithmetic paths in `tlang_memory::value::arithmetic`, all operations here
// are wrapped in thin functions so the optimizer cannot fold them away.

use tlang_memory::prelude::*;

mod common;

fn eval_binop(op: &str, a: &str, b: &str) -> TlangValue {
    let src = format!("fn op(a, b) {{ a {op} b }}\nop({a}, {b})");
    common::eval(&src)
}

// ── Signed integer arithmetic ─────────────────────────────────────────────────

#[test]
fn test_signed_integer_addition() {
    // Both operands are explicitly negative → I64 + I64
    let value = eval_binop("+", "(-3)", "(-5)");
    assert_eq!(value, TlangValue::I64(-8));
}

#[test]
fn test_signed_multiplication() {
    // Both operands must be signed (I64) to stay in int_arithmetic
    let value = eval_binop("*", "(-4)", "(-3)");
    assert_eq!(value, TlangValue::I64(12));
}

#[test]
fn test_signed_integer_large_exponent() {
    // Signed base, exponent > 32 → falls back to float in int_arithmetic
    let value = eval_binop("**", "(-2)", "33");
    assert!(matches!(value, TlangValue::F64(_)));
}

// ── Unsigned subtraction underflow ────────────────────────────────────────────

#[test]
fn test_unsigned_subtraction_underflow_to_float() {
    // 5u64 - 10u64 overflows → implementation falls back to F64
    let value = eval_binop("-", "5", "10");
    assert!(matches!(value, TlangValue::F64(f) if (f - (-5.0)).abs() < 1e-10));
}

// ── Division edge cases ────────────────────────────────────────────────────────

#[test]
fn test_integer_division_non_exact() {
    // 10 / 3 is not evenly divisible → F64 at runtime
    let value = eval_binop("/", "10", "3");
    assert!(matches!(value, TlangValue::F64(f) if (f - 10.0 / 3.0).abs() < 1e-10));
}

#[test]
fn test_integer_division_by_zero() {
    let value = eval_binop("/", "1", "0");
    assert!(matches!(value, TlangValue::F64(f) if f.is_infinite() && f > 0.0));
}

#[test]
fn test_float_division_by_zero() {
    let value = eval_binop("/", "1.0", "0.0");
    assert!(matches!(value, TlangValue::F64(f) if f.is_infinite() && f > 0.0));
}

// ── Modulo edge cases ──────────────────────────────────────────────────────────

#[test]
fn test_integer_modulo_by_zero() {
    let value = eval_binop("%", "5", "0");
    assert!(matches!(value, TlangValue::F64(f) if f.is_nan()));
}

#[test]
fn test_float_modulo_by_zero() {
    let value = eval_binop("%", "5.0", "0.0");
    assert!(matches!(value, TlangValue::F64(f) if f.is_nan()));
}

#[test]
fn test_float_modulo() {
    let value = eval_binop("%", "10.0", "3.0");
    assert!(matches!(value, TlangValue::F64(f) if (f - 1.0).abs() < 1e-10));
}

// ── Exponentiation edge cases ─────────────────────────────────────────────────

#[test]
fn test_unsigned_large_exponent_falls_back_to_float() {
    // Exponent > 32 falls back to float in uint_arithmetic
    let value = eval_binop("**", "2", "33");
    assert!(matches!(value, TlangValue::F64(_)));
}

#[test]
fn test_negative_integer_exponent() {
    // Mixed types (U64 base, I64 exponent) → float arithmetic path
    let value = eval_binop("**", "2", "(-1)");
    assert!(matches!(value, TlangValue::F64(f) if (f - 0.5).abs() < 1e-10));
}

#[test]
fn test_signed_negative_exponent() {
    // Both signed: int_arithmetic handles r < 0 → F64
    let value = eval_binop("**", "(-2)", "(-1)");
    assert!(matches!(value, TlangValue::F64(f) if (f - (-0.5)).abs() < 1e-10));
}

#[test]
fn test_signed_zero_base_negative_exponent() {
    // 0 ** -n = infinity in int_arithmetic
    let value = eval_binop("**", "(-0)", "(-1)");
    assert!(matches!(value, TlangValue::F64(f) if f.is_infinite()));
}

// ── Float arithmetic ───────────────────────────────────────────────────────────

#[test]
fn test_float_subtraction() {
    let value = eval_binop("-", "5.0", "2.5");
    assert!(matches!(value, TlangValue::F64(f) if (f - 2.5).abs() < 1e-10));
}

#[test]
fn test_float_multiplication() {
    let value = eval_binop("*", "2.0", "3.5");
    assert!(matches!(value, TlangValue::F64(f) if (f - 7.0).abs() < 1e-10));
}

#[test]
fn test_float_power() {
    let value = eval_binop("**", "2.0", "3.0");
    assert!(matches!(value, TlangValue::F64(f) if (f - 8.0).abs() < 1e-10));
}

// ── Mixed int/float arithmetic ────────────────────────────────────────────────

#[test]
fn test_int_plus_float() {
    let value = eval_binop("+", "1", "0.5");
    assert!(matches!(value, TlangValue::F64(f) if (f - 1.5).abs() < 1e-10));
}

#[test]
fn test_float_plus_int() {
    let value = eval_binop("+", "0.5", "1");
    assert!(matches!(value, TlangValue::F64(f) if (f - 1.5).abs() < 1e-10));
}

#[test]
fn test_int_times_float() {
    let value = eval_binop("*", "3", "1.5");
    assert!(matches!(value, TlangValue::F64(f) if (f - 4.5).abs() < 1e-10));
}
