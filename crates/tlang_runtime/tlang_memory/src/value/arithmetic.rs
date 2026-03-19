use super::{TlangPrimitive, TlangValue};

#[derive(Debug, Clone, Copy)]
enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
}

#[inline(always)]
fn arithmetic_op(op: ArithmeticOp, lhs: TlangValue, rhs: TlangValue) -> TlangValue {
    match (lhs.as_primitive(), rhs.as_primitive()) {
        (TlangPrimitive::Int(l), TlangPrimitive::Int(r)) => int_arithmetic(op, l, r),
        (TlangPrimitive::UInt(l), TlangPrimitive::UInt(r)) => uint_arithmetic(op, l, r),
        _ => {
            let lhs_f = to_f64(&lhs);
            let rhs_f = to_f64(&rhs);
            float_arithmetic(op, lhs_f, rhs_f)
        }
    }
}

// Integer × Integer operations
fn int_arithmetic(op: ArithmeticOp, l: i64, r: i64) -> TlangValue {
    use ArithmeticOp::*;
    use TlangValue::*;

    let l_f = l as f64;
    let r_f = r as f64;

    match op {
        Add => l.checked_add(r).map(I64).unwrap_or_else(|| F64(l_f + r_f)),
        Sub => l.checked_sub(r).map(I64).unwrap_or_else(|| F64(l_f - r_f)),
        Mul => l.checked_mul(r).map(I64).unwrap_or_else(|| F64(l_f * r_f)),
        Div => {
            if r == 0 {
                F64(f64::INFINITY)
            } else if l % r == 0 {
                I64(l / r)
            } else {
                F64(l_f / r_f)
            }
        }
        Rem => {
            if r == 0 {
                F64(f64::NAN)
            } else {
                I64(l % r)
            }
        }
        Pow => {
            if r < 0 {
                if l == 0 {
                    F64(f64::INFINITY)
                } else {
                    F64(l_f.powf(r_f))
                }
            } else if r <= 32 {
                // Small positive integer exponent → fast repeated squaring
                I64(int_pow(l, r as u32))
            } else {
                // large exponent → fallback to float
                F64(l_f.powf(r_f))
            }
        }
    }
}

// Unsigned × Unsigned operations
fn uint_arithmetic(op: ArithmeticOp, l: u64, r: u64) -> TlangValue {
    use ArithmeticOp::*;
    use TlangValue::*;

    let l_f = l as f64;
    let r_f = r as f64;

    match op {
        Add => l.checked_add(r).map(U64).unwrap_or_else(|| F64(l_f + r_f)),
        Sub => l.checked_sub(r).map(U64).unwrap_or_else(|| F64(l_f - r_f)),
        Mul => l.checked_mul(r).map(U64).unwrap_or_else(|| F64(l_f * r_f)),
        Div => {
            if r == 0 {
                F64(f64::INFINITY)
            } else if l.is_multiple_of(r) {
                U64(l / r)
            } else {
                F64(l_f / r_f)
            }
        }
        Rem => {
            if r == 0 {
                F64(f64::NAN)
            } else {
                U64(l % r)
            }
        }
        Pow => {
            if r <= 32 {
                U64(uint_pow(l, r as u32))
            } else {
                F64(l_f.powf(r_f))
            }
        }
    }
}

// Float / Mixed operations
fn float_arithmetic(op: ArithmeticOp, lhs: f64, rhs: f64) -> TlangValue {
    use ArithmeticOp::*;
    use TlangValue::*;

    match op {
        Add => F64(lhs + rhs),
        Sub => F64(lhs - rhs),
        Mul => F64(lhs * rhs),
        Div => {
            if rhs == 0.0 {
                F64(f64::INFINITY)
            } else {
                F64(lhs / rhs)
            }
        }
        Rem => {
            if rhs == 0.0 {
                F64(f64::NAN)
            } else {
                F64(lhs % rhs)
            }
        }
        Pow => F64(lhs.powf(rhs)),
    }
}

// Convert any numeric TlangValue to f64
#[inline(always)]
fn to_f64(v: &TlangValue) -> f64 {
    match v.as_primitive() {
        TlangPrimitive::Int(i) => i as f64,
        TlangPrimitive::UInt(u) => u as f64,
        TlangPrimitive::Float(f) => f,
        _ => panic!("Unsupported numeric operand: {:?}", v),
    }
}

// Fast integer exponentiation for small positive powers
#[inline(always)]
fn int_pow(mut base: i64, mut exp: u32) -> i64 {
    let mut result = 1i64;
    while exp > 0 {
        if exp & 1 == 1 {
            result = result.saturating_mul(base);
        }
        exp >>= 1;
        if exp > 0 {
            base = base.saturating_mul(base);
        }
    }
    result
}

// Fast unsigned exponentiation for small positive powers
#[inline(always)]
fn uint_pow(mut base: u64, mut exp: u32) -> u64 {
    let mut result = 1u64;
    while exp > 0 {
        if exp & 1 == 1 {
            result = result.saturating_mul(base);
        }
        exp >>= 1;
        if exp > 0 {
            base = base.saturating_mul(base);
        }
    }
    result
}

pub trait TlangArithmetic {
    fn add(self, rhs: Self) -> Self;
    fn sub(self, rhs: Self) -> Self;
    fn mul(self, rhs: Self) -> Self;
    fn div(self, rhs: Self) -> Self;
    fn rem(self, rhs: Self) -> Self;
    fn pow(self, rhs: Self) -> Self;
}

impl TlangArithmetic for TlangValue {
    #[inline]
    fn add(self, rhs: Self) -> Self {
        arithmetic_op(ArithmeticOp::Add, self, rhs)
    }

    #[inline]
    fn sub(self, rhs: Self) -> Self {
        arithmetic_op(ArithmeticOp::Sub, self, rhs)
    }

    #[inline]
    fn mul(self, rhs: Self) -> Self {
        arithmetic_op(ArithmeticOp::Mul, self, rhs)
    }

    #[inline]
    fn div(self, rhs: Self) -> Self {
        arithmetic_op(ArithmeticOp::Div, self, rhs)
    }

    #[inline]
    fn rem(self, rhs: Self) -> Self {
        arithmetic_op(ArithmeticOp::Rem, self, rhs)
    }

    #[inline]
    fn pow(self, rhs: Self) -> Self {
        arithmetic_op(ArithmeticOp::Pow, self, rhs)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::value::TlangValue;

    // ── Signed integer (I64) arithmetic ──────────────────────────────────────

    #[test]
    fn test_int_add() {
        assert_eq!(TlangValue::I64(3).add(TlangValue::I64(4)), TlangValue::I64(7));
    }

    #[test]
    fn test_int_sub() {
        assert_eq!(TlangValue::I64(10).sub(TlangValue::I64(3)), TlangValue::I64(7));
    }

    #[test]
    fn test_int_mul() {
        assert_eq!(TlangValue::I64(4).mul(TlangValue::I64(5)), TlangValue::I64(20));
    }

    #[test]
    fn test_int_div_exact() {
        assert_eq!(TlangValue::I64(12).div(TlangValue::I64(4)), TlangValue::I64(3));
    }

    #[test]
    fn test_int_div_non_exact() {
        // 10 / 3 is not exact → F64
        assert!(
            matches!(TlangValue::I64(10).div(TlangValue::I64(3)), TlangValue::F64(f) if (f - 10.0/3.0).abs() < 1e-10)
        );
    }

    #[test]
    fn test_int_div_by_zero() {
        let result = TlangValue::I64(5).div(TlangValue::I64(0));
        assert!(matches!(result, TlangValue::F64(f) if f.is_infinite() && f > 0.0));
    }

    #[test]
    fn test_int_rem() {
        assert_eq!(TlangValue::I64(17).rem(TlangValue::I64(5)), TlangValue::I64(2));
    }

    #[test]
    fn test_int_rem_by_zero() {
        let result = TlangValue::I64(5).rem(TlangValue::I64(0));
        assert!(matches!(result, TlangValue::F64(f) if f.is_nan()));
    }

    #[test]
    fn test_int_pow_small() {
        assert_eq!(TlangValue::I64(2).pow(TlangValue::I64(10)), TlangValue::I64(1024));
    }

    #[test]
    fn test_int_pow_large_exponent() {
        // Exponent > 32 falls back to float
        assert!(matches!(
            TlangValue::I64(2).pow(TlangValue::I64(33)),
            TlangValue::F64(_)
        ));
    }

    #[test]
    fn test_int_pow_negative_exponent() {
        // Negative exponent → float result
        let result = TlangValue::I64(2).pow(TlangValue::I64(-1));
        assert!(matches!(result, TlangValue::F64(f) if (f - 0.5).abs() < 1e-10));
    }

    #[test]
    fn test_int_pow_zero_base_negative_exponent() {
        let result = TlangValue::I64(0).pow(TlangValue::I64(-1));
        assert!(matches!(result, TlangValue::F64(f) if f.is_infinite()));
    }

    #[test]
    fn test_int_add_overflow_falls_back_to_float() {
        let result = TlangValue::I64(i64::MAX).add(TlangValue::I64(1));
        assert!(matches!(result, TlangValue::F64(_)));
    }

    #[test]
    fn test_int_sub_overflow_falls_back_to_float() {
        let result = TlangValue::I64(i64::MIN).sub(TlangValue::I64(1));
        assert!(matches!(result, TlangValue::F64(_)));
    }

    #[test]
    fn test_int_mul_overflow_falls_back_to_float() {
        let result = TlangValue::I64(i64::MAX).mul(TlangValue::I64(2));
        assert!(matches!(result, TlangValue::F64(_)));
    }

    // ── Unsigned integer (U64) arithmetic ────────────────────────────────────

    #[test]
    fn test_uint_add() {
        assert_eq!(TlangValue::U64(10).add(TlangValue::U64(5)), TlangValue::U64(15));
    }

    #[test]
    fn test_uint_sub() {
        assert_eq!(TlangValue::U64(10).sub(TlangValue::U64(3)), TlangValue::U64(7));
    }

    #[test]
    fn test_uint_sub_underflow_to_float() {
        // 5 - 10 underflows u64 → F64
        let result = TlangValue::U64(5).sub(TlangValue::U64(10));
        assert!(matches!(result, TlangValue::F64(f) if (f - (-5.0)).abs() < 1e-10));
    }

    #[test]
    fn test_uint_mul() {
        assert_eq!(TlangValue::U64(6).mul(TlangValue::U64(7)), TlangValue::U64(42));
    }

    #[test]
    fn test_uint_div_exact() {
        assert_eq!(TlangValue::U64(20).div(TlangValue::U64(4)), TlangValue::U64(5));
    }

    #[test]
    fn test_uint_div_non_exact() {
        assert!(
            matches!(TlangValue::U64(10).div(TlangValue::U64(3)), TlangValue::F64(f) if (f - 10.0/3.0).abs() < 1e-10)
        );
    }

    #[test]
    fn test_uint_div_by_zero() {
        let result = TlangValue::U64(5).div(TlangValue::U64(0));
        assert!(matches!(result, TlangValue::F64(f) if f.is_infinite() && f > 0.0));
    }

    #[test]
    fn test_uint_rem() {
        assert_eq!(TlangValue::U64(17).rem(TlangValue::U64(5)), TlangValue::U64(2));
    }

    #[test]
    fn test_uint_rem_by_zero() {
        let result = TlangValue::U64(5).rem(TlangValue::U64(0));
        assert!(matches!(result, TlangValue::F64(f) if f.is_nan()));
    }

    #[test]
    fn test_uint_pow_small() {
        assert_eq!(TlangValue::U64(2).pow(TlangValue::U64(8)), TlangValue::U64(256));
    }

    #[test]
    fn test_uint_pow_large_exponent() {
        // u64 exponent > 32 → float
        assert!(matches!(
            TlangValue::U64(2).pow(TlangValue::U64(33)),
            TlangValue::F64(_)
        ));
    }

    #[test]
    fn test_uint_add_overflow_falls_back_to_float() {
        let result = TlangValue::U64(u64::MAX).add(TlangValue::U64(1));
        assert!(matches!(result, TlangValue::F64(_)));
    }

    // ── Float arithmetic ──────────────────────────────────────────────────────

    #[test]
    fn test_float_add() {
        assert!(
            matches!(TlangValue::F64(1.5).add(TlangValue::F64(2.5)), TlangValue::F64(f) if (f - 4.0).abs() < 1e-10)
        );
    }

    #[test]
    fn test_float_sub() {
        assert!(
            matches!(TlangValue::F64(5.0).sub(TlangValue::F64(2.5)), TlangValue::F64(f) if (f - 2.5).abs() < 1e-10)
        );
    }

    #[test]
    fn test_float_mul() {
        assert!(
            matches!(TlangValue::F64(2.0).mul(TlangValue::F64(3.5)), TlangValue::F64(f) if (f - 7.0).abs() < 1e-10)
        );
    }

    #[test]
    fn test_float_div() {
        assert!(
            matches!(TlangValue::F64(10.0).div(TlangValue::F64(4.0)), TlangValue::F64(f) if (f - 2.5).abs() < 1e-10)
        );
    }

    #[test]
    fn test_float_div_by_zero() {
        let result = TlangValue::F64(1.0).div(TlangValue::F64(0.0));
        assert!(matches!(result, TlangValue::F64(f) if f.is_infinite() && f > 0.0));
    }

    #[test]
    fn test_float_rem() {
        assert!(
            matches!(TlangValue::F64(10.0).rem(TlangValue::F64(3.0)), TlangValue::F64(f) if (f - 1.0).abs() < 1e-10)
        );
    }

    #[test]
    fn test_float_rem_by_zero() {
        let result = TlangValue::F64(5.0).rem(TlangValue::F64(0.0));
        assert!(matches!(result, TlangValue::F64(f) if f.is_nan()));
    }

    #[test]
    fn test_float_pow() {
        assert!(
            matches!(TlangValue::F64(2.0).pow(TlangValue::F64(3.0)), TlangValue::F64(f) if (f - 8.0).abs() < 1e-10)
        );
    }

    // ── Mixed type arithmetic (int + float → float path) ─────────────────────

    #[test]
    fn test_mixed_int_float_add() {
        // I64 + F64 → falls through to float path
        let result = TlangValue::I64(1).add(TlangValue::F64(0.5));
        assert!(matches!(result, TlangValue::F64(f) if (f - 1.5).abs() < 1e-10));
    }

    #[test]
    fn test_mixed_uint_float_sub() {
        let result = TlangValue::U64(5).sub(TlangValue::F64(1.5));
        assert!(matches!(result, TlangValue::F64(f) if (f - 3.5).abs() < 1e-10));
    }
}
