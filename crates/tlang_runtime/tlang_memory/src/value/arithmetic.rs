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
