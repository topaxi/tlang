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

impl TlangValue {
    #[inline(always)]
    fn apply_arithmetic_op(self, rhs: Self, op: ArithmeticOp) -> Self {
        use TlangPrimitive::*;

        match (self.as_primitive(), rhs.as_primitive()) {
            (UInt(lhs), UInt(rhs)) => match op {
                ArithmeticOp::Add => TlangValue::U64(lhs + rhs),
                ArithmeticOp::Sub if lhs >= rhs => TlangValue::U64(lhs - rhs),
                ArithmeticOp::Sub if lhs as i64 >= 0 => TlangValue::I64(lhs as i64 - rhs as i64),
                ArithmeticOp::Sub => TlangValue::I64((lhs as i128 - rhs as i128) as i64),
                ArithmeticOp::Mul => TlangValue::U64(lhs * rhs),
                ArithmeticOp::Div if lhs.is_multiple_of(rhs) => TlangValue::U64(lhs / rhs),
                ArithmeticOp::Div => TlangValue::F64(lhs as f64 / rhs as f64),
                ArithmeticOp::Rem => TlangValue::U64(lhs % rhs),
                ArithmeticOp::Pow => TlangValue::U64(lhs.pow(rhs as u32)),
            },
            (Int(lhs), Int(rhs)) => match op {
                ArithmeticOp::Add => TlangValue::I64(lhs + rhs),
                ArithmeticOp::Sub => TlangValue::I64(lhs - rhs),
                ArithmeticOp::Mul => TlangValue::I64(lhs * rhs),
                ArithmeticOp::Div if lhs % rhs == 0 => TlangValue::I64(lhs / rhs),
                ArithmeticOp::Div => TlangValue::F64(lhs as f64 / rhs as f64),
                ArithmeticOp::Rem => TlangValue::I64(lhs % rhs),
                ArithmeticOp::Pow if rhs >= 0 => TlangValue::I64(lhs.pow(rhs as u32)),
                ArithmeticOp::Pow => TlangValue::I64(lhs.pow(rhs as u32)),
            },
            (UInt(lhs), Int(rhs)) => match op {
                ArithmeticOp::Add => TlangValue::I64(lhs as i64 + rhs),
                ArithmeticOp::Sub => TlangValue::I64(lhs as i64 - rhs),
                ArithmeticOp::Mul => TlangValue::I64(lhs as i64 * rhs),
                ArithmeticOp::Div if lhs as i64 % rhs == 0 => TlangValue::I64(lhs as i64 / rhs),
                ArithmeticOp::Div => TlangValue::F64(lhs as f64 / rhs as f64),
                ArithmeticOp::Rem => TlangValue::I64(lhs as i64 % rhs),
                ArithmeticOp::Pow if rhs >= 0 => TlangValue::I64((lhs as i64).pow(rhs as u32)),
                ArithmeticOp::Pow => TlangValue::I64((lhs as i64).pow(rhs as u32)),
            },
            (Int(lhs), UInt(rhs)) => match op {
                ArithmeticOp::Add => TlangValue::I64(lhs + rhs as i64),
                ArithmeticOp::Sub => TlangValue::I64(lhs - rhs as i64),
                ArithmeticOp::Mul => TlangValue::I64(lhs * rhs as i64),
                ArithmeticOp::Div if lhs % rhs as i64 == 0 => TlangValue::I64(lhs / rhs as i64),
                ArithmeticOp::Div => TlangValue::F64(lhs as f64 / rhs as f64),
                ArithmeticOp::Rem => TlangValue::I64(lhs % rhs as i64),
                ArithmeticOp::Pow => TlangValue::I64(lhs.pow(rhs as u32)),
            },
            (Float(lhs), Float(rhs)) => match op {
                ArithmeticOp::Add => TlangValue::F64(lhs + rhs),
                ArithmeticOp::Sub => TlangValue::F64(lhs - rhs),
                ArithmeticOp::Mul => TlangValue::F64(lhs * rhs),
                ArithmeticOp::Div => TlangValue::F64(lhs / rhs),
                ArithmeticOp::Rem => TlangValue::F64(lhs % rhs),
                ArithmeticOp::Pow => TlangValue::F64(lhs.powf(rhs)),
            },
            (UInt(lhs), Float(rhs)) => match op {
                ArithmeticOp::Add => TlangValue::F64(lhs as f64 + rhs),
                ArithmeticOp::Sub => TlangValue::F64(lhs as f64 - rhs),
                ArithmeticOp::Mul => TlangValue::F64(lhs as f64 * rhs),
                ArithmeticOp::Div => TlangValue::F64(lhs as f64 / rhs),
                ArithmeticOp::Rem => TlangValue::F64(lhs as f64 % rhs),
                ArithmeticOp::Pow => TlangValue::F64((lhs as f64).powf(rhs)),
            },
            (Float(lhs), UInt(rhs)) => match op {
                ArithmeticOp::Add => TlangValue::F64(lhs + rhs as f64),
                ArithmeticOp::Sub => TlangValue::F64(lhs - rhs as f64),
                ArithmeticOp::Mul => TlangValue::F64(lhs * rhs as f64),
                ArithmeticOp::Div => TlangValue::F64(lhs / rhs as f64),
                ArithmeticOp::Rem => TlangValue::F64(lhs % rhs as f64),
                ArithmeticOp::Pow => TlangValue::F64(lhs.powf(rhs as f64)),
            },
            (Int(lhs), Float(rhs)) => match op {
                ArithmeticOp::Add => TlangValue::F64(lhs as f64 + rhs),
                ArithmeticOp::Sub => TlangValue::F64(lhs as f64 - rhs),
                ArithmeticOp::Mul => TlangValue::F64(lhs as f64 * rhs),
                ArithmeticOp::Div => TlangValue::F64(lhs as f64 / rhs),
                ArithmeticOp::Rem => TlangValue::F64(lhs as f64 % rhs),
                ArithmeticOp::Pow => TlangValue::F64((lhs as f64).powf(rhs)),
            },
            (Float(lhs), Int(rhs)) => match op {
                ArithmeticOp::Add => TlangValue::F64(lhs + rhs as f64),
                ArithmeticOp::Sub => TlangValue::F64(lhs - rhs as f64),
                ArithmeticOp::Mul => TlangValue::F64(lhs * rhs as f64),
                ArithmeticOp::Div => TlangValue::F64(lhs / rhs as f64),
                ArithmeticOp::Rem => TlangValue::F64(lhs % rhs as f64),
                ArithmeticOp::Pow => TlangValue::F64(lhs.powf(rhs as f64)),
            },
            (lhs, rhs) => panic!("Unsupported operation: {lhs:?} {op:?} {rhs:?}"),
        }
    }
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
    #[inline(always)]
    fn add(self, rhs: Self) -> Self {
        self.apply_arithmetic_op(rhs, ArithmeticOp::Add)
    }

    #[inline(always)]
    fn sub(self, rhs: Self) -> Self {
        self.apply_arithmetic_op(rhs, ArithmeticOp::Sub)
    }

    #[inline(always)]
    fn mul(self, rhs: Self) -> Self {
        self.apply_arithmetic_op(rhs, ArithmeticOp::Mul)
    }

    #[inline(always)]
    fn div(self, rhs: Self) -> Self {
        self.apply_arithmetic_op(rhs, ArithmeticOp::Div)
    }

    #[inline(always)]
    fn rem(self, rhs: Self) -> Self {
        self.apply_arithmetic_op(rhs, ArithmeticOp::Rem)
    }

    #[inline(always)]
    fn pow(self, rhs: Self) -> Self {
        self.apply_arithmetic_op(rhs, ArithmeticOp::Pow)
    }
}
