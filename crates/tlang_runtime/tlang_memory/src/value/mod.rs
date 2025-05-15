use crate::{InterpreterState, impl_from_tlang_value};

pub use self::object::TlangObjectId;

pub mod object;

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TlangPrimitive {
    Nil,
    Bool(bool),
    Int(i64),
    UInt(u64),
    Float(f64),
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TlangValue {
    Nil,
    Bool(bool),

    // Signed integers (small to large)
    I8(i64),
    I16(i64),
    I32(i64),
    I64(i64),

    // Unsigned integers (small to large)
    U8(u64),
    U16(u64),
    U32(u64),
    U64(u64),

    // Floating point types
    F32(f64),
    F64(f64),

    // Object (structs, closures, etc.)
    Object(TlangObjectId),
}

impl std::cmp::Eq for TlangValue {}

impl std::hash::Hash for TlangValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            TlangValue::Nil => 0.hash(state),
            TlangValue::Bool(b) => b.hash(state),

            TlangValue::U8(i) => i.hash(state),
            TlangValue::U16(i) => i.hash(state),
            TlangValue::U32(i) => i.hash(state),
            TlangValue::U64(i) => i.hash(state),

            TlangValue::I8(i) => i.hash(state),
            TlangValue::I16(i) => i.hash(state),
            TlangValue::I32(i) => i.hash(state),
            TlangValue::I64(i) => i.hash(state),

            TlangValue::F32(f) => f.to_bits().hash(state), // do we want to support this?
            TlangValue::F64(f) => f.to_bits().hash(state), // do we want to support this?

            TlangValue::Object(id) => id.hash(state),
        }
    }
}

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
    pub fn new_object(id: TlangObjectId) -> Self {
        TlangValue::Object(id)
    }

    pub fn get_object_id(self) -> Option<TlangObjectId> {
        match self {
            TlangValue::Object(id) => Some(id),
            _ => None,
        }
    }

    pub fn is_nil(self) -> bool {
        matches!(self, TlangValue::Nil)
    }

    pub fn is_object(self) -> bool {
        matches!(self, TlangValue::Object(_))
    }

    pub fn is_truthy(self, state: &InterpreterState) -> bool {
        match self {
            TlangValue::Nil => false,
            TlangValue::Bool(b) => b,

            TlangValue::U8(i) | TlangValue::U16(i) | TlangValue::U32(i) | TlangValue::U64(i) => {
                i != 0
            }

            TlangValue::I8(i) | TlangValue::I16(i) | TlangValue::I32(i) | TlangValue::I64(i) => {
                i != 0
            }

            TlangValue::F32(f) | TlangValue::F64(f) => f != 0.0,

            TlangValue::Object(id) => state
                .get_object_by_id(id)
                .is_some_and(|kind| kind.is_truthy()),
        }
    }

    pub fn as_primitive(self) -> TlangPrimitive {
        match self {
            TlangValue::Nil => TlangPrimitive::Nil,
            TlangValue::Bool(b) => TlangPrimitive::Bool(b),

            TlangValue::U8(i) => TlangPrimitive::UInt(i),
            TlangValue::U16(i) => TlangPrimitive::UInt(i),
            TlangValue::U32(i) => TlangPrimitive::UInt(i),
            TlangValue::U64(i) => TlangPrimitive::UInt(i),

            TlangValue::I8(i) => TlangPrimitive::Int(i),
            TlangValue::I16(i) => TlangPrimitive::Int(i),
            TlangValue::I32(i) => TlangPrimitive::Int(i),
            TlangValue::I64(i) => TlangPrimitive::Int(i),

            TlangValue::F32(f) => TlangPrimitive::Float(f),
            TlangValue::F64(f) => TlangPrimitive::Float(f),

            TlangValue::Object(_) => panic!("Cannot convert object to primitive"),
        }
    }

    pub fn as_usize(self) -> usize {
        match self.as_primitive() {
            TlangPrimitive::Nil => 0,
            TlangPrimitive::Bool(bool) => bool as usize,
            TlangPrimitive::Int(i) => i as usize,
            TlangPrimitive::UInt(i) => i as usize,
            TlangPrimitive::Float(f) => f as usize,
        }
    }

    pub fn as_f64(self) -> f64 {
        match self.as_primitive() {
            TlangPrimitive::Nil => f64::NAN,
            TlangPrimitive::Bool(bool) => bool as usize as f64,
            TlangPrimitive::Int(i) => i as f64,
            TlangPrimitive::UInt(i) => i as f64,
            TlangPrimitive::Float(f) => f,
        }
    }

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
                ArithmeticOp::Div if lhs % rhs == 0 => TlangValue::U64(lhs / rhs),
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

impl_from_tlang_value! {
    i8 => I8,
    i16 => I16,
    i32 => I32,
    i64 => I64,

    u8 => U8,
    u16 => U16,
    u32 => U32,
    u64 => U64,

    f32 => F32,
    f64 => F64,

    bool => Bool,
}

#[cfg(target_pointer_width = "32")]
impl_from_tlang_value! {
    isize => I32,
    usize => U32,
}

#[cfg(target_pointer_width = "64")]
impl_from_tlang_value! {
    isize => I64,
    usize => U64,
}

impl std::fmt::Display for TlangValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TlangValue::Nil => write!(f, "nil"),
            TlangValue::Bool(b) => write!(f, "{b}"),

            TlangValue::U8(i) => write!(f, "{i}"),
            TlangValue::U16(i) => write!(f, "{i}"),
            TlangValue::U32(i) => write!(f, "{i}"),
            TlangValue::U64(i) => write!(f, "{i}"),

            TlangValue::I8(i) => write!(f, "{i}"),
            TlangValue::I16(i) => write!(f, "{i}"),
            TlangValue::I32(i) => write!(f, "{i}"),
            TlangValue::I64(i) => write!(f, "{i}"),

            TlangValue::F32(fl) => write!(f, "{fl}"),
            TlangValue::F64(fl) => write!(f, "{fl}"),

            TlangValue::Object(id) => write!(f, "Object({id})"),
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
