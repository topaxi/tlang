use crate::impl_from_tlang_value;

mod arithmetic;

pub use self::object::{ReferencedValuesIter, TlangObjectId};
pub use arithmetic::TlangArithmetic;

pub mod function;
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
        // Include a discriminant so different types with the same inner value
        // produce different hashes (e.g. U8(1) vs U16(1) vs I8(1)).
        std::mem::discriminant(self).hash(state);
        match self {
            TlangValue::Nil => {}
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

    /// # Panics
    pub fn is_truthy(self) -> bool {
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

            TlangValue::Object(_) => {
                panic!("Truthiness of objects must be checked through the interpreter state")
            }
        }
    }

    /// # Panics
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

#[cfg(test)]
mod tests {
    use std::collections::hash_map::DefaultHasher;
    use std::hash::{Hash, Hasher};

    use super::*;

    fn hash_of(v: TlangValue) -> u64 {
        let mut h = DefaultHasher::new();
        v.hash(&mut h);
        h.finish()
    }

    // ── is_nil ────────────────────────────────────────────────────────────────

    #[test]
    fn test_is_nil_true() {
        assert!(TlangValue::Nil.is_nil());
    }

    #[test]
    fn test_is_nil_false_for_bool() {
        assert!(!TlangValue::Bool(false).is_nil());
    }

    #[test]
    fn test_is_nil_false_for_int() {
        assert!(!TlangValue::I64(0).is_nil());
    }

    // ── is_object ─────────────────────────────────────────────────────────────

    #[test]
    fn test_is_object_true() {
        let id = TlangObjectId::from(42usize);
        assert!(TlangValue::Object(id).is_object());
    }

    #[test]
    fn test_is_object_false_for_nil() {
        assert!(!TlangValue::Nil.is_object());
    }

    #[test]
    fn test_get_object_id_some() {
        let id = TlangObjectId::from(7usize);
        assert_eq!(TlangValue::Object(id).get_object_id(), Some(id));
    }

    #[test]
    fn test_get_object_id_none_for_primitive() {
        assert_eq!(TlangValue::U64(0).get_object_id(), None);
    }

    // ── is_truthy ─────────────────────────────────────────────────────────────

    #[test]
    fn test_is_truthy_nil_is_false() {
        assert!(!TlangValue::Nil.is_truthy());
    }

    #[test]
    fn test_is_truthy_bool_true() {
        assert!(TlangValue::Bool(true).is_truthy());
    }

    #[test]
    fn test_is_truthy_bool_false() {
        assert!(!TlangValue::Bool(false).is_truthy());
    }

    #[test]
    fn test_is_truthy_nonzero_u64() {
        assert!(TlangValue::U64(1).is_truthy());
    }

    #[test]
    fn test_is_truthy_zero_u64() {
        assert!(!TlangValue::U64(0).is_truthy());
    }

    #[test]
    fn test_is_truthy_nonzero_i64() {
        assert!(TlangValue::I64(-1).is_truthy());
    }

    #[test]
    fn test_is_truthy_zero_i64() {
        assert!(!TlangValue::I64(0).is_truthy());
    }

    #[test]
    fn test_is_truthy_nonzero_f64() {
        assert!(TlangValue::F64(0.1).is_truthy());
    }

    #[test]
    fn test_is_truthy_zero_f64() {
        assert!(!TlangValue::F64(0.0).is_truthy());
    }

    #[test]
    fn test_is_truthy_nonzero_u8() {
        assert!(TlangValue::U8(5).is_truthy());
    }

    #[test]
    fn test_is_truthy_zero_u8() {
        assert!(!TlangValue::U8(0).is_truthy());
    }

    #[test]
    fn test_is_truthy_nonzero_u16() {
        assert!(TlangValue::U16(100).is_truthy());
    }

    #[test]
    fn test_is_truthy_nonzero_u32() {
        assert!(TlangValue::U32(1).is_truthy());
    }

    #[test]
    fn test_is_truthy_nonzero_i8() {
        assert!(TlangValue::I8(1).is_truthy());
    }

    #[test]
    fn test_is_truthy_nonzero_i16() {
        assert!(TlangValue::I16(-10).is_truthy());
    }

    #[test]
    fn test_is_truthy_nonzero_i32() {
        assert!(TlangValue::I32(42).is_truthy());
    }

    #[test]
    fn test_is_truthy_nonzero_f32() {
        assert!(TlangValue::F32(1.0).is_truthy());
    }

    #[test]
    fn test_is_truthy_zero_f32() {
        assert!(!TlangValue::F32(0.0).is_truthy());
    }

    // ── as_primitive ──────────────────────────────────────────────────────────

    #[test]
    fn test_as_primitive_nil() {
        assert_eq!(TlangValue::Nil.as_primitive(), TlangPrimitive::Nil);
    }

    #[test]
    fn test_as_primitive_bool() {
        assert_eq!(
            TlangValue::Bool(true).as_primitive(),
            TlangPrimitive::Bool(true)
        );
    }

    #[test]
    fn test_as_primitive_u8() {
        assert_eq!(TlangValue::U8(3).as_primitive(), TlangPrimitive::UInt(3));
    }

    #[test]
    fn test_as_primitive_u16() {
        assert_eq!(TlangValue::U16(10).as_primitive(), TlangPrimitive::UInt(10));
    }

    #[test]
    fn test_as_primitive_u32() {
        assert_eq!(TlangValue::U32(42).as_primitive(), TlangPrimitive::UInt(42));
    }

    #[test]
    fn test_as_primitive_u64() {
        assert_eq!(TlangValue::U64(99).as_primitive(), TlangPrimitive::UInt(99));
    }

    #[test]
    fn test_as_primitive_i8() {
        assert_eq!(TlangValue::I8(-1).as_primitive(), TlangPrimitive::Int(-1));
    }

    #[test]
    fn test_as_primitive_i16() {
        assert_eq!(TlangValue::I16(-5).as_primitive(), TlangPrimitive::Int(-5));
    }

    #[test]
    fn test_as_primitive_i32() {
        assert_eq!(
            TlangValue::I32(-100).as_primitive(),
            TlangPrimitive::Int(-100)
        );
    }

    #[test]
    fn test_as_primitive_i64() {
        assert_eq!(
            TlangValue::I64(-999).as_primitive(),
            TlangPrimitive::Int(-999)
        );
    }

    #[test]
    fn test_as_primitive_f32() {
        assert!(
            matches!(TlangValue::F32(1.5).as_primitive(), TlangPrimitive::Float(f) if (f - 1.5).abs() < 1e-6)
        );
    }

    #[test]
    #[allow(clippy::approx_constant)]
    fn test_as_primitive_f64() {
        assert!(
            matches!(TlangValue::F64(3.14).as_primitive(), TlangPrimitive::Float(f) if (f - 3.14).abs() < 1e-10)
        );
    }

    // ── as_usize ──────────────────────────────────────────────────────────────

    #[test]
    fn test_as_usize_u64() {
        assert_eq!(TlangValue::U64(5).as_usize(), 5usize);
    }

    #[test]
    fn test_as_usize_i64() {
        assert_eq!(TlangValue::I64(7).as_usize(), 7usize);
    }

    #[test]
    fn test_as_usize_f64() {
        assert_eq!(TlangValue::F64(3.9).as_usize(), 3usize);
    }

    #[test]
    fn test_as_usize_bool_true() {
        assert_eq!(TlangValue::Bool(true).as_usize(), 1usize);
    }

    #[test]
    fn test_as_usize_bool_false() {
        assert_eq!(TlangValue::Bool(false).as_usize(), 0usize);
    }

    #[test]
    fn test_as_usize_nil() {
        assert_eq!(TlangValue::Nil.as_usize(), 0usize);
    }

    // ── as_f64 ────────────────────────────────────────────────────────────────

    #[test]
    fn test_as_f64_u64() {
        assert!((TlangValue::U64(10).as_f64() - 10.0).abs() < 1e-10);
    }

    #[test]
    fn test_as_f64_i64() {
        assert!((TlangValue::I64(-3).as_f64() - (-3.0)).abs() < 1e-10);
    }

    #[test]
    fn test_as_f64_f64() {
        assert!((TlangValue::F64(2.5).as_f64() - 2.5).abs() < 1e-10);
    }

    #[test]
    fn test_as_f64_bool_true() {
        assert!((TlangValue::Bool(true).as_f64() - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_as_f64_nil_is_nan() {
        assert!(TlangValue::Nil.as_f64().is_nan());
    }

    // ── From<T> conversions ────────────────────────────────────────────────────

    #[test]
    fn test_from_i64() {
        assert_eq!(TlangValue::from(-42i64), TlangValue::I64(-42));
    }

    #[test]
    fn test_from_u64() {
        assert_eq!(TlangValue::from(99u64), TlangValue::U64(99));
    }

    #[test]
    fn test_from_f64() {
        assert!(matches!(TlangValue::from(1.5f64), TlangValue::F64(f) if (f - 1.5).abs() < 1e-10));
    }

    #[test]
    fn test_from_bool() {
        assert_eq!(TlangValue::from(true), TlangValue::Bool(true));
    }

    #[test]
    fn test_from_i8() {
        assert_eq!(TlangValue::from(-1i8), TlangValue::I8(-1));
    }

    #[test]
    fn test_from_i16() {
        assert_eq!(TlangValue::from(-100i16), TlangValue::I16(-100));
    }

    #[test]
    fn test_from_i32() {
        assert_eq!(TlangValue::from(-1000i32), TlangValue::I32(-1000));
    }

    #[test]
    fn test_from_u8() {
        assert_eq!(TlangValue::from(255u8), TlangValue::U8(255));
    }

    #[test]
    fn test_from_u16() {
        assert_eq!(TlangValue::from(1000u16), TlangValue::U16(1000));
    }

    #[test]
    fn test_from_u32() {
        assert_eq!(TlangValue::from(100_000u32), TlangValue::U32(100_000));
    }

    #[test]
    fn test_from_f32() {
        assert!(matches!(TlangValue::from(2.5f32), TlangValue::F32(f) if (f - 2.5).abs() < 1e-5));
    }

    #[test]
    fn test_from_usize() {
        let v = TlangValue::from(42usize);
        // usize maps to U64 on 64-bit or U32 on 32-bit
        assert!(matches!(v, TlangValue::U64(42) | TlangValue::U32(42)));
    }

    // ── Display ───────────────────────────────────────────────────────────────

    #[test]
    fn test_display_nil() {
        assert_eq!(TlangValue::Nil.to_string(), "nil");
    }

    #[test]
    fn test_display_bool_true() {
        assert_eq!(TlangValue::Bool(true).to_string(), "true");
    }

    #[test]
    fn test_display_bool_false() {
        assert_eq!(TlangValue::Bool(false).to_string(), "false");
    }

    #[test]
    fn test_display_u8() {
        assert_eq!(TlangValue::U8(7).to_string(), "7");
    }

    #[test]
    fn test_display_u16() {
        assert_eq!(TlangValue::U16(256).to_string(), "256");
    }

    #[test]
    fn test_display_u32() {
        assert_eq!(TlangValue::U32(65536).to_string(), "65536");
    }

    #[test]
    fn test_display_u64() {
        assert_eq!(TlangValue::U64(100).to_string(), "100");
    }

    #[test]
    fn test_display_i8() {
        assert_eq!(TlangValue::I8(-1).to_string(), "-1");
    }

    #[test]
    fn test_display_i16() {
        assert_eq!(TlangValue::I16(-512).to_string(), "-512");
    }

    #[test]
    fn test_display_i32() {
        assert_eq!(TlangValue::I32(-1024).to_string(), "-1024");
    }

    #[test]
    fn test_display_i64() {
        assert_eq!(TlangValue::I64(-42).to_string(), "-42");
    }

    #[test]
    fn test_display_f32() {
        // f32 Display rounds; just verify it contains the number
        let s = TlangValue::F32(1.5).to_string();
        assert!(s.contains("1.5"));
    }

    #[test]
    #[allow(clippy::approx_constant)]
    fn test_display_f64() {
        assert_eq!(TlangValue::F64(3.14).to_string(), "3.14");
    }

    #[test]
    fn test_display_object() {
        let id = TlangObjectId::from(0usize);
        let s = TlangValue::Object(id).to_string();
        assert!(s.starts_with("Object("));
    }

    // ── Hash ──────────────────────────────────────────────────────────────────

    #[test]
    fn test_hash_nil_is_stable() {
        assert_eq!(hash_of(TlangValue::Nil), hash_of(TlangValue::Nil));
    }

    #[test]
    fn test_hash_bool() {
        assert_eq!(
            hash_of(TlangValue::Bool(true)),
            hash_of(TlangValue::Bool(true))
        );
        assert_ne!(
            hash_of(TlangValue::Bool(true)),
            hash_of(TlangValue::Bool(false))
        );
    }

    #[test]
    fn test_hash_integers_stable() {
        assert_eq!(hash_of(TlangValue::U64(42)), hash_of(TlangValue::U64(42)));
        assert_eq!(hash_of(TlangValue::I64(-1)), hash_of(TlangValue::I64(-1)));
    }

    #[test]
    fn test_hash_u8_u16_u32() {
        // Just verify these don't panic
        let _ = hash_of(TlangValue::U8(1));
        let _ = hash_of(TlangValue::U16(2));
        let _ = hash_of(TlangValue::U32(3));
    }

    #[test]
    fn test_hash_i8_i16_i32() {
        let _ = hash_of(TlangValue::I8(-1));
        let _ = hash_of(TlangValue::I16(-2));
        let _ = hash_of(TlangValue::I32(-3));
    }

    #[test]
    fn test_hash_f32_f64() {
        let _ = hash_of(TlangValue::F32(1.0));
        let _ = hash_of(TlangValue::F64(2.0));
    }

    #[test]
    fn test_hash_object() {
        let id = TlangObjectId::from(5usize);
        let _ = hash_of(TlangValue::Object(id));
    }
}
