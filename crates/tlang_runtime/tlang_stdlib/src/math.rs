use tlang_macros::native_fn;
use tlang_memory::{VMState, prelude::*};

#[native_fn]
pub fn floor(state: &mut VMState, value: TlangValue) -> TlangValue {
    match value.as_primitive() {
        TlangPrimitive::Float(f) => TlangValue::from(f.floor()),
        TlangPrimitive::Int(_) | TlangPrimitive::UInt(_) => value,
        value => state.panic(format!("Expected float or int, got {value:?}")),
    }
}

#[native_fn]
pub fn sqrt(_: &mut VMState, value: TlangValue) -> TlangValue {
    TlangValue::from(value.as_f64().sqrt())
}

#[native_fn]
pub fn max(state: &mut VMState, lhs: TlangValue, rhs: TlangValue) -> TlangValue {
    match (lhs.as_primitive(), rhs.as_primitive()) {
        (TlangPrimitive::UInt(i1), TlangPrimitive::UInt(i2)) => TlangValue::from(i1.max(i2)),
        (TlangPrimitive::Int(i1), TlangPrimitive::Int(i2)) => TlangValue::from(i1.max(i2)),
        (TlangPrimitive::Float(f1), TlangPrimitive::Float(f2)) => TlangValue::from(f1.max(f2)),
        (TlangPrimitive::UInt(i), TlangPrimitive::Float(f)) => TlangValue::from((i as f64).max(f)),
        (TlangPrimitive::Int(i), TlangPrimitive::Float(f)) => TlangValue::from((i as f64).max(f)),
        (TlangPrimitive::Float(f), TlangPrimitive::UInt(i)) => TlangValue::from(f.max(i as f64)),
        (TlangPrimitive::Float(f), TlangPrimitive::Int(i)) => TlangValue::from(f.max(i as f64)),
        (value1, value2) => state.panic(format!(
            "Expected float or int, got {value1:?} and {value2:?}"
        )),
    }
}

#[cfg(not(target_family = "wasm"))]
#[native_fn]
pub fn random(_: &mut VMState) -> TlangValue {
    TlangValue::from(rand::random::<f64>())
}

#[cfg(not(target_family = "wasm"))]
#[native_fn]
pub fn random_int(state: &mut VMState, max: TlangValue) -> TlangValue {
    match max.as_primitive() {
        TlangPrimitive::UInt(i) => TlangValue::from(rand::random::<u64>() % i),
        TlangPrimitive::Int(i) => TlangValue::from(rand::random::<i64>() % i),
        value => state.panic(format!("Expected int, got {value:?}")),
    }
}

#[cfg(test)]
mod tests {
    use tlang_memory::{TlangValue, VMState};

    use super::{floor, max, sqrt};

    #[test]
    fn test_floor_float() {
        let mut state = VMState::new();
        let result = floor(&mut state, TlangValue::F64(3.7));
        assert!(matches!(result, TlangValue::F64(f) if (f - 3.0).abs() < 1e-10));
    }

    #[test]
    fn test_floor_negative_float() {
        let mut state = VMState::new();
        let result = floor(&mut state, TlangValue::F64(-2.3));
        assert!(matches!(result, TlangValue::F64(f) if (f - (-3.0)).abs() < 1e-10));
    }

    #[test]
    fn test_floor_integer_passthrough() {
        let mut state = VMState::new();
        let result = floor(&mut state, TlangValue::I64(5));
        assert_eq!(result, TlangValue::I64(5));
    }

    #[test]
    fn test_sqrt_perfect_square() {
        let mut state = VMState::new();
        let result = sqrt(&mut state, TlangValue::F64(16.0));
        assert!(matches!(result, TlangValue::F64(f) if (f - 4.0).abs() < 1e-10));
    }

    #[test]
    fn test_sqrt_zero() {
        let mut state = VMState::new();
        let result = sqrt(&mut state, TlangValue::F64(0.0));
        assert!(matches!(result, TlangValue::F64(f) if f.abs() < 1e-10));
    }

    #[test]
    fn test_max_int_first_larger() {
        let mut state = VMState::new();
        let result = max(&mut state, TlangValue::I64(10), TlangValue::I64(5));
        assert_eq!(result, TlangValue::I64(10));
    }

    #[test]
    fn test_max_int_second_larger() {
        let mut state = VMState::new();
        let result = max(&mut state, TlangValue::I64(3), TlangValue::I64(7));
        assert_eq!(result, TlangValue::I64(7));
    }

    #[test]
    fn test_max_float() {
        let mut state = VMState::new();
        let result = max(&mut state, TlangValue::F64(1.5), TlangValue::F64(2.5));
        assert!(matches!(result, TlangValue::F64(f) if (f - 2.5).abs() < 1e-10));
    }

    #[test]
    fn test_max_int_and_float() {
        let mut state = VMState::new();
        let result = max(&mut state, TlangValue::I64(3), TlangValue::F64(3.5));
        assert!(matches!(result, TlangValue::F64(f) if (f - 3.5).abs() < 1e-10));
    }

    #[test]
    fn test_max_float_and_int() {
        let mut state = VMState::new();
        let result = max(&mut state, TlangValue::F64(4.0), TlangValue::I64(2));
        assert!(matches!(result, TlangValue::F64(f) if (f - 4.0).abs() < 1e-10));
    }
}
