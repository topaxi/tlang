use tlang_macros::native_fn;

use crate::state::InterpreterState;
use crate::value::{TlangPrimitive, TlangValue};

#[native_fn]
pub fn floor(state: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    let value = args[0];

    match value.as_primitive() {
        TlangPrimitive::Float(f) => TlangValue::from(f.floor()),
        TlangPrimitive::Int(_) | TlangPrimitive::UInt(_) => value,
        value => state.panic(format!("Expected float or int, got {:?}", value)),
    }
}

#[native_fn]
pub fn sqrt(_: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    TlangValue::from(args[0].as_f64().sqrt())
}

#[native_fn]
pub fn max(state: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    match (args[0].as_primitive(), args[1].as_primitive()) {
        (TlangPrimitive::UInt(i1), TlangPrimitive::UInt(i2)) => TlangValue::from(i1.max(i2)),
        (TlangPrimitive::Int(i1), TlangPrimitive::Int(i2)) => TlangValue::from(i1.max(i2)),
        (TlangPrimitive::Float(f1), TlangPrimitive::Float(f2)) => TlangValue::from(f1.max(f2)),
        (TlangPrimitive::UInt(i), TlangPrimitive::Float(f)) => TlangValue::from((i as f64).max(f)),
        (TlangPrimitive::Int(i), TlangPrimitive::Float(f)) => TlangValue::from((i as f64).max(f)),
        (TlangPrimitive::Float(f), TlangPrimitive::UInt(i)) => TlangValue::from(f.max(i as f64)),
        (TlangPrimitive::Float(f), TlangPrimitive::Int(i)) => TlangValue::from(f.max(i as f64)),
        (value1, value2) => state.panic(format!(
            "Expected float or int, got {:?} and {:?}",
            value1, value2
        )),
    }
}

#[cfg(not(target_family = "wasm"))]
#[native_fn]
pub fn random(_: &mut InterpreterState, _: &[TlangValue]) -> TlangValue {
    TlangValue::from(rand::random::<f64>())
}

#[cfg(not(target_family = "wasm"))]
#[native_fn]
pub fn random_int(state: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    match args[0].as_primitive() {
        TlangPrimitive::UInt(i) => TlangValue::from(rand::random::<u64>() % i),
        TlangPrimitive::Int(i) => TlangValue::from(rand::random::<i64>() % i),
        value => state.panic(format!("Expected int, got {:?}", value)),
    }
}
