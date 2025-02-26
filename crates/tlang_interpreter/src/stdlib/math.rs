use tlang_macros::native_fn;

use crate::state::InterpreterState;
use crate::value::{TlangPrimitive, TlangValue};

#[native_fn]
pub fn floor(_: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    match args[0].as_primitive() {
        TlangPrimitive::Float(f) => TlangValue::F64(f.floor()),
        TlangPrimitive::Int(i) => TlangValue::I64(i),
        TlangPrimitive::UInt(i) => TlangValue::U64(i),
        value => panic!("Expected float or int, got {:?}", value),
    }
}

#[native_fn]
pub fn sqrt(_: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    match args[0].as_primitive() {
        TlangPrimitive::Float(f) => TlangValue::F64(f.sqrt()),
        TlangPrimitive::Int(i) => TlangValue::F64(((i) as f64).sqrt()),
        TlangPrimitive::UInt(i) => TlangValue::F64(((i) as f64).sqrt()),
        value => panic!("Expected float or int, got {:?}", value),
    }
}

#[native_fn]
pub fn max(_: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    match (args[0].as_primitive(), args[1].as_primitive()) {
        (TlangPrimitive::UInt(i1), TlangPrimitive::UInt(i2)) => TlangValue::U64(i1.max(i2)),
        (TlangPrimitive::Int(i1), TlangPrimitive::Int(i2)) => TlangValue::I64(i1.max(i2)),
        (TlangPrimitive::Float(f1), TlangPrimitive::Float(f2)) => TlangValue::F64(f1.max(f2)),
        (TlangPrimitive::UInt(i), TlangPrimitive::Float(f)) => TlangValue::F64((i as f64).max(f)),
        (TlangPrimitive::Int(i), TlangPrimitive::Float(f)) => TlangValue::F64((i as f64).max(f)),
        (TlangPrimitive::Float(f), TlangPrimitive::UInt(i)) => TlangValue::F64(f.max(i as f64)),
        (TlangPrimitive::Float(f), TlangPrimitive::Int(i)) => TlangValue::F64(f.max(i as f64)),
        (value1, value2) => panic!("Expected float or int, got {:?} and {:?}", value1, value2),
    }
}

#[cfg(feature = "rand")]
#[native_fn]
pub fn random(_: &mut InterpreterState, _: &[TlangValue]) -> TlangValue {
    TlangValue::F64(rand::random())
}

#[cfg(feature = "rand")]
#[native_fn]
pub fn random_int(_: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    match args[0].as_primitive() {
        TlangPrimitive::UInt(i) => TlangValue::U64(rand::random::<u64>() % i),
        TlangPrimitive::Int(i) => TlangValue::I64(rand::random::<i64>() % i),
        value => panic!("Expected int, got {:?}", value),
    }
}
