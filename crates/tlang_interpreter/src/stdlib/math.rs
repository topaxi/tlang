use tlang_macros::native_fn;

use crate::state::InterpreterState;
use crate::value::TlangValue;

#[native_fn]
pub fn floor(_: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    match &args[0] {
        TlangValue::Float(f) => TlangValue::Float(f.floor()),
        TlangValue::Int(i) => TlangValue::Int(*i),
        value => panic!("Expected float or int, got {:?}", value),
    }
}

#[native_fn]
pub fn sqrt(_: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    match &args[0] {
        TlangValue::Float(f) => TlangValue::Float(f.sqrt()),
        TlangValue::Int(i) => TlangValue::Float(((*i) as f64).sqrt()),
        value => panic!("Expected float or int, got {:?}", value),
    }
}

#[native_fn]
pub fn max(_: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    match (args[0], args[1]) {
        (TlangValue::Float(f1), TlangValue::Float(f2)) => TlangValue::Float(f1.max(f2)),
        (TlangValue::Int(i1), TlangValue::Int(i2)) => TlangValue::Int(i1.max(i2)),
        (TlangValue::Float(f), TlangValue::Int(i)) => TlangValue::Float(f.max(i as f64)),
        (TlangValue::Int(i), TlangValue::Float(f)) => TlangValue::Float((i as f64).max(f)),
        (value1, value2) => panic!("Expected float or int, got {:?} and {:?}", value1, value2),
    }
}

#[cfg(feature = "rand")]
#[native_fn]
pub fn random(_: &mut InterpreterState, _: &[TlangValue]) -> TlangValue {
    TlangValue::Float(rand::random())
}

#[cfg(feature = "rand")]
#[native_fn]
pub fn random_int(_: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    match &args[0] {
        TlangValue::Int(i) => TlangValue::Int(rand::random::<i64>() % i),
        value => panic!("Expected int, got {:?}", value),
    }
}
