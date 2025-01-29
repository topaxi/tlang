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
