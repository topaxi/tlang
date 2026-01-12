use tlang_macros::native_fn;
use tlang_runtime::memory::{InterpreterState, prelude::*};
use tlang_runtime::memory as tlang_memory;

#[native_fn]
pub fn random(_: &mut InterpreterState) -> TlangValue {
    TlangValue::from(js_sys::Math::random())
}

#[native_fn]
pub fn random_int(_: &mut InterpreterState, max: TlangValue) -> TlangValue {
    let random_float = js_sys::Math::random();
    let random_int = (random_float * max.as_f64()) as i64;
    TlangValue::from(random_int)
}
