use tlang_core::memory as tlang_memory;
use tlang_core::memory::{VMState, prelude::*};
use tlang_macros::native_fn;

#[native_fn]
pub fn random(_: &mut VMState) -> TlangValue {
    TlangValue::from(js_sys::Math::random())
}

#[native_fn]
pub fn random_int(_: &mut VMState, max: TlangValue) -> TlangValue {
    let random_float = js_sys::Math::random();
    let random_int = (random_float * max.as_f64()) as i64;
    TlangValue::from(random_int)
}
