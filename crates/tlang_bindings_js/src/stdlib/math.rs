use tlang_interpreter::state::InterpreterState;
use tlang_interpreter::value::TlangValue;
use tlang_macros::native_fn;

#[native_fn]
pub fn random(_: &mut InterpreterState, _: &[TlangValue]) -> TlangValue {
    TlangValue::from(js_sys::Math::random())
}

#[native_fn]
pub fn random_int(_: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    let random_float = js_sys::Math::random();
    let random_int = (random_float * args[0].as_f64()) as i64;
    TlangValue::from(random_int)
}
