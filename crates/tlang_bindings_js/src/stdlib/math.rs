use tlang_interpreter::state::InterpreterState;
use tlang_interpreter::value::TlangValue;
use tlang_macros::native_fn;

#[native_fn]
pub fn random(_: &mut InterpreterState, _: &[TlangValue]) -> TlangValue {
    TlangValue::Float(js_sys::Math::random())
}

#[native_fn]
pub fn random_int(_: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    match &args[0] {
        TlangValue::Int(i) => {
            let random_float = js_sys::Math::random();
            let random_int = (random_float * *i as f64) as i64;
            TlangValue::Int(random_int)
        }
        value => panic!("Expected int, got {:?}", value),
    }
}
