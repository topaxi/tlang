use tlang_macros::native_fn;
use tlang_memory::{InterpreterState, TlangValue};

fn stringify(state: &mut InterpreterState, values: &[TlangValue]) -> String {
    values
        .iter()
        .map(|v| state.stringify(*v))
        .collect::<Vec<_>>()
        .join(" ")
}

#[native_fn]
pub fn log(state: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    println!("{}", stringify(state, args));
    TlangValue::Nil
}

#[native_fn]
pub fn panic(state: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    let stringified = stringify(state, args);
    state.panic(format!("Panic!: {stringified}"));
}
