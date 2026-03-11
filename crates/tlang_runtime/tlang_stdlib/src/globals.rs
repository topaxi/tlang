use tlang_macros::native_fn;
use tlang_memory::{TlangValue, VMState};

fn stringify(state: &mut VMState, values: &[TlangValue]) -> String {
    values
        .iter()
        .map(|v| state.stringify(*v))
        .collect::<Vec<_>>()
        .join(" ")
}

#[native_fn]
pub fn log(state: &mut VMState, args: &[TlangValue]) -> TlangValue {
    println!("{}", stringify(state, args));
    TlangValue::Nil
}

#[native_fn]
pub fn panic(state: &mut VMState, args: &[TlangValue]) -> TlangValue {
    let stringified = stringify(state, args);
    state.panic(format!("Panic!: {stringified}"));
}
