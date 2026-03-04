use tlang_macros::native_fn;
use tlang_memory;
use tlang_memory::{InterpreterState, prelude::*};

#[native_fn]
pub fn from_char_code(state: &mut InterpreterState, code: TlangValue) -> TlangValue {
    let char_code = code.as_usize() as u32;
    if let Some(ch) = std::char::from_u32(char_code) {
        state.new_string(ch.to_string())
    } else {
        state.panic(format!("Invalid char code: {}", char_code))
    }
}
