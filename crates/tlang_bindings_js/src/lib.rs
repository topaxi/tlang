extern crate console_error_panic_hook;
extern crate tlang_interpreter;

use wasm_bindgen::prelude::*;

pub use tlang_interpreter::NativeFn;

pub mod codemirror;
pub mod compiler;
pub mod interpreter;
mod stdlib;

unsafe extern "C" {
    fn __wasm_call_ctors();
}

#[wasm_bindgen(start)]
pub fn main() {
    unsafe { __wasm_call_ctors() };
}
