extern crate console_error_panic_hook;

use wasm_bindgen::prelude::*;

pub use tlang_runtime::memory::NativeFnDef;

pub mod codemirror;
pub mod interpreter;
mod stdlib;
pub mod tlang;

unsafe extern "C" {
    fn __wasm_call_ctors();
}

/// # Panics
#[wasm_bindgen(start)]
pub fn init() {
    unsafe { __wasm_call_ctors() };

    console_error_panic_hook::set_once();
    console_log::init_with_level(log::Level::Debug).expect("error initializing log");
}
