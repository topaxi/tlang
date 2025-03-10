use tlang_memory::InterpreterState;
use tlang_memory::prelude::*;

pub struct NativeFn {
    pub name: &'static str,
    pub binding_name: &'static str,
    pub function: fn(&mut InterpreterState, &[TlangValue]) -> TlangValue,
    pub module_path: &'static str,
}

inventory::collect!(NativeFn);

pub mod collections;
pub mod math;
pub mod utils;
