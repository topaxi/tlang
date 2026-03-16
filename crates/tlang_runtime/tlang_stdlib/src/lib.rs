pub use tlang_memory::NativeFnDef;
pub use tlang_memory::{
    NativeEnumDef, NativeEnumVariantDef, NativeMethodDef, NativeProtocolDef, NativeProtocolImplDef,
    NativeStructDef,
};

pub mod collections;
pub mod globals;
pub mod math;
pub mod option;
pub mod protocols;
pub mod regex;
pub mod result;
pub mod string;

/// Ensures all stdlib module object files are linked when tlang_stdlib is
/// used as a static library (rlib). Without at least one direct symbol
/// reference per module, the linker omits those object files and their
/// `inventory::submit!` items are never collected.
pub fn force_link() {
    let _ = option::new_option_some as *const () as usize;
    let _ = result::new_result_ok as *const () as usize;
    let _ = collections::len as *const () as usize;
    let _ = protocols::map as *const () as usize;
    let _ = regex::REGEX_FIELD_SOURCE;
}
