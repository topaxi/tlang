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
///
/// Must be **called** (not just referenced as a pointer) so that optimizers
/// cannot elide it, particularly in WASM release builds.
pub fn force_link() {
    // `black_box` prevents the optimizer from treating these as dead code.
    // Each call keeps one symbol from a module alive, which in turn keeps
    // the entire object file (and its inventory::submit! ctors) in the binary.
    std::hint::black_box(option::new_option_some as *const () as usize);
    std::hint::black_box(result::new_result_ok as *const () as usize);
    std::hint::black_box(collections::len as *const () as usize);
    std::hint::black_box(globals::log as *const () as usize);
    std::hint::black_box(math::floor as *const () as usize);
    std::hint::black_box(protocols::map as *const () as usize);
    std::hint::black_box(regex::regex_match as *const () as usize);
    std::hint::black_box(string::from_char_code as *const () as usize);
}
