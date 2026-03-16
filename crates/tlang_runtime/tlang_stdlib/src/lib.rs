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

/// Called once at startup to anchor this crate in the final binary.
///
/// A single reference to this symbol is enough: because `tlang_stdlib` compiles
/// with `codegen-units = 1` (see workspace `Cargo.toml`), the whole crate lands
/// in one object file.  Pulling in any symbol from it causes the linker to
/// include all `inventory::submit!` constructors from every submodule.
///
/// `#[inline(never)]` prevents the compiler from eliminating the call site,
/// which would remove the only reference that keeps this object file in the
/// WASM binary.
#[inline(never)]
pub fn init() {}
