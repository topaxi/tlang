//! Octarine runtime library for tlang.
//!
//! Provides native stream types, I/O operations, and an event-loop-based
//! runtime. The native types and functions are automatically registered via
//! the `inventory` crate so that the tlang interpreter discovers them at
//! startup.

pub use tlang_memory::{
    NativeEnumDef, NativeEnumVariantDef, NativeFnDef, NativeMethodDef, NativeProtocolDef,
    NativeProtocolImplDef, NativeStructDef,
};

pub mod event_loop;
pub mod io_worker;
pub mod runtime;
pub mod stdlib;
pub mod stream;

/// Called once at startup to anchor this crate in the final binary.
///
/// A single reference to this symbol is enough: because `tlang_octarine`
/// compiles with `codegen-units = 1`, the whole crate lands in one object
/// file. Pulling in any symbol from it causes the linker to include all
/// `inventory::submit!` constructors from every submodule.
///
/// `#[inline(never)]` prevents the compiler from eliminating the call site.
#[inline(never)]
pub fn init() {}
