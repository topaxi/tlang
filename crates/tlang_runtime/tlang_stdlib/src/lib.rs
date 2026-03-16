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
