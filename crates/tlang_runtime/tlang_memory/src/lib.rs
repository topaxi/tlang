mod macros;
pub mod allocator;
pub mod resolver;
pub mod scope;
pub mod shape;
pub mod state;
pub mod value;

pub use resolver::Resolver;
pub use state::InterpreterState;
pub use value::{TlangObjectKind, TlangValue};

pub mod prelude {
    pub use crate::value::{
        TlangNativeFn, TlangObjectId, TlangObjectKind, TlangPrimitive, TlangSlice, TlangStruct,
        TlangValue,
    };

    pub use crate::shape::{ShapeKey as TlangShapeKey, TlangStructMethod, TlangStructShape};
}

pub struct NativeFnDef {
    pub name: &'static str,
    pub binding_name: &'static str,
    pub function: fn(&mut InterpreterState, &[TlangValue]) -> TlangValue,
    pub module_path: &'static str,
}

inventory::collect!(NativeFnDef);
