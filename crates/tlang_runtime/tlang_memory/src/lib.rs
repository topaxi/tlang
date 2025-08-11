mod macros;
pub mod resolver;
pub mod scope;
pub mod shape;
pub mod state;
pub mod value;

pub use resolver::Resolver;
pub use state::InterpreterState;
pub use value::{TlangValue, object::TlangObjectKind};

pub mod prelude {
    pub use crate::value::{
        TlangPrimitive, TlangValue,
        object::{TlangNativeFn, TlangObjectId, TlangObjectKind, TlangSlice, TlangStruct},
    };

    pub use crate::shape::{
        ShapeKey as TlangShapeKey, Shaped as TlangShaped, TlangStructMethod, TlangStructShape,
    };
}

pub struct NativeFnDef {
    pub name: &'static str,
    pub binding_name: &'static str,
    pub arity: usize,
    pub function: fn(&mut InterpreterState, &[TlangValue]) -> TlangValue,
    pub module_path: &'static str,
}

inventory::collect!(NativeFnDef);
