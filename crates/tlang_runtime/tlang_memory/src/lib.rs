mod macros;
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
