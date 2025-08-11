mod macros;
pub mod resolver;
pub mod scope;
pub mod shape;
pub mod state;
pub mod value;

pub use resolver::Resolver;
pub use state::InterpreterState;
pub use value::{TlangValue, object::TlangObjectKind};

pub use self::value::function::NativeFnReturn;

pub mod prelude {
    pub use crate::value::{
        TlangPrimitive, TlangValue,
        function::TlangNativeFn,
        object::{TlangObjectId, TlangObjectKind, TlangSlice, TlangStruct},
    };

    pub use crate::shape::{
        ShapeKey as TlangShapeKey, Shaped as TlangShaped, TlangStructMethod, TlangStructShape,
    };
}

pub struct NativeFnDef {
    name: &'static str,
    binding_name: &'static str,
    arity: usize,
    function: fn(&mut InterpreterState, &[TlangValue]) -> NativeFnReturn,
    module_path: &'static str,
}

impl NativeFnDef {
    pub const fn new(
        name: &'static str,
        binding_name: &'static str,
        arity: usize,
        function: fn(&mut InterpreterState, &[TlangValue]) -> NativeFnReturn,
        module_path: &'static str,
    ) -> Self {
        Self {
            name,
            binding_name,
            arity,
            function,
            module_path,
        }
    }

    pub fn name(&self) -> String {
        if self.binding_name.is_empty() {
            let module_name = self.module_path.split("::").last().unwrap_or_default();

            if module_name == "globals" {
                self.name.to_string()
            } else {
                module_name.to_string() + "::" + self.name
            }
        } else {
            self.binding_name.to_string()
        }
    }

    pub const fn arity(&self) -> usize {
        self.arity
    }

    pub const fn fn_ptr(&self) -> fn(&mut InterpreterState, &[TlangValue]) -> NativeFnReturn {
        self.function
    }
}

inventory::collect!(NativeFnDef);
