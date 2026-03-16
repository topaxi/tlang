pub mod execution;
pub mod heap;
mod macros;
pub mod program;
pub mod resolver;
pub mod scope;
pub mod shape;
pub mod state;
pub mod value;

pub use execution::ExecutionContext;
pub use heap::{Heap, MemoryStats};
pub use program::Program;
pub use resolver::Resolver;
pub use state::VMState;
pub use value::{TlangValue, object::TlangObjectKind};

pub use self::value::function::NativeFnReturn;

pub mod prelude {
    pub use crate::value::{
        ReferencedValuesIter, TlangPrimitive, TlangValue,
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
    function: fn(&mut VMState, &[TlangValue]) -> NativeFnReturn,
    module_path: &'static str,
}

impl NativeFnDef {
    pub const fn new(
        name: &'static str,
        binding_name: &'static str,
        arity: usize,
        function: fn(&mut VMState, &[TlangValue]) -> NativeFnReturn,
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

    pub fn name(&'static self) -> String {
        if self.binding_name.is_empty() {
            let module_name = self.module();

            if module_name == "globals" {
                self.name.to_string()
            } else {
                module_name.to_string() + "::" + self.name
            }
        } else {
            self.binding_name.to_string()
        }
    }

    pub fn module(&'static self) -> &'static str {
        self.module_path.split("::").last().unwrap_or_default()
    }

    pub const fn arity(&'static self) -> usize {
        self.arity
    }

    pub const fn fn_ptr(&'static self) -> fn(&mut VMState, &[TlangValue]) -> NativeFnReturn {
        self.function
    }
}

inventory::collect!(NativeFnDef);

// ── Native protocol descriptor ─────────────────────────────────────────────

pub struct NativeProtocolDef {
    name: &'static str,
    methods: &'static [(&'static str, u16)], // (method_name, arity)
}

impl NativeProtocolDef {
    pub const fn new(name: &'static str, methods: &'static [(&'static str, u16)]) -> Self {
        Self { name, methods }
    }

    pub const fn name(&self) -> &'static str {
        self.name
    }

    pub const fn methods(&self) -> &'static [(&'static str, u16)] {
        self.methods
    }
}

inventory::collect!(NativeProtocolDef);

// ── Native method descriptor ───────────────────────────────────────────────

pub struct NativeMethodDef {
    type_name: &'static str,
    method_name: &'static str,
    function: fn(&mut VMState, &[TlangValue]) -> NativeFnReturn,
    priority: u8,
}

impl NativeMethodDef {
    pub const fn new(
        type_name: &'static str,
        method_name: &'static str,
        function: fn(&mut VMState, &[TlangValue]) -> NativeFnReturn,
        priority: u8,
    ) -> Self {
        Self {
            type_name,
            method_name,
            function,
            priority,
        }
    }

    pub const fn type_name(&self) -> &'static str {
        self.type_name
    }

    pub const fn method_name(&self) -> &'static str {
        self.method_name
    }

    pub const fn fn_ptr(&self) -> fn(&mut VMState, &[TlangValue]) -> NativeFnReturn {
        self.function
    }

    pub const fn priority(&self) -> u8 {
        self.priority
    }
}

inventory::collect!(NativeMethodDef);

// ── Native protocol-impl descriptor ────────────────────────────────────────

pub struct NativeProtocolImplDef {
    protocol: &'static str,
    type_name: &'static str,
    method: &'static str,
    function: fn(&mut VMState, &[TlangValue]) -> NativeFnReturn,
    priority: u8,
}

impl NativeProtocolImplDef {
    pub const fn new(
        protocol: &'static str,
        type_name: &'static str,
        method: &'static str,
        function: fn(&mut VMState, &[TlangValue]) -> NativeFnReturn,
        priority: u8,
    ) -> Self {
        Self {
            protocol,
            type_name,
            method,
            function,
            priority,
        }
    }

    pub const fn protocol(&self) -> &'static str {
        self.protocol
    }

    pub const fn type_name(&self) -> &'static str {
        self.type_name
    }

    pub const fn method(&self) -> &'static str {
        self.method
    }

    pub const fn fn_ptr(&self) -> fn(&mut VMState, &[TlangValue]) -> NativeFnReturn {
        self.function
    }

    pub const fn priority(&self) -> u8 {
        self.priority
    }
}

inventory::collect!(NativeProtocolImplDef);

// ── Native enum descriptor ─────────────────────────────────────────────────

pub struct NativeEnumVariantDef {
    pub name: &'static str,
    pub fields: &'static [&'static str],
}

pub struct NativeEnumDef {
    name: &'static str,
    variants: &'static [NativeEnumVariantDef],
}

impl NativeEnumDef {
    pub const fn new(name: &'static str, variants: &'static [NativeEnumVariantDef]) -> Self {
        Self { name, variants }
    }

    pub const fn name(&self) -> &'static str {
        self.name
    }

    pub const fn variants(&self) -> &'static [NativeEnumVariantDef] {
        self.variants
    }
}

inventory::collect!(NativeEnumDef);

// ── Native struct descriptor ───────────────────────────────────────────────

pub struct NativeStructDef {
    name: &'static str,
    fields: &'static [&'static str],
}

impl NativeStructDef {
    pub const fn new(name: &'static str, fields: &'static [&'static str]) -> Self {
        Self { name, fields }
    }

    pub const fn name(&self) -> &'static str {
        self.name
    }

    pub const fn fields(&self) -> &'static [&'static str] {
        self.fields
    }
}

inventory::collect!(NativeStructDef);
