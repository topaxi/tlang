use std::collections::HashSet;

use log::debug;
use tlang_hir as hir;
use tlang_interpreter::Interpreter;
use tlang_memory::prelude::*;
use tlang_memory::{NativeFnDef, NativeProtocolDef, VMState};
use tlang_symbols::SymbolType;

pub struct VM {
    state: VMState,
}

impl Default for VM {
    fn default() -> Self {
        VM::new()
    }
}

impl VM {
    fn builtin_module_symbols() -> Vec<(&'static str, SymbolType)> {
        let mut module_names = HashSet::new();

        inventory::iter::<NativeFnDef>
            .into_iter()
            .map(|def| def.module())
            .filter(|module_name| module_names.insert(module_name.to_string()))
            .map(|module_name| (module_name, SymbolType::Module))
            .collect()
    }

    const fn builtin_static_symbols() -> &'static [(&'static str, SymbolType)] {
        &[
            ("Option", SymbolType::Enum),
            ("Option::None", SymbolType::EnumVariant(0)),
            ("Result", SymbolType::Enum),
            ("Regex", SymbolType::Struct),
            ("math::pi", SymbolType::Variable),
        ]
    }

    fn builtin_protocol_symbols() -> Vec<(String, SymbolType)> {
        let mut symbols = Vec::new();

        for def in inventory::iter::<NativeProtocolDef> {
            symbols.push((def.name().to_string(), SymbolType::Protocol));
            for &(method, arity) in def.methods() {
                symbols.push((
                    format!("{}::{}", def.name(), method),
                    SymbolType::ProtocolMethod(arity),
                ));
            }
        }

        symbols
    }

    pub fn builtin_symbols() -> Vec<(String, SymbolType, Option<usize>)> {
        let mut slot_counter = 0usize;

        // Module symbols: no slot (never looked up as values)
        let module_syms: Vec<(String, SymbolType, Option<usize>)> = Self::builtin_module_symbols()
            .into_iter()
            .map(|(name, ty)| (name.to_string(), ty, None))
            .collect();

        // Native function symbols: sorted by name for determinism, each gets a slot
        let mut fn_defs: Vec<&NativeFnDef> = inventory::iter::<NativeFnDef>.into_iter().collect();
        fn_defs.sort_by_key(|def| def.name());
        let fn_syms: Vec<(String, SymbolType, Option<usize>)> = fn_defs
            .iter()
            .map(|def| {
                let slot = slot_counter;
                slot_counter += 1;
                (
                    def.name(),
                    SymbolType::Function(def.arity() as u16),
                    Some(slot),
                )
            })
            .collect();

        // Const symbols: value-producing ones each get a slot
        let const_syms: Vec<(String, SymbolType, Option<usize>)> = Self::builtin_static_symbols()
            .iter()
            .map(|(name, ty)| {
                let slot = match ty {
                    SymbolType::Enum | SymbolType::Protocol | SymbolType::ProtocolMethod(_) => None,
                    _ => {
                        let s = Some(slot_counter);
                        slot_counter += 1;
                        s
                    }
                };
                (name.to_string(), *ty, slot)
            })
            .collect();

        // Protocol symbols: derived from inventory (no slots)
        let protocol_syms: Vec<(String, SymbolType, Option<usize>)> =
            Self::builtin_protocol_symbols()
                .into_iter()
                .map(|(name, ty)| (name, ty, None))
                .collect();

        module_syms
            .into_iter()
            .chain(fn_syms)
            .chain(const_syms)
            .chain(protocol_syms)
            .collect()
    }

    /// # Panics
    pub fn new() -> Self {
        let mut state = VMState::default();

        // Register the interpreter's call handler so native functions can
        // invoke user-defined callables via `VMState::call()`.
        state.register_call_fn(Interpreter::call_handler);

        // Build stable slot assignments and initialize the global slots Vec.
        let builtin_defs = Self::builtin_symbols();
        state.init_global_slots(
            builtin_defs
                .iter()
                .filter_map(|(name, _, slot)| slot.map(|i| (name.as_str(), i))),
        );

        Self::init_stdlib(&mut state);

        // Collect and register all inventory-submitted native definitions.
        state.collect_native_inventory();

        state.set_global(
            "math::pi".to_string(),
            TlangValue::F64(std::f64::consts::PI),
        );

        VM { state }
    }

    #[cfg(feature = "stdlib")]
    fn init_stdlib(state: &mut VMState) {
        tlang_interpreter::init_stdlib(state);
    }

    #[cfg(not(feature = "stdlib"))]
    fn init_stdlib(_state: &mut VMState) {}

    pub fn state(&self) -> &VMState {
        &self.state
    }

    pub fn state_mut(&mut self) -> &mut VMState {
        &mut self.state
    }

    pub fn into_state(self) -> VMState {
        self.state
    }

    pub fn eval(&mut self, module: &hir::Module) -> TlangValue {
        Interpreter.eval(&mut self.state, module)
    }

    pub fn eval_expr(&mut self, expr: &hir::Expr) -> tlang_interpreter::EvalResult {
        Interpreter.eval_expr(&mut self.state, expr)
    }

    pub fn define_native_fn<F>(&mut self, name: &str, f: F) -> TlangValue
    where
        F: Fn(&mut VMState, &[TlangValue]) -> tlang_memory::NativeFnReturn + 'static,
    {
        let fn_object = self.state.new_native_fn(name, f);
        debug!("Defining global native function: {name}");
        self.state.set_global(name.to_string(), fn_object);
        fn_object
    }
}
