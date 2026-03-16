use tlang_macros::{define_enum, native_fn, native_method, protocol_impl};
use tlang_memory::{TlangValue, VMState};

define_enum! {
    enum Option {
        Some(value),
        None,
    }
}

// Re-export under the old names for backward compatibility.
pub const OPTION_VARIANT_SOME: usize = OPTION_SOME;
pub const OPTION_VARIANT_NONE: usize = OPTION_NONE;

#[native_fn(name = "Option::Some")]
pub fn new_option_some(state: &mut VMState, value: TlangValue) -> TlangValue {
    state.new_enum(state.heap.builtin_shapes.option, OPTION_SOME, vec![value])
}

#[native_method("Option")]
fn is_some(vm: &mut VMState, this: TlangValue) -> TlangValue {
    TlangValue::from(vm.get_enum(this).unwrap().variant == OPTION_SOME)
}

#[native_method("Option")]
fn is_none(vm: &mut VMState, this: TlangValue) -> TlangValue {
    TlangValue::from(vm.get_enum(this).unwrap().variant == OPTION_NONE)
}

#[native_method("Option")]
fn unwrap(vm: &mut VMState, this: TlangValue) -> TlangValue {
    let e = vm.get_enum(this).unwrap();

    if e.variant == OPTION_NONE {
        vm.panic("Called unwrap on None".to_string());
    }

    e.field_values[0]
}

#[native_method("Option")]
fn map(vm: &mut VMState, this: TlangValue, func: TlangValue) -> TlangValue {
    let e = vm.get_enum(this).unwrap();

    if e.variant == OPTION_NONE {
        return vm.new_enum(vm.heap.builtin_shapes.option, OPTION_NONE, vec![]);
    }

    let inner = e.field_values[0];
    let mapped = vm.call(func, &[inner]);
    vm.new_enum(vm.heap.builtin_shapes.option, OPTION_SOME, vec![mapped])
}

#[protocol_impl("Truthy", "Option")]
fn truthy(vm: &mut VMState, this: TlangValue) -> TlangValue {
    TlangValue::Bool(vm.get_enum(this).unwrap().variant == OPTION_SOME)
}

#[protocol_impl("Functor", "Option", method = "map")]
fn functor_map(vm: &mut VMState, this: TlangValue, func: TlangValue) -> TlangValue {
    let e = vm.get_enum(this).unwrap();

    if e.variant == OPTION_NONE {
        return vm.new_enum(vm.heap.builtin_shapes.option, OPTION_NONE, vec![]);
    }

    let inner = e.field_values[0];
    let mapped = vm.call(func, &[inner]);
    vm.new_enum(vm.heap.builtin_shapes.option, OPTION_SOME, vec![mapped])
}

/// Legacy entry point — now a no-op since shapes, methods, and globals
/// (including `Option::None`) are registered via inventory.
#[deprecated(note = "Use VMState::collect_native_inventory() instead")]
pub fn define_option_shape(_state: &mut VMState) {}

#[cfg(test)]
mod tests {
    use tlang_memory::{TlangValue, VMState};

    use crate::option::{OPTION_VARIANT_NONE, OPTION_VARIANT_SOME};

    fn vm_state() -> VMState {
        let mut state = VMState::new();
        state.collect_native_inventory();
        state
    }

    #[test]
    fn test_enum_truthiness_option_some() {
        let mut state = vm_state();
        let option_shape = state.heap.builtin_shapes.option;

        // Option::Some(truthy value) should be truthy
        let obj = state.new_enum(option_shape, OPTION_VARIANT_SOME, vec![TlangValue::I64(42)]);
        assert!(state.is_truthy(obj), "Option::Some(42) should be truthy");

        // Option::Some(falsy value) should be truthy too
        let obj = state.new_enum(option_shape, OPTION_VARIANT_SOME, vec![TlangValue::I64(0)]);
        assert!(state.is_truthy(obj), "Option::Some(0) should be truthy");
    }

    #[test]
    fn test_enum_truthiness_option_none() {
        let mut state = vm_state();
        let option_shape = state.heap.builtin_shapes.option;

        // Option::None should be falsy (variant 1 = None)
        let obj = state.new_enum(option_shape, OPTION_VARIANT_NONE, vec![]);
        assert!(!state.is_truthy(obj), "Option::None should be falsy");
    }
}
