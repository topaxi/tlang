use tlang_macros::{define_enum, native_fn, native_method, protocol_impl};
use tlang_memory::{TlangValue, VMState};

define_enum! {
    enum Result {
        Ok(value),
        Err(error),
    }
}

// Re-export under the old names for backward compatibility.
pub const RESULT_VARIANT_OK: usize = RESULT_OK;
pub const RESULT_VARIANT_ERR: usize = RESULT_ERR;

#[native_fn(name = "Result::Ok")]
pub fn new_result_ok(state: &mut VMState, value: TlangValue) -> TlangValue {
    state.new_enum(state.heap.builtin_shapes.result, RESULT_OK, vec![value])
}

#[native_fn(name = "Result::Err")]
pub fn new_result_err(state: &mut VMState, err: TlangValue) -> TlangValue {
    state.new_enum(state.heap.builtin_shapes.result, RESULT_ERR, vec![err])
}

#[native_method("Result")]
fn is_ok(vm: &mut VMState, this: TlangValue) -> TlangValue {
    TlangValue::from(vm.get_enum(this).unwrap().variant == RESULT_OK)
}

#[native_method("Result")]
fn is_err(vm: &mut VMState, this: TlangValue) -> TlangValue {
    TlangValue::from(vm.get_enum(this).unwrap().variant == RESULT_ERR)
}

#[native_method("Result")]
fn map(vm: &mut VMState, this: TlangValue, func: TlangValue) -> TlangValue {
    let e = vm.get_enum(this).unwrap();

    if e.variant == RESULT_ERR {
        let err_val = e.field_values[0];
        return vm.new_enum(vm.heap.builtin_shapes.result, RESULT_ERR, vec![err_val]);
    }

    let inner = e.field_values[0];
    let mapped = vm.call(func, &[inner]);
    vm.new_enum(vm.heap.builtin_shapes.result, RESULT_OK, vec![mapped])
}

#[native_method("Result")]
fn unwrap(vm: &mut VMState, this: TlangValue) -> TlangValue {
    let e = vm.get_enum(this).unwrap();

    if e.variant == RESULT_ERR {
        vm.panic("Called unwrap on Err".to_string());
    }

    e.field_values[0]
}

#[protocol_impl("Truthy", "Result")]
fn truthy(vm: &mut VMState, this: TlangValue) -> TlangValue {
    TlangValue::Bool(vm.get_enum(this).unwrap().variant == RESULT_OK)
}

#[protocol_impl("Functor", "Result", method = "map")]
fn functor_map(vm: &mut VMState, this: TlangValue, func: TlangValue) -> TlangValue {
    let e = vm.get_enum(this).unwrap();

    if e.variant == RESULT_ERR {
        let err_val = e.field_values[0];
        return vm.new_enum(vm.heap.builtin_shapes.result, RESULT_ERR, vec![err_val]);
    }

    let inner = e.field_values[0];
    let mapped = vm.call(func, &[inner]);
    vm.new_enum(vm.heap.builtin_shapes.result, RESULT_OK, vec![mapped])
}

/// Legacy entry point — now a no-op since shapes and methods are registered
/// via inventory.
#[deprecated(note = "Use VMState::collect_native_inventory() instead")]
pub fn define_result_shape(_state: &mut VMState) {}

#[cfg(test)]
mod tests {
    use tlang_memory::{TlangValue, VMState};

    use crate::result::{RESULT_VARIANT_ERR, RESULT_VARIANT_OK};

    fn vm_state() -> VMState {
        let mut state = VMState::new();
        state.collect_native_inventory();
        state
    }

    #[test]
    fn test_enum_truthiness_result_ok() {
        let mut state = vm_state();
        let result_shape = state.heap.builtin_shapes.result;

        // Result::Ok(truthy value) should be truthy
        let obj = state.new_enum(result_shape, RESULT_VARIANT_OK, vec![TlangValue::I64(42)]);
        assert!(state.is_truthy(obj), "Result::Ok(42) should be truthy");

        // Result::Ok(falsy value) should be truthy too
        let obj = state.new_enum(result_shape, RESULT_VARIANT_OK, vec![TlangValue::I64(0)]);
        assert!(state.is_truthy(obj), "Result::Ok(0) should be truthy");
    }

    #[test]
    fn test_enum_truthiness_result_err() {
        let mut state = vm_state();
        let result_shape = state.heap.builtin_shapes.result;

        // Result::Err should be falsy (variant 1 = Err)
        let obj = state.new_enum(result_shape, RESULT_VARIANT_ERR, vec![TlangValue::I64(42)]);
        assert!(!state.is_truthy(obj), "Result::Err should be falsy");
    }
}
