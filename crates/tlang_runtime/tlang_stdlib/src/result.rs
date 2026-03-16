use tlang_macros::{define_enum, native_fn};
use tlang_memory::{TlangValue, VMState};

define_enum! {
    enum Result {
        Ok(value),
        Err(error),
    }

    impl Result {
        fn is_ok(this) {
            TlangValue::from(vm.get_enum(this).unwrap().variant == RESULT_OK)
        }

        fn is_err(this) {
            TlangValue::from(vm.get_enum(this).unwrap().variant == RESULT_ERR)
        }

        fn map(this, func) {
            let e = vm.get_enum(this).unwrap();
            if e.variant == RESULT_ERR {
                let err_val = e.field_values[0];
                return vm.new_enum(vm.heap.builtin_shapes.result, RESULT_ERR, vec![err_val]);
            }
            let inner = e.field_values[0];
            let mapped = vm.call(func, &[inner]);
            vm.new_enum(vm.heap.builtin_shapes.result, RESULT_OK, vec![mapped])
        }

        fn unwrap(this) {
            let e = vm.get_enum(this).unwrap();
            if e.variant == RESULT_ERR {
                vm.panic("Called unwrap on Err".to_string());
            }
            e.field_values[0]
        }
    }

    impl Truthy for Result {
        fn truthy(this) {
            TlangValue::Bool(vm.get_enum(this).unwrap().variant == RESULT_OK)
        }
    }

    impl Functor for Result {
        fn map(this, func) {
            let e = vm.get_enum(this).unwrap();
            if e.variant == RESULT_ERR {
                let err_val = e.field_values[0];
                return vm.new_enum(vm.heap.builtin_shapes.result, RESULT_ERR, vec![err_val]);
            }
            let inner = e.field_values[0];
            let mapped = vm.call(func, &[inner]);
            vm.new_enum(vm.heap.builtin_shapes.result, RESULT_OK, vec![mapped])
        }
    }
}

#[native_fn(name = "Result::Ok")]
pub fn new_result_ok(state: &mut VMState, value: TlangValue) -> TlangValue {
    state.new_enum(state.heap.builtin_shapes.result, RESULT_OK, vec![value])
}

#[native_fn(name = "Result::Err")]
pub fn new_result_err(state: &mut VMState, err: TlangValue) -> TlangValue {
    state.new_enum(state.heap.builtin_shapes.result, RESULT_ERR, vec![err])
}

#[cfg(test)]
mod tests {
    use tlang_memory::{TlangValue, VMState};

    use crate::result::{RESULT_ERR, RESULT_OK};

    fn vm_state() -> VMState {
        let mut state = VMState::new();
        state.collect_native_inventory();
        state
    }

    #[test]
    fn test_enum_truthiness_result_ok() {
        let mut state = vm_state();
        let result_shape = state.heap.builtin_shapes.result;

        let obj = state.new_enum(result_shape, RESULT_OK, vec![TlangValue::I64(42)]);
        assert!(state.is_truthy(obj), "Result::Ok(42) should be truthy");

        let obj = state.new_enum(result_shape, RESULT_OK, vec![TlangValue::I64(0)]);
        assert!(state.is_truthy(obj), "Result::Ok(0) should be truthy");
    }

    #[test]
    fn test_enum_truthiness_result_err() {
        let mut state = vm_state();
        let result_shape = state.heap.builtin_shapes.result;

        let obj = state.new_enum(result_shape, RESULT_ERR, vec![TlangValue::I64(42)]);
        assert!(!state.is_truthy(obj), "Result::Err should be falsy");
    }
}
