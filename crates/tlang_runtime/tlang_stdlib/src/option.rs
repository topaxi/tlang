use tlang_macros::{define_enum, native_fn};
use tlang_memory::{TlangValue, VMState};

define_enum! {
    enum Option {
        Some(value),
        None,
    }

    impl Option {
        fn is_some(this) {
            TlangValue::from(vm.get_enum(this).unwrap().variant == OPTION_SOME)
        }

        fn is_none(this) {
            TlangValue::from(vm.get_enum(this).unwrap().variant == OPTION_NONE)
        }

        fn unwrap(this) {
            let e = vm.get_enum(this).unwrap();
            if e.variant == OPTION_NONE {
                vm.panic("Called unwrap on None".to_string());
            }
            e.field_values[0]
        }

        fn map(this, func) {
            let e = vm.get_enum(this).unwrap();
            if e.variant == OPTION_NONE {
                return vm.new_enum(vm.heap.builtin_shapes.option, OPTION_NONE, vec![]);
            }
            let inner = e.field_values[0];
            let mapped = vm.call(func, &[inner]);
            vm.new_enum(vm.heap.builtin_shapes.option, OPTION_SOME, vec![mapped])
        }
    }

    impl Truthy for Option {
        fn truthy(this) {
            TlangValue::Bool(vm.get_enum(this).unwrap().variant == OPTION_SOME)
        }
    }

    impl Functor for Option {
        fn map(this, func) {
            let e = vm.get_enum(this).unwrap();
            if e.variant == OPTION_NONE {
                return vm.new_enum(vm.heap.builtin_shapes.option, OPTION_NONE, vec![]);
            }
            let inner = e.field_values[0];
            let mapped = vm.call(func, &[inner]);
            vm.new_enum(vm.heap.builtin_shapes.option, OPTION_SOME, vec![mapped])
        }
    }
}

#[native_fn(name = "Option::Some")]
pub fn new_option_some(state: &mut VMState, value: TlangValue) -> TlangValue {
    state.new_enum(state.heap.builtin_shapes.option, OPTION_SOME, vec![value])
}

#[cfg(test)]
mod tests {
    use tlang_memory::{TlangValue, VMState};

    use crate::option::{OPTION_NONE, OPTION_SOME};

    fn vm_state() -> VMState {
        let mut state = VMState::new();
        state.collect_native_inventory();
        state
    }

    #[test]
    fn test_enum_truthiness_option_some() {
        let mut state = vm_state();
        let option_shape = state.heap.builtin_shapes.option;

        let obj = state.new_enum(option_shape, OPTION_SOME, vec![TlangValue::I64(42)]);
        assert!(state.is_truthy(obj), "Option::Some(42) should be truthy");

        let obj = state.new_enum(option_shape, OPTION_SOME, vec![TlangValue::I64(0)]);
        assert!(state.is_truthy(obj), "Option::Some(0) should be truthy");
    }

    #[test]
    fn test_enum_truthiness_option_none() {
        let mut state = vm_state();
        let option_shape = state.heap.builtin_shapes.option;

        let obj = state.new_enum(option_shape, OPTION_NONE, vec![]);
        assert!(!state.is_truthy(obj), "Option::None should be falsy");
    }
}
