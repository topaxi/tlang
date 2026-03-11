use std::collections::HashMap;

use tlang_macros::native_fn;
use tlang_memory::{NativeFnReturn, TlangValue, VMState};

pub const OPTION_VARIANT_SOME: usize = 0;
pub const OPTION_VARIANT_NONE: usize = 1;

#[native_fn(name = "Option::Some")]
pub fn new_option_some(state: &mut VMState, value: TlangValue) -> TlangValue {
    state.new_enum(
        state.heap.builtin_shapes.option,
        OPTION_VARIANT_SOME,
        vec![value],
    )
}

#[allow(clippy::missing_panics_doc)]
pub fn define_option_shape(state: &mut VMState) {
    let mut method_map = HashMap::with_capacity(5);

    method_map.insert(
        "is_some".to_string(),
        state.new_native_method("Option::is_some", |state, this, _args| {
            let this = state.get_enum(this).unwrap();

            NativeFnReturn::Return(TlangValue::from(this.variant == OPTION_VARIANT_SOME))
        }),
    );

    method_map.insert(
        "is_none".to_string(),
        state.new_native_method("Option::is_none", |state, this, _args| {
            let this = state.get_enum(this).unwrap();

            NativeFnReturn::Return(TlangValue::from(this.variant == OPTION_VARIANT_NONE))
        }),
    );

    method_map.insert(
        "unwrap".to_string(),
        state.new_native_method("Option::unwrap", |state, this, _args| {
            let this = state.get_enum(this).unwrap();

            if this.variant == OPTION_VARIANT_NONE {
                state.panic("Called unwrap on None".to_string());
            }

            NativeFnReturn::Return(this.field_values[0])
        }),
    );

    method_map.insert(
        "map".to_string(),
        state.new_native_method("Option::map", |state, this, args| {
            let func = args[0];
            let this = state.get_enum(this).unwrap();

            if this.variant == OPTION_VARIANT_NONE {
                let none = state.new_enum(
                    state.heap.builtin_shapes.option,
                    OPTION_VARIANT_NONE,
                    vec![],
                );
                return NativeFnReturn::Return(none);
            }

            let inner = this.field_values[0];
            let mapped = state.call(func, &[inner]);
            let some = state.new_enum(
                state.heap.builtin_shapes.option,
                OPTION_VARIANT_SOME,
                vec![mapped],
            );
            NativeFnReturn::Return(some)
        }),
    );

    state
        .heap
        .builtin_shapes
        .get_option_shape_mut()
        .set_methods(method_map);

    let none_value = state.new_enum(
        state.heap.builtin_shapes.option,
        OPTION_VARIANT_NONE,
        vec![],
    );

    state.set_global("Option::None".to_string(), none_value);
}

#[cfg(test)]
mod tests {
    use tlang_memory::{TlangValue, VMState};

    use crate::option::{OPTION_VARIANT_NONE, OPTION_VARIANT_SOME};

    use super::define_option_shape;
    use crate::protocols::define_builtin_protocols;

    fn vm_state() -> VMState {
        let mut state = VMState::new();
        define_option_shape(&mut state);
        define_builtin_protocols(&mut state);
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
