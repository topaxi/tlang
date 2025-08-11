use std::collections::HashMap;

use tlang_macros::native_fn;
use tlang_memory::value::object::NativeFnReturn;
use tlang_memory::{InterpreterState, TlangValue};

#[native_fn(name = "Option::Some")]
pub fn new_option_some(state: &mut InterpreterState, value: TlangValue) -> TlangValue {
    state.new_enum(state.builtin_shapes.option, 0, vec![value])
}

#[allow(clippy::missing_panics_doc)]
pub fn define_option_shape(state: &mut InterpreterState) {
    let mut method_map = HashMap::with_capacity(3);

    method_map.insert(
        "is_some".to_string(),
        state.new_native_method(|state, this, _args| {
            let this = state.get_enum(this).unwrap();

            NativeFnReturn::Return(TlangValue::from(this.variant == 0))
        }),
    );

    method_map.insert(
        "is_none".to_string(),
        state.new_native_method(|state, this, _args| {
            let this = state.get_enum(this).unwrap();

            NativeFnReturn::Return(TlangValue::from(this.variant == 1))
        }),
    );

    method_map.insert(
        "unwrap".to_string(),
        state.new_native_method(|state, this, _args| {
            let this = state.get_enum(this).unwrap();

            if this.variant == 1 {
                state.panic("Called unwrap on None".to_string());
            }

            NativeFnReturn::Return(this.field_values[0])
        }),
    );

    state
        .builtin_shapes
        .get_option_shape_mut()
        .set_methods(method_map);

    let none_value = state.new_enum(state.builtin_shapes.option, 1, vec![]);

    state.set_global("Option::None".to_string(), none_value);
}
