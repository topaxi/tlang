use std::collections::HashMap;

use tlang_macros::native_fn;
use tlang_memory::{InterpreterState, NativeFnReturn, TlangValue};

#[native_fn(name = "Result::Ok")]
pub fn new_result_ok(state: &mut InterpreterState, value: TlangValue) -> TlangValue {
    state.new_enum(state.builtin_shapes.result, 0, vec![value])
}

#[native_fn(name = "Result::Err")]
pub fn new_result_err(state: &mut InterpreterState, err: TlangValue) -> TlangValue {
    state.new_enum(state.builtin_shapes.result, 1, vec![err])
}

#[allow(clippy::missing_panics_doc)]
pub fn define_result_shape(state: &mut InterpreterState) {
    let mut method_map = HashMap::with_capacity(3);

    method_map.insert(
        "is_ok".to_string(),
        state.new_native_method(|state, this, _args| {
            let this = state.get_enum(this).unwrap();

            NativeFnReturn::Return(TlangValue::from(this.variant == 0))
        }),
    );

    method_map.insert(
        "is_err".to_string(),
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
                state.panic("Called unwrap on Err".to_string());
            } else {
                NativeFnReturn::Return(this.field_values[0])
            }
        }),
    );
    state
        .builtin_shapes
        .get_result_shape_mut()
        .set_methods(method_map);
}
