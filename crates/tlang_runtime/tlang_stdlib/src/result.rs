use std::collections::HashMap;

use tlang_macros::native_fn;
use tlang_memory::{InterpreterState, NativeFnReturn, TlangValue};

pub const RESULT_VARIANT_OK: usize = 0;
pub const RESULT_VARIANT_ERR: usize = 1;

#[native_fn(name = "Result::Ok")]
pub fn new_result_ok(state: &mut InterpreterState, value: TlangValue) -> TlangValue {
    state.new_enum(
        state.heap.builtin_shapes.result,
        RESULT_VARIANT_OK,
        vec![value],
    )
}

#[native_fn(name = "Result::Err")]
pub fn new_result_err(state: &mut InterpreterState, err: TlangValue) -> TlangValue {
    state.new_enum(
        state.heap.builtin_shapes.result,
        RESULT_VARIANT_ERR,
        vec![err],
    )
}

#[allow(clippy::missing_panics_doc)]
pub fn define_result_shape(state: &mut InterpreterState) {
    let mut method_map = HashMap::with_capacity(5);

    method_map.insert(
        "is_ok".to_string(),
        state.new_native_method("Result::is_ok", |state, this, _args| {
            let this = state.get_enum(this).unwrap();

            NativeFnReturn::Return(TlangValue::from(this.variant == RESULT_VARIANT_OK))
        }),
    );

    method_map.insert(
        "is_err".to_string(),
        state.new_native_method("Result::is_err", |state, this, _args| {
            let this = state.get_enum(this).unwrap();

            NativeFnReturn::Return(TlangValue::from(this.variant == RESULT_VARIANT_ERR))
        }),
    );

    method_map.insert(
        "map".to_string(),
        state.new_native_method("Result::map", |state, this, args| {
            let func = args[0];
            let this = state.get_enum(this).unwrap();

            if this.variant == RESULT_VARIANT_ERR {
                let err_val = this.field_values[0];
                let err = state.new_enum(
                    state.heap.builtin_shapes.result,
                    RESULT_VARIANT_ERR,
                    vec![err_val],
                );
                return NativeFnReturn::Return(err);
            }

            let inner = this.field_values[0];
            let mapped = state.call(func, &[inner]);
            let ok = state.new_enum(
                state.heap.builtin_shapes.result,
                RESULT_VARIANT_OK,
                vec![mapped],
            );
            NativeFnReturn::Return(ok)
        }),
    );

    method_map.insert(
        "unwrap".to_string(),
        state.new_native_method("Result::unwrap", |state, this, _args| {
            let this = state.get_enum(this).unwrap();

            if this.variant == RESULT_VARIANT_ERR {
                state.panic("Called unwrap on Err".to_string());
            } else {
                NativeFnReturn::Return(this.field_values[0])
            }
        }),
    );

    state
        .heap
        .builtin_shapes
        .get_result_shape_mut()
        .set_methods(method_map);
}

#[cfg(test)]
mod tests {
    use tlang_memory::{InterpreterState, TlangValue};

    use crate::result::{RESULT_VARIANT_ERR, RESULT_VARIANT_OK};

    use super::define_result_shape;
    use crate::protocols::define_builtin_protocols;

    fn interpreter_state() -> InterpreterState {
        let mut state = InterpreterState::new();
        define_result_shape(&mut state);
        define_builtin_protocols(&mut state);
        state
    }

    #[test]
    fn test_enum_truthiness_result_ok() {
        let mut state = interpreter_state();
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
        let mut state = interpreter_state();
        let result_shape = state.heap.builtin_shapes.result;

        // Result::Err should be falsy (variant 1 = Err)
        let obj = state.new_enum(result_shape, RESULT_VARIANT_ERR, vec![TlangValue::I64(42)]);
        assert!(!state.is_truthy(obj), "Result::Err should be falsy");
    }
}
