use tlang_memory::{InterpreterState, NativeFnReturn, TlangValue};

use crate::option::{OPTION_VARIANT_NONE, OPTION_VARIANT_SOME};
use crate::result::{RESULT_VARIANT_ERR, RESULT_VARIANT_OK};

pub fn define_builtin_protocols(state: &mut InterpreterState) {
    state.register_protocol("Functor".to_string(), vec!["map".to_string()]);

    // Functor::map for Option
    let option_map = state.new_native_fn("Functor::Option::map", |state, args| {
        let this = args[0];
        let func = args[1];
        let this_enum = state.get_enum(this).unwrap();

        if this_enum.variant == OPTION_VARIANT_NONE {
            let none = state.new_enum(
                state.heap.builtin_shapes.option,
                OPTION_VARIANT_NONE,
                vec![],
            );
            return NativeFnReturn::Return(none);
        }

        let inner = this_enum.field_values[0];
        let mapped = state.call(func, &[inner]);
        let some = state.new_enum(
            state.heap.builtin_shapes.option,
            OPTION_VARIANT_SOME,
            vec![mapped],
        );
        NativeFnReturn::Return(some)
    });
    state.register_protocol_impl("Functor", "Option", "map", option_map);

    // Functor::map for Result
    let result_map = state.new_native_fn("Functor::Result::map", |state, args| {
        let this = args[0];
        let func = args[1];
        let this_enum = state.get_enum(this).unwrap();

        if this_enum.variant == RESULT_VARIANT_ERR {
            let err_val = this_enum.field_values[0];
            let err = state.new_enum(
                state.heap.builtin_shapes.result,
                RESULT_VARIANT_ERR,
                vec![err_val],
            );
            return NativeFnReturn::Return(err);
        }

        let inner = this_enum.field_values[0];
        let mapped = state.call(func, &[inner]);
        let ok = state.new_enum(
            state.heap.builtin_shapes.result,
            RESULT_VARIANT_OK,
            vec![mapped],
        );
        NativeFnReturn::Return(ok)
    });
    state.register_protocol_impl("Functor", "Result", "map", result_map);

    // Functor::map for List
    let list_map = state.new_native_fn("Functor::List::map", |state, args| {
        let this = args[0];
        let func = args[1];
        let list = state.get_struct(this).unwrap();
        let len = list.len();
        let values: Vec<TlangValue> = list.values().to_vec();

        let mut result = Vec::with_capacity(len);
        for item in values {
            result.push(state.call(func, &[item]));
        }

        NativeFnReturn::Return(state.new_list(result))
    });
    state.register_protocol_impl("Functor", "List", "map", list_map);
}
