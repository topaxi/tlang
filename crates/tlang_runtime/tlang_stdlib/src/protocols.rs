use tlang_memory::value::object::{TlangObjectKind, TlangStruct};
use tlang_memory::{InterpreterState, NativeFnReturn, TlangValue};

use crate::option::{OPTION_VARIANT_NONE, OPTION_VARIANT_SOME};
use crate::result::{RESULT_VARIANT_ERR, RESULT_VARIANT_OK};

pub fn define_builtin_protocols(state: &mut InterpreterState) {
    state.register_protocol("Truthy".to_string(), vec!["truthy".to_string()]);
    state.register_protocol("Functor".to_string(), vec!["map".to_string()]);
    state.register_protocol("Iterable".to_string(), vec!["iter".to_string()]);
    state.register_protocol("Iterator".to_string(), vec!["next".to_string()]);

    // Truthy::truthy for Option
    let option_truthy = state.new_native_fn("Truthy::Option::truthy", |state, args| {
        let this = state.get_enum(args[0]).unwrap();
        NativeFnReturn::Return(TlangValue::Bool(this.variant == OPTION_VARIANT_SOME))
    });
    state.register_protocol_impl("Truthy", "Option", "truthy", option_truthy);

    // Truthy::truthy for Result
    let result_truthy = state.new_native_fn("Truthy::Result::truthy", |state, args| {
        let this = state.get_enum(args[0]).unwrap();
        NativeFnReturn::Return(TlangValue::Bool(this.variant == RESULT_VARIANT_OK))
    });
    state.register_protocol_impl("Truthy", "Result", "truthy", result_truthy);

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

    // Functor::map for Slice (maps over slice view, returns a new list)
    let slice_map = state.new_native_fn("Functor::Slice::map", |state, args| {
        let this = args[0];
        let func = args[1];
        let slice = state.get_slice(this).unwrap();
        let values: Vec<TlangValue> = state.get_slice_values(slice).to_vec();

        let mut result = Vec::with_capacity(values.len());
        for item in values {
            result.push(state.call(func, &[item]));
        }

        NativeFnReturn::Return(state.new_list(result))
    });
    state.register_protocol_impl("Functor", "Slice", "map", slice_map);

    // Functor::map for String (maps over each character)
    let string_map = state.new_native_fn("Functor::String::map", |state, args| {
        let this = args[0];
        let func = args[1];
        let chars: Vec<String> = state
            .get_object(this)
            .unwrap()
            .as_str()
            .unwrap()
            .chars()
            .map(|c| c.to_string())
            .collect();

        let mut result = String::with_capacity(chars.len());
        for ch in chars {
            let ch_val = state.new_string(ch);
            let mapped = state.call(func, &[ch_val]);
            result.push_str(&state.stringify(mapped));
        }

        NativeFnReturn::Return(state.new_string(result))
    });
    state.register_protocol_impl("Functor", "String", "map", string_map);

    // Iterable::iter for List → creates a ListIterator
    let list_iter = state.new_native_fn("Iterable::List::iter", |state, args| {
        let this = args[0];
        NativeFnReturn::Return(state.new_object(TlangObjectKind::Struct(TlangStruct::new(
            state.heap.builtin_shapes.list_iterator,
            vec![this, TlangValue::from(0i64)],
        ))))
    });
    state.register_protocol_impl("Iterable", "List", "iter", list_iter);

    // Iterable::iter for Slice → materializes slice to a list, then creates a ListIterator
    let slice_iter = state.new_native_fn("Iterable::Slice::iter", |state, args| {
        let this = args[0];
        let slice = state.get_slice(this).unwrap();
        let values = state.get_slice_values(slice).to_vec();
        let list = state.new_list(values);
        NativeFnReturn::Return(state.new_object(TlangObjectKind::Struct(TlangStruct::new(
            state.heap.builtin_shapes.list_iterator,
            vec![list, TlangValue::from(0i64)],
        ))))
    });
    state.register_protocol_impl("Iterable", "Slice", "iter", slice_iter);

    // Iterable::iter for String → splits into chars, creates a ListIterator over char strings
    let string_iter = state.new_native_fn("Iterable::String::iter", |state, args| {
        let this = args[0];
        let chars: Vec<String> = state
            .get_object(this)
            .unwrap()
            .as_str()
            .unwrap()
            .chars()
            .map(|c| c.to_string())
            .collect();
        let char_values: Vec<TlangValue> =
            chars.iter().map(|c| state.new_string(c.clone())).collect();
        let list = state.new_list(char_values);
        NativeFnReturn::Return(state.new_object(TlangObjectKind::Struct(TlangStruct::new(
            state.heap.builtin_shapes.list_iterator,
            vec![list, TlangValue::from(0i64)],
        ))))
    });
    state.register_protocol_impl("Iterable", "String", "iter", string_iter);

    // Iterator::next for ListIterator → returns Option::Some(value) or Option::None
    let list_iterator_next = state.new_native_fn("Iterator::ListIterator::next", |state, args| {
        let this = args[0];
        let iter = state.get_struct(this).unwrap();
        let list_val = iter[0];
        let index = iter[1].as_usize();
        let list_len = state.get_struct(list_val).unwrap().len();

        if index >= list_len {
            let none = state.new_enum(
                state.heap.builtin_shapes.option,
                OPTION_VARIANT_NONE,
                vec![],
            );
            return NativeFnReturn::Return(none);
        }

        let value = state.get_struct(list_val).unwrap()[index];
        let some = state.new_enum(
            state.heap.builtin_shapes.option,
            OPTION_VARIANT_SOME,
            vec![value],
        );
        let iter_mut = state.get_struct_mut(this).unwrap();
        iter_mut[1] = TlangValue::from(index + 1);
        NativeFnReturn::Return(some)
    });
    state.register_protocol_impl("Iterator", "ListIterator", "next", list_iterator_next);
}
