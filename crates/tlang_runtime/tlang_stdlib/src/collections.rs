use std::collections::HashMap;

use tlang_macros::native_fn;
use tlang_memory::value::function::NativeFnReturn;
use tlang_memory::{InterpreterState, prelude::*};

#[native_fn(name = "len")]
pub fn len(state: &mut InterpreterState, iterable: TlangValue) -> TlangValue {
    match state.get_object(iterable) {
        Some(TlangObjectKind::Struct(obj)) => TlangValue::from(obj.len()),
        Some(TlangObjectKind::String(string)) => TlangValue::from(string.len()),
        _ => state.panic(format!("Expected struct or string, got {iterable:?}")),
    }
}

#[allow(clippy::missing_panics_doc)]
#[native_fn(name = "map")]
pub fn map(state: &mut InterpreterState, iterable: TlangValue, func: TlangValue) -> TlangValue {
    match state.get_object(iterable) {
        Some(TlangObjectKind::Struct(s)) => {
            if let Some(shape) = state.heap.get_shape(s)
                && let Some(TlangStructMethod::Native(id)) = shape.get_method("map")
            {
                let id = *id;
                return *state
                    .call_native_fn(id, &[iterable, func])
                    .unwrap()
                    .value()
                    .unwrap();
            }

            state.panic(format!(
                "map is not defined for {}",
                state.stringify(iterable)
            ));
        }
        Some(TlangObjectKind::Enum(e)) => {
            if let Some(shape) = state.heap.get_shape(e)
                && let Some(TlangStructMethod::Native(id)) = shape.get_method("map")
            {
                let id = *id;
                return *state
                    .call_native_fn(id, &[iterable, func])
                    .unwrap()
                    .value()
                    .unwrap();
            }

            state.panic(format!(
                "map is not defined for {}",
                state.stringify(iterable)
            ));
        }
        Some(TlangObjectKind::Slice(slice)) => {
            let values: Vec<TlangValue> = state.get_slice_values(*slice).to_vec();
            let mut result = Vec::with_capacity(values.len());
            for item in values {
                result.push(state.call(func, &[item]));
            }
            state.new_list(result)
        }
        Some(TlangObjectKind::String(s)) => {
            let chars: Vec<String> = s.chars().map(|c| c.to_string()).collect();
            let mut result = String::with_capacity(chars.len());
            for ch in chars {
                let ch_val = state.new_string(ch);
                let mapped = state.call(func, &[ch_val]);
                result.push_str(&state.stringify(mapped));
            }
            state.new_string(result)
        }
        _ => state.panic(format!(
            "map is not defined for {}",
            state.stringify(iterable)
        )),
    }
}

#[allow(clippy::missing_panics_doc)]
pub fn define_list_shape(state: &mut InterpreterState) {
    define_list_iterator_shape(state);

    let mut method_map = HashMap::with_capacity(3);

    method_map.insert(
        "iter".to_string(),
        state.new_native_method("List::iter", |state, this, _args| {
            NativeFnReturn::Return(state.new_object(TlangObjectKind::Struct(TlangStruct::new(
                state.heap.builtin_shapes.list_iterator,
                vec![this, TlangValue::from(0)],
            ))))
        }),
    );

    method_map.insert(
        "map".to_string(),
        state.new_native_method("List::map", |state, this, args| {
            let func = args[0];
            let list = state.get_struct(this).unwrap();
            let len = list.len();
            let values: Vec<TlangValue> = list.values().to_vec();

            let mut result = Vec::with_capacity(len);
            for item in values {
                result.push(state.call(func, &[item]));
            }

            NativeFnReturn::Return(state.new_list(result))
        }),
    );

    method_map.insert(
        "slice".to_string(),
        state.new_native_method("List::slice", |state, this, args| {
            let list = state.get_struct(this).unwrap();

            let start = args[0].as_usize();
            let end = if args.len() < 2 || args[1].is_nil() {
                list.len()
            } else {
                args[1].as_usize()
            };

            NativeFnReturn::Return(state.new_slice(this, start, end - start))
        }),
    );

    state
        .heap
        .builtin_shapes
        .get_list_shape_mut()
        .set_methods(method_map);
}

fn define_list_iterator_shape(state: &mut InterpreterState) {
    let mut method_map = HashMap::with_capacity(1);

    method_map.insert(
        "next".to_string(),
        state.new_native_method("Iterator::next", |state, this, _args| {
            let iter = state.get_struct(this).unwrap();
            let list = state.get_struct(iter[0]).unwrap();
            let index = iter[1].as_usize();

            if index >= list.len() {
                let none = state.new_enum(state.heap.builtin_shapes.option, 1, vec![]);

                NativeFnReturn::Return(none)
            } else {
                let some = state.new_enum(state.heap.builtin_shapes.option, 0, vec![list[index]]);
                let iter_mut = state.get_struct_mut(this).unwrap();
                iter_mut[1] = TlangValue::from(index + 1);

                NativeFnReturn::Return(some)
            }
        }),
    );

    state
        .heap
        .builtin_shapes
        .get_list_iterator_shape_mut()
        .set_methods(method_map);
}
