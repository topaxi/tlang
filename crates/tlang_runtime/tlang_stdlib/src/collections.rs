use std::collections::HashMap;

use tlang_macros::native_fn;
use tlang_memory::value::object::NativeFnReturn;
use tlang_memory::{InterpreterState, prelude::*};

#[native_fn(name = "len")]
pub fn len(state: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    match state.get_object(args[0]) {
        Some(TlangObjectKind::Struct(obj)) => TlangValue::from(obj.len()),
        Some(TlangObjectKind::String(string)) => TlangValue::from(string.len()),
        _ => state.panic(format!("Expected struct or string, got {:?}", args[0])),
    }
}

pub fn define_list_shape(state: &mut InterpreterState) {
    define_list_iterator_shape(state);

    let mut method_map = HashMap::with_capacity(2);

    method_map.insert(
        "iter".to_string(),
        state.new_native_method(|state, this, _args| {
            NativeFnReturn::Return(state.new_object(TlangObjectKind::Struct(TlangStruct::new(
                state.builtin_shapes.list_iterator,
                vec![this, TlangValue::from(0)],
            ))))
        }),
    );

    method_map.insert(
        "slice".to_string(),
        state.new_native_method(|state, this, args| {
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
        .builtin_shapes
        .get_list_shape_mut()
        .set_methods(method_map);
}

fn define_list_iterator_shape(state: &mut InterpreterState) {
    let mut method_map = HashMap::with_capacity(1);

    method_map.insert(
        "next".to_string(),
        state.new_native_method(|state, this, _args| {
            let iter = state.get_struct(this).unwrap();
            let list = state.get_struct(iter[0]).unwrap();
            let index = iter[1].as_usize();

            if index >= list.len() {
                let none = state.new_enum(state.builtin_shapes.option, 1, vec![]);

                NativeFnReturn::Return(none)
            } else {
                let some = state.new_enum(state.builtin_shapes.option, 0, vec![list[index]]);
                let iter_mut = state.get_struct_mut(this).unwrap();
                iter_mut[1] = TlangValue::from(index + 1);

                NativeFnReturn::Return(some)
            }
        }),
    );

    state
        .builtin_shapes
        .get_list_iterator_shape_mut()
        .set_methods(method_map);
}
