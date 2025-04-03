use std::collections::HashMap;

use tlang_macros::native_fn;
use tlang_memory::value::NativeFnReturn;
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
    let mut method_map = HashMap::with_capacity(1);

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
