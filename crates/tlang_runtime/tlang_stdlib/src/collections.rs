use std::collections::HashMap;

use tlang_macros::native_fn;
use tlang_memory::value::NativeFnReturn;
use tlang_memory::{InterpreterState, prelude::*};

#[native_fn(name = "len")]
pub fn len(state: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    match state.get_object(args[0]) {
        Some(TlangObjectKind::Struct(obj)) => TlangValue::from(obj.field_values.len()),
        Some(TlangObjectKind::String(string)) => TlangValue::from(string.len()),
        _ => state.panic(format!("Expected struct or string, got {:?}", args[0])),
    }
}

pub fn define_list_shape(state: &mut InterpreterState) {
    let mut method_map = HashMap::with_capacity(1);

    method_map.insert(
        "slice".to_string(),
        TlangStructMethod::from_value(state.new_native_fn(|state, args| {
            let this = state
                .get_object(args[0])
                .and_then(|o| o.get_struct())
                .unwrap();

            let start = args[1].as_usize();
            let end = if args.len() < 3 || args[2].is_nil() {
                this.field_values.len()
            } else {
                args[2].as_usize()
            };

            NativeFnReturn::Return(state.new_slice(args[0], start, end - start))
        })),
    );

    state
        .builtin_shapes
        .get_list_shape_mut()
        .set_methods(method_map);
}
