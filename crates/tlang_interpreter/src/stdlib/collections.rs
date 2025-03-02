use std::collections::HashMap;

use log::debug;
use tlang_macros::native_fn;

use crate::shape::{TlangStructMethod, TlangStructShape};
use crate::state::InterpreterState;
use crate::value::{NativeFnReturn, TlangObjectKind, TlangStruct, TlangValue};
use crate::Interpreter;

#[native_fn(name = "len")]
pub fn len(state: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    match state.get_object(args[0]) {
        Some(TlangObjectKind::Struct(obj)) => TlangValue::U64(obj.field_values.len() as u64),
        Some(TlangObjectKind::String(string)) => TlangValue::U64(string.len() as u64),
        _ => panic!("Expected struct or string, got {:?}", args[0]),
    }
}

pub fn define_list_shape(interpreter: &mut Interpreter) {
    let mut list_methods = HashMap::with_capacity(1);

    list_methods.insert(
        "slice".to_string(),
        TlangStructMethod::from_value(interpreter.create_native_fn(|state, args| {
            debug!(
                "Calling slice on list, args: {:?}",
                args.iter().map(|a| state.stringify(*a)).collect::<Vec<_>>()
            );

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

            let field_values = this.field_values[start..end].to_vec();

            NativeFnReturn::Return(state.new_object(TlangObjectKind::Struct(TlangStruct {
                shape: this.shape,
                field_values,
            })))
        })),
    );

    interpreter
        .state
        .builtin_shapes
        .get_list_shape_mut()
        .set_methods(list_methods);
}
