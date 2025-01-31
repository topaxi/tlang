use std::collections::HashMap;

use tlang_macros::native_fn;

use crate::state::InterpreterState;
use crate::value::{
    NativeFnReturn, TlangObjectId, TlangObjectKind, TlangStruct, TlangStructMethod,
    TlangStructShape, TlangValue,
};
use crate::Interpreter;

#[native_fn(name = "len")]
pub fn len(state: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    match state.get_object(args[0]) {
        Some(TlangObjectKind::Struct(obj)) => TlangValue::Int(obj.field_values.len() as i64),
        Some(TlangObjectKind::String(string)) => TlangValue::Int(string.len() as i64),
        _ => panic!("Expected struct or string, got {:?}", args[0]),
    }
}

pub fn define_list_shape(interpreter: &mut Interpreter) {
    let mut list_methods = HashMap::new();

    let slice_method_id = TlangObjectId::new();
    let slice_method = TlangStructMethod::Native(slice_method_id);

    list_methods.insert("slice".to_string(), slice_method);

    let list_shape = TlangStructShape::new("List".to_string(), vec![], list_methods);

    interpreter.insert_native_fn(slice_method_id, |state, args| {
        let this = state
            .get_object(args[0])
            .and_then(|o| o.get_struct())
            .unwrap();

        let start = args[1].as_usize().unwrap();
        let end = if args.len() < 3 {
            this.field_values.len()
        } else {
            args[2].as_usize().unwrap_or(this.field_values.len())
        };

        let field_values = this.field_values[start..end].to_vec();

        NativeFnReturn::Return(state.new_object(TlangObjectKind::Struct(TlangStruct {
            shape: this.shape,
            field_values,
        })))
    });

    interpreter
        .state
        .shapes
        .insert(interpreter.state.list_shape, list_shape);
}
