use tlang_macros::native_fn;

use crate::state::InterpreterState;
use crate::value::{TlangObjectKind, TlangValue};

#[native_fn(name = "len")]
pub fn len(state: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    match state.get_object(args[0]) {
        Some(TlangObjectKind::Struct(obj)) => TlangValue::Int(obj.field_values.len() as i64),
        Some(TlangObjectKind::String(string)) => TlangValue::Int(string.len() as i64),
        _ => panic!("Expected struct or string, got {:?}", args[0]),
    }
}
