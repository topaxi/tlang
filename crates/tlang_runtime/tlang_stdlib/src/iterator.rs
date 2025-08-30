use tlang_macros::native_fn;
use tlang_memory::prelude::*;
use tlang_memory::{InterpreterState, TlangObjectKind, TlangValue};

#[native_fn(name = "iterator::iter")]
pub fn iter_fn(state: &mut InterpreterState, value: TlangValue) -> TlangValue {
    // This native function mimics a protocol/trait system for iteration
    // It calls the iter method on the tlang struct/object
    match state.get_object(value) {
        Some(TlangObjectKind::Struct(_)) => {
            // For lists/structs, create an iterator using the existing iter method logic
            // This mimics what the List::iter method does
            state.new_object(TlangObjectKind::Struct(TlangStruct::new(
                state.builtin_shapes.list_iterator,
                vec![value, TlangValue::from(0)],
            )))
        }
        _ => state.panic(format!("Expected iterable struct, got {:?}", value)),
    }
}
