use tlang_macros::{define_struct, native_fn, native_method, protocol_impl};
use tlang_memory::value::object::{TlangObjectKind, TlangStruct};
use tlang_memory::{TlangValue, VMState};

use crate::option::{OPTION_NONE, OPTION_SOME};

#[native_fn(name = "len")]
pub fn len(state: &mut VMState, iterable: TlangValue) -> TlangValue {
    match state.get_object(iterable) {
        Some(TlangObjectKind::Struct(obj)) => TlangValue::from(obj.len()),
        Some(TlangObjectKind::String(string)) => TlangValue::from(string.len()),
        _ => state.panic(format!("Expected struct or string, got {iterable:?}")),
    }
}

define_struct! {
    struct ListIterator { list, index }
}

define_struct! {
    struct List {}
}

#[native_method("List")]
fn iter(vm: &mut VMState, this: TlangValue) -> TlangValue {
    vm.new_object(TlangObjectKind::Struct(TlangStruct::new(
        vm.heap.builtin_shapes.list_iterator,
        vec![this, TlangValue::from(0i64)],
    )))
}

#[native_method("List")]
fn map(vm: &mut VMState, this: TlangValue, func: TlangValue) -> TlangValue {
    let list = vm.get_struct(this).unwrap();
    let len = list.len();
    let values: Vec<TlangValue> = list.values().to_vec();

    let mut result = Vec::with_capacity(len);
    for item in values {
        result.push(vm.call(func, &[item]));
    }

    vm.new_list(result)
}

#[native_method("List")]
fn slice(vm: &mut VMState, this: TlangValue, start: TlangValue, end: TlangValue) -> TlangValue {
    let list = vm.get_struct(this).unwrap();

    let start_idx = start.as_usize();
    let end_idx = if end.is_nil() {
        list.len()
    } else {
        end.as_usize()
    };

    vm.new_slice(this, start_idx, end_idx - start_idx)
}

#[native_method("ListIterator")]
fn next(vm: &mut VMState, this: TlangValue) -> TlangValue {
    let iter_obj = vm.get_struct(this).unwrap();
    let list_val = iter_obj[0];
    let index = iter_obj[1].as_usize();
    let list = vm.get_struct(list_val).unwrap();

    if index >= list.len() {
        return vm.new_enum(vm.heap.builtin_shapes.option, OPTION_NONE, vec![]);
    }

    let value = list[index];
    let some = vm.new_enum(vm.heap.builtin_shapes.option, OPTION_SOME, vec![value]);
    let iter_mut = vm.get_struct_mut(this).unwrap();
    iter_mut[1] = TlangValue::from(index + 1);
    some
}

// --- Protocol implementations ---

#[protocol_impl("Functor", "List", method = "map")]
fn functor_map(vm: &mut VMState, this: TlangValue, func: TlangValue) -> TlangValue {
    let list = vm.get_struct(this).unwrap();
    let len = list.len();
    let values: Vec<TlangValue> = list.values().to_vec();

    let mut result = Vec::with_capacity(len);
    for item in values {
        result.push(vm.call(func, &[item]));
    }

    vm.new_list(result)
}

#[protocol_impl("Functor", "Slice", method = "map")]
fn slice_functor_map(vm: &mut VMState, this: TlangValue, func: TlangValue) -> TlangValue {
    let s = vm.get_slice(this).unwrap();
    let values: Vec<TlangValue> = vm.get_slice_values(s).to_vec();

    let mut result = Vec::with_capacity(values.len());
    for item in values {
        result.push(vm.call(func, &[item]));
    }

    vm.new_list(result)
}

#[protocol_impl("Functor", "String", method = "map")]
fn string_functor_map(vm: &mut VMState, this: TlangValue, func: TlangValue) -> TlangValue {
    let chars: Vec<String> = vm
        .get_object(this)
        .unwrap()
        .as_str()
        .unwrap()
        .chars()
        .map(|c| c.to_string())
        .collect();

    let mut result = String::with_capacity(chars.len());
    for ch in chars {
        let ch_val = vm.new_string(ch);
        let mapped = vm.call(func, &[ch_val]);
        result.push_str(&vm.stringify(mapped));
    }

    vm.new_string(result)
}

#[protocol_impl("Iterable", "List", method = "iter")]
fn list_iter(vm: &mut VMState, this: TlangValue) -> TlangValue {
    vm.new_object(TlangObjectKind::Struct(TlangStruct::new(
        vm.heap.builtin_shapes.list_iterator,
        vec![this, TlangValue::from(0i64)],
    )))
}

#[protocol_impl("Iterable", "Slice", method = "iter")]
fn slice_iter(vm: &mut VMState, this: TlangValue) -> TlangValue {
    let s = vm.get_slice(this).unwrap();
    let values = vm.get_slice_values(s).to_vec();
    let list = vm.new_list(values);
    vm.new_object(TlangObjectKind::Struct(TlangStruct::new(
        vm.heap.builtin_shapes.list_iterator,
        vec![list, TlangValue::from(0i64)],
    )))
}

#[protocol_impl("Iterable", "String", method = "iter")]
fn string_iter(vm: &mut VMState, this: TlangValue) -> TlangValue {
    let chars: Vec<String> = vm
        .get_object(this)
        .unwrap()
        .as_str()
        .unwrap()
        .chars()
        .map(|c| c.to_string())
        .collect();
    let char_values: Vec<TlangValue> = chars.iter().map(|c| vm.new_string(c.clone())).collect();
    let list = vm.new_list(char_values);
    vm.new_object(TlangObjectKind::Struct(TlangStruct::new(
        vm.heap.builtin_shapes.list_iterator,
        vec![list, TlangValue::from(0i64)],
    )))
}

#[protocol_impl("Iterator", "ListIterator", method = "next")]
fn list_iterator_next(vm: &mut VMState, this: TlangValue) -> TlangValue {
    let iter_obj = vm.get_struct(this).unwrap();
    let list_val = iter_obj[0];
    let index = iter_obj[1].as_usize();
    let list_len = vm.get_struct(list_val).unwrap().len();

    if index >= list_len {
        return vm.new_enum(vm.heap.builtin_shapes.option, OPTION_NONE, vec![]);
    }

    let value = vm.get_struct(list_val).unwrap()[index];
    let some = vm.new_enum(vm.heap.builtin_shapes.option, OPTION_SOME, vec![value]);
    let iter_mut = vm.get_struct_mut(this).unwrap();
    iter_mut[1] = TlangValue::from(index + 1);
    some
}

/// Legacy entry point — now a no-op.
#[deprecated(note = "Use VMState::collect_native_inventory() instead")]
pub fn define_list_shape(_state: &mut VMState) {}
