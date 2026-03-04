use tlang_hir as hir;
use tlang_runtime::memory::{InterpreterState, NativeFnReturn, prelude::*};
use wasm_bindgen::prelude::*;

pub struct TlangInterpreter(tlang_runtime::interpreter::Interpreter);

impl Default for TlangInterpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl TlangInterpreter {
    pub(crate) fn new() -> Self {
        let interpreter = tlang_runtime::interpreter::Interpreter::default();

        TlangInterpreter(interpreter)
    }

    pub fn define_js_fn(&mut self, name: &str, f: js_sys::Function) {
        self.0.define_native_fn(name, move |state, args| {
            let this = JsValue::null();

            let value = match args.len() {
                0 => {
                    let result = f.call0(&this);
                    match result {
                        Ok(value) => js_value_to_tlang_value(state, &value),
                        Err(err) => {
                            state.panic(format!("Error calling JavaScript function: {err:?}"))
                        }
                    }
                }
                1 => {
                    let arg = tlang_value_to_js_value(state, args[0]).unwrap();
                    let result = f.call1(&this, &arg);
                    match result {
                        Ok(value) => js_value_to_tlang_value(state, &value),
                        Err(err) => {
                            state.panic(format!("Error calling JavaScript function: {err:?}"))
                        }
                    }
                }
                2 => {
                    let arg1 = tlang_value_to_js_value(state, args[0]).unwrap();
                    let arg2 = tlang_value_to_js_value(state, args[1]).unwrap();
                    let result = f.call2(&this, &arg1, &arg2);
                    match result {
                        Ok(value) => js_value_to_tlang_value(state, &value),
                        Err(err) => {
                            state.panic(format!("Error calling JavaScript function: {err:?}"))
                        }
                    }
                }
                3 => {
                    let arg1 = tlang_value_to_js_value(state, args[0]).unwrap();
                    let arg2 = tlang_value_to_js_value(state, args[1]).unwrap();
                    let arg3 = tlang_value_to_js_value(state, args[2]).unwrap();
                    let result = f.call3(&this, &arg1, &arg2, &arg3);
                    match result {
                        Ok(value) => js_value_to_tlang_value(state, &value),
                        Err(err) => {
                            state.panic(format!("Error calling JavaScript function: {err:?}"))
                        }
                    }
                }
                _ => {
                    let js_args = js_sys::Array::new();
                    for arg in args {
                        js_args.push(&tlang_value_to_js_value(state, *arg).unwrap());
                    }
                    let result = f.apply(&this, &js_args);
                    match result {
                        Ok(value) => js_value_to_tlang_value(state, &value),
                        Err(err) => {
                            state.panic(format!("Error calling JavaScript function: {err:?}"))
                        }
                    }
                }
            };

            NativeFnReturn::Return(value)
        });
    }

    pub(crate) fn eval(&mut self, hir: &hir::Module) -> Result<JsValue, TlangValueSerializeError> {
        let value = self.0.eval(hir);
        tlang_value_to_js_value(self.0.state(), value)
    }
}

#[derive(Debug)]
pub struct TlangValueSerializeError(String);

impl TlangValueSerializeError {
    pub fn new(message: &str) -> Self {
        Self(message.to_string())
    }
}

impl std::fmt::Display for TlangValueSerializeError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl std::error::Error for TlangValueSerializeError {}

fn tlang_value_to_js_value(
    state: &InterpreterState,
    value: TlangValue,
) -> Result<JsValue, TlangValueSerializeError> {
    let js_value = match value {
        TlangValue::Nil => JsValue::null(),
        TlangValue::Object(_) => tlang_object_to_js_value(state, value)?,
        value => match value.as_primitive() {
            TlangPrimitive::Nil => JsValue::null(),
            TlangPrimitive::Bool(value) => JsValue::from(value),
            TlangPrimitive::Int(value) => JsValue::from(value),
            TlangPrimitive::UInt(value) => JsValue::from(value),
            TlangPrimitive::Float(value) => JsValue::from(value),
        },
    };

    Ok(js_value)
}

fn tlang_object_to_js_value(
    state: &InterpreterState,
    value: TlangValue,
) -> Result<JsValue, TlangValueSerializeError> {
    let js_value = match state.get_object(value) {
        Some(TlangObjectKind::String(s)) => JsValue::from(s),
        Some(TlangObjectKind::Struct(s)) => {
            if s.shape() == state.heap.builtin_shapes.list {
                let array = js_sys::Array::new();
                for value in s.values() {
                    array.push(&tlang_value_to_js_value(state, *value)?);
                }
                return Ok(JsValue::from(array));
            }

            let shape = state
                .get_shape(s)
                .and_then(|s| s.get_struct_shape())
                .unwrap();
            let object = js_sys::Object::new();
            for (field, idx) in &shape.field_map {
                let key = JsValue::from(field);
                let value = s[*idx];
                js_sys::Reflect::set(&object, &key, &tlang_value_to_js_value(state, value)?)
                    .expect("Unable to set property on object");
            }
            JsValue::from(object)
        }
        Some(TlangObjectKind::Enum(e)) => {
            let shape = state.get_shape(e).and_then(|s| s.get_enum_shape()).unwrap();
            let variant = shape
                .variants
                .iter()
                .enumerate()
                .find(|(variant, _)| *variant == e.variant)
                .map(|(_, variant)| variant)
                .unwrap();
            let object = js_sys::Object::new();
            for (field, idx) in &variant.field_map {
                let key = JsValue::from(field);
                let value = e.field_values[*idx];
                js_sys::Reflect::set(&object, &key, &tlang_value_to_js_value(state, value)?)
                    .expect("Unable to set property on object");
            }
            JsValue::from(object)
        }
        Some(TlangObjectKind::Slice(slice)) => {
            let values = state.get_slice_values(*slice);
            let array = js_sys::Array::new();
            for value in values {
                array.push(&tlang_value_to_js_value(state, *value)?);
            }
            JsValue::from(array)
        }
        Some(TlangObjectKind::Fn(_)) => JsValue::from(js_sys::Object::new()),
        Some(TlangObjectKind::NativeFn) => JsValue::from(js_sys::Object::new()),
        Some(TlangObjectKind::Closure(_)) => JsValue::from(js_sys::Object::new()),
        Some(TlangObjectKind::Cell(cell)) => tlang_value_to_js_value(state, cell.value)?,
        None => {
            return Err(TlangValueSerializeError::new(
                "Unable to convert TlangValue to JsValue: Object not found in heap",
            ));
        }
    };

    Ok(js_value)
}

fn js_value_to_tlang_value(state: &mut InterpreterState, value: &JsValue) -> TlangValue {
    if value.is_null() || value.is_undefined() {
        return TlangValue::Nil;
    }

    if let Some(b) = value.as_bool() {
        return TlangValue::Bool(b);
    }

    if let Some(i) = value.as_f64() {
        return TlangValue::F64(i);
    }

    if let Some(array) = value.dyn_ref::<js_sys::Array>() {
        let mut values = Vec::with_capacity(array.length() as usize);

        for i in 0..array.length() {
            let item = array.get(i);
            values.push(js_value_to_tlang_value(state, &item));
        }

        return state.new_list(values);
    }

    todo!()
}
