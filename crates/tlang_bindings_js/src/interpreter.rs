use tlang_hir::hir;
use tlang_memory::value::NativeFnReturn;
use tlang_memory::{InterpreterState, prelude::*};
use wasm_bindgen::prelude::*;

pub struct TlangInterpreter(tlang_interpreter::Interpreter);

impl Default for TlangInterpreter {
    fn default() -> Self {
        Self::new()
    }
}

impl TlangInterpreter {
    pub(crate) fn new() -> Self {
        let interpreter = tlang_interpreter::Interpreter::default();

        TlangInterpreter(interpreter)
    }

    pub fn define_js_fn(&mut self, name: &str, f: js_sys::Function) {
        self.0.define_native_fn(name, move |state, args| {
            let this = JsValue::null();

            let value = match args.len() {
                0 => {
                    let result = f.call0(&this);
                    match result {
                        Ok(value) => js_value_to_tlang_value(state, value),
                        Err(err) => {
                            state.panic(format!("Error calling JavaScript function: {:?}", err))
                        }
                    }
                }
                1 => {
                    let arg = tlang_value_to_js_value(state, args[0]);
                    let result = f.call1(&this, &arg);
                    match result {
                        Ok(value) => js_value_to_tlang_value(state, value),
                        Err(err) => {
                            state.panic(format!("Error calling JavaScript function: {:?}", err))
                        }
                    }
                }
                2 => {
                    let arg1 = tlang_value_to_js_value(state, args[0]);
                    let arg2 = tlang_value_to_js_value(state, args[1]);
                    let result = f.call2(&this, &arg1, &arg2);
                    match result {
                        Ok(value) => js_value_to_tlang_value(state, value),
                        Err(err) => {
                            state.panic(format!("Error calling JavaScript function: {:?}", err))
                        }
                    }
                }
                3 => {
                    let arg1 = tlang_value_to_js_value(state, args[0]);
                    let arg2 = tlang_value_to_js_value(state, args[1]);
                    let arg3 = tlang_value_to_js_value(state, args[2]);
                    let result = f.call3(&this, &arg1, &arg2, &arg3);
                    match result {
                        Ok(value) => js_value_to_tlang_value(state, value),
                        Err(err) => {
                            state.panic(format!("Error calling JavaScript function: {:?}", err))
                        }
                    }
                }
                _ => {
                    let js_args = js_sys::Array::new();
                    for arg in args {
                        js_args.push(&tlang_value_to_js_value(state, *arg));
                    }
                    let result = f.apply(&this, &js_args);
                    match result {
                        Ok(value) => js_value_to_tlang_value(state, value),
                        Err(err) => {
                            state.panic(format!("Error calling JavaScript function: {:?}", err))
                        }
                    }
                }
            };

            NativeFnReturn::Return(value)
        });
    }

    pub(crate) fn eval(&mut self, hir: &hir::Module) -> JsValue {
        let value = self.0.eval(hir);
        tlang_value_to_js_value(self.0.state(), value)
    }
}

fn tlang_value_to_js_value(state: &InterpreterState, value: TlangValue) -> JsValue {
    match value {
        TlangValue::Nil => JsValue::null(),
        TlangValue::Object(_) => tlang_object_to_js_value(state, value),
        value => match value.as_primitive() {
            TlangPrimitive::Nil => JsValue::null(),
            TlangPrimitive::Bool(value) => JsValue::from(value),
            TlangPrimitive::Int(value) => JsValue::from(value),
            TlangPrimitive::UInt(value) => JsValue::from(value),
            TlangPrimitive::Float(value) => JsValue::from(value),
        },
    }
}

fn tlang_object_to_js_value(state: &InterpreterState, value: TlangValue) -> JsValue {
    match state.get_object(value) {
        Some(TlangObjectKind::String(s)) => JsValue::from(s),
        Some(TlangObjectKind::Struct(s)) => {
            if s.shape == state.builtin_shapes.list {
                let array = js_sys::Array::new();
                for value in &s.field_values {
                    array.push(&tlang_value_to_js_value(state, *value));
                }
                return JsValue::from(array);
            }

            let shape = state.get_shape(s.shape).unwrap();
            let object = js_sys::Object::new();
            for (field, idx) in &shape.field_map {
                let key = JsValue::from(field);
                let value = s.field_values[*idx];
                js_sys::Reflect::set(&object, &key, &tlang_value_to_js_value(state, value))
                    .expect("Unable to set property on object");
            }
            JsValue::from(object)
        }
        _ => JsValue::from("unknown object"),
    }
}

fn js_value_to_tlang_value(state: &mut InterpreterState, value: JsValue) -> TlangValue {
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
            values.push(js_value_to_tlang_value(state, item));
        }

        return state.new_list(values);
    }

    todo!()
}
