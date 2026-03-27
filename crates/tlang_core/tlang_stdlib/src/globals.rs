use tlang_macros::native_fn;
use tlang_memory::{NativeFnReturn, TlangValue, VMState};

fn stringify(state: &mut VMState, values: &[TlangValue]) -> String {
    values
        .iter()
        .map(|v| state.stringify(*v))
        .collect::<Vec<_>>()
        .join(" ")
}

#[native_fn]
pub fn log(state: &mut VMState, args: &[TlangValue]) -> TlangValue {
    println!("{}", stringify(state, args));
    TlangValue::Nil
}

#[native_fn]
pub fn panic(state: &mut VMState, args: &[TlangValue]) -> TlangValue {
    let stringified = stringify(state, args);
    state.panic(format!("Panic!: {stringified}"));
}

#[native_fn]
pub fn compose(state: &mut VMState, f: TlangValue, g: TlangValue) -> TlangValue {
    state.new_native_fn("compose$result", move |state, args| {
        let g_result = state.call(g, args);
        NativeFnReturn::Return(state.call(f, &[g_result]))
    })
}

/// Tagged string interpolation: `f"text {expr} more"` → `f(["text ", " more"], [expr])`
///
/// Each interpolated value is converted to a string via `Display::to_string`.
#[native_fn]
pub fn f(state: &mut VMState, parts: TlangValue, values: TlangValue) -> TlangValue {
    let parts_list: Vec<TlangValue> = state
        .get_struct(parts)
        .map(|s| s.values().to_vec())
        .unwrap_or_else(|| state.panic("f(): first argument must be a list".to_string()));
    let values_list: Vec<TlangValue> = state
        .get_struct(values)
        .map(|s| s.values().to_vec())
        .unwrap_or_else(|| state.panic("f(): second argument must be a list".to_string()));

    let mut result = String::new();
    for (i, &part) in parts_list.iter().enumerate() {
        let s = state
            .get_object(part)
            .and_then(|o| o.as_str())
            .map(|s| s.to_string())
            .unwrap_or_else(|| state.panic("f(): parts element must be a string".to_string()));
        result.push_str(&s);
        if i < values_list.len() {
            result.push_str(&state.stringify(values_list[i]));
        }
    }

    state.new_string(result)
}

#[cfg(test)]
mod tests {
    use tlang_memory::{TlangValue, VMState};

    use super::{log, panic as tlang_panic};

    #[test]
    fn test_log_returns_nil() {
        let mut state = VMState::new();
        let result = log(&mut state, &[TlangValue::I64(42)]);
        assert_eq!(result, TlangValue::Nil);
    }

    #[test]
    fn test_log_with_multiple_args_returns_nil() {
        let mut state = VMState::new();
        let result = log(&mut state, &[TlangValue::I64(1), TlangValue::Bool(true)]);
        assert_eq!(result, TlangValue::Nil);
    }

    #[test]
    fn test_log_with_no_args_returns_nil() {
        let mut state = VMState::new();
        let result = log(&mut state, &[]);
        assert_eq!(result, TlangValue::Nil);
    }

    #[test]
    #[should_panic]
    fn test_panic_aborts() {
        let mut state = VMState::new();
        tlang_panic(&mut state, &[TlangValue::Bool(true)]);
    }
}
