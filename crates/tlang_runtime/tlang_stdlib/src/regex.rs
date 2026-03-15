use std::collections::HashMap;

use regex::Regex;
use tlang_memory::{NativeFnReturn, TlangValue, VMState};

/// Field indices for the Regex struct.
pub const REGEX_FIELD_SOURCE: usize = 0;
pub const REGEX_FIELD_FLAGS: usize = 1;

fn get_regex_pattern(state: &VMState, this: TlangValue) -> String {
    let s = state.get_struct(this).expect("expected Regex struct");
    state
        .get_object(s[REGEX_FIELD_SOURCE])
        .and_then(|o| o.as_str())
        .unwrap_or("")
        .to_string()
}

fn get_regex_flags(state: &VMState, this: TlangValue) -> String {
    let s = state.get_struct(this).expect("expected Regex struct");
    state
        .get_object(s[REGEX_FIELD_FLAGS])
        .and_then(|o| o.as_str())
        .unwrap_or("")
        .to_string()
}

fn build_regex(state: &mut VMState, this: TlangValue) -> Regex {
    let source = get_regex_pattern(state, this);
    let flags = get_regex_flags(state, this);

    // Build a flags-prefixed pattern so the regex crate respects them.
    // Supported flags: i (case-insensitive), m (multi-line), s (dot-all), x (verbose).
    let pattern = if flags.is_empty() {
        source
    } else {
        format!("(?{flags}){source}")
    };

    Regex::new(&pattern).unwrap_or_else(|e| state.panic(format!("Invalid regex: {e}")))
}

pub(crate) fn regex_test(state: &mut VMState, regex_val: TlangValue, haystack: &str) -> bool {
    let re = build_regex(state, regex_val);
    re.is_match(haystack)
}

#[allow(clippy::missing_panics_doc, clippy::too_many_lines)]
pub fn define_regex_shape(state: &mut VMState) {
    let mut method_map = HashMap::with_capacity(5);

    method_map.insert(
        "test".to_string(),
        state.new_native_method("Regex::test", |state, this, args| {
            let haystack = state
                .get_object(args[0])
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| state.panic("Regex::test expects a string argument".to_string()))
                .to_string();

            let re = build_regex(state, this);
            NativeFnReturn::Return(TlangValue::Bool(re.is_match(&haystack)))
        }),
    );

    method_map.insert(
        "exec".to_string(),
        state.new_native_method("Regex::exec", |state, this, args| {
            let haystack = state
                .get_object(args[0])
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| state.panic("Regex::exec expects a string argument".to_string()))
                .to_string();

            let re = build_regex(state, this);
            let result = if let Some(m) = re.find(&haystack) {
                let matched = state.new_string(m.as_str().to_string());
                let some_shape = state.heap.builtin_shapes.option;
                state.new_enum(
                    some_shape,
                    crate::option::OPTION_VARIANT_SOME,
                    vec![matched],
                )
            } else {
                state.new_enum(
                    state.heap.builtin_shapes.option,
                    crate::option::OPTION_VARIANT_NONE,
                    vec![],
                )
            };
            NativeFnReturn::Return(result)
        }),
    );

    method_map.insert(
        "replace_all".to_string(),
        state.new_native_method("Regex::replace_all", |state, this, args| {
            let haystack = state
                .get_object(args[0])
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| {
                    state.panic("Regex::replace_all expects a string first argument".to_string())
                })
                .to_string();

            let replacement = state
                .get_object(args[1])
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| {
                    state.panic("Regex::replace_all expects a string second argument".to_string())
                })
                .to_string();

            let re = build_regex(state, this);
            let result = re.replace_all(&haystack, replacement.as_str()).to_string();
            NativeFnReturn::Return(state.new_string(result))
        }),
    );

    method_map.insert(
        "replace_first".to_string(),
        state.new_native_method("Regex::replace_first", |state, this, args| {
            let haystack = state
                .get_object(args[0])
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| {
                    state.panic("Regex::replace_first expects a string first argument".to_string())
                })
                .to_string();

            let replacement = state
                .get_object(args[1])
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| {
                    state.panic("Regex::replace_first expects a string second argument".to_string())
                })
                .to_string();

            let re = build_regex(state, this);
            let result = re.replace(&haystack, replacement.as_str()).to_string();
            NativeFnReturn::Return(state.new_string(result))
        }),
    );

    method_map.insert(
        "flags".to_string(),
        state.new_native_method("Regex::flags", |state, this, args| {
            if args.is_empty() {
                let flags = get_regex_flags(state, this);
                return NativeFnReturn::Return(state.new_string(flags));
            }
            let source = get_regex_pattern(state, this);
            let flags = state
                .get_object(args[0])
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| {
                    state.panic("Regex::flags expects a string argument".to_string())
                })
                .to_string();
            NativeFnReturn::Return(state.new_regex(source, flags))
        }),
    );

    state
        .heap
        .builtin_shapes
        .get_regex_shape_mut()
        .set_methods(method_map);
}

#[cfg(test)]
mod tests {
    use tlang_memory::{TlangValue, VMState};

    use super::{REGEX_FIELD_FLAGS, REGEX_FIELD_SOURCE, define_regex_shape};

    fn setup() -> VMState {
        let mut state = VMState::new();
        crate::option::define_option_shape(&mut state);
        define_regex_shape(&mut state);
        state
    }

    fn regex(state: &mut VMState, source: &str, flags: &str) -> TlangValue {
        state.new_regex(source.to_string(), flags.to_string())
    }

    fn call_test(state: &mut VMState, re: TlangValue, haystack: &str) -> bool {
        let _shape_key = state.heap.builtin_shapes.regex;
        let method = state
            .heap
            .builtin_shapes
            .get_regex_shape()
            .get_method("test")
            .cloned()
            .expect("test method");
        let hay = state.new_string(haystack.to_string());
        match method {
            tlang_memory::shape::TlangStructMethod::Native(id) => {
                let result = state
                    .call_native_fn(id, &[re, hay])
                    .expect("native fn result");
                match result {
                    tlang_memory::NativeFnReturn::Return(v) => matches!(v, TlangValue::Bool(true)),
                    _ => false,
                }
            }
            _ => panic!("expected native method"),
        }
    }

    #[test]
    fn test_basic_match() {
        let mut state = setup();
        let re = regex(&mut state, "foo", "");
        assert!(call_test(&mut state, re, "foobar"));
        assert!(!call_test(&mut state, re, "barbaz"));
    }

    #[test]
    fn test_case_insensitive_flag() {
        let mut state = setup();
        let re = regex(&mut state, "FOO", "i");
        assert!(call_test(&mut state, re, "foobar"));
    }

    #[test]
    fn test_source_field() {
        let mut state = setup();
        let re = regex(&mut state, "hello", "");
        let s = state.get_struct(re).unwrap();
        let source = state
            .get_object(s[REGEX_FIELD_SOURCE])
            .and_then(|o| o.as_str())
            .unwrap()
            .to_string();
        assert_eq!(source, "hello");
    }

    #[test]
    fn test_flags_field() {
        let mut state = setup();
        let re = regex(&mut state, "hello", "i");
        let s = state.get_struct(re).unwrap();
        let flags = state
            .get_object(s[REGEX_FIELD_FLAGS])
            .and_then(|o| o.as_str())
            .unwrap()
            .to_string();
        assert_eq!(flags, "i");
    }

    #[test]
    fn test_stringify() {
        let mut state = setup();
        let re = regex(&mut state, "foo|bar", "i");
        assert_eq!(state.stringify(re), "/foo|bar/i");
    }

    fn call_method_str(state: &mut VMState, re: TlangValue, method: &str, arg: &str) -> TlangValue {
        let m = state
            .heap
            .builtin_shapes
            .get_regex_shape()
            .get_method(method)
            .cloned()
            .unwrap_or_else(|| panic!("method {method} not found"));
        let arg_val = state.new_string(arg.to_string());
        match m {
            tlang_memory::shape::TlangStructMethod::Native(id) => state
                .call_native_fn(id, &[re, arg_val])
                .and_then(|r| match r {
                    tlang_memory::NativeFnReturn::Return(v) => Some(v),
                    _ => None,
                })
                .expect("return value"),
            _ => panic!("expected native method"),
        }
    }

    fn flags_of(state: &VMState, re: TlangValue) -> String {
        let s = state.get_struct(re).unwrap();
        state
            .get_object(s[REGEX_FIELD_FLAGS])
            .and_then(|o| o.as_str())
            .unwrap_or("")
            .to_string()
    }

    #[test]
    fn test_flags_method() {
        let mut state = setup();
        let re = regex(&mut state, "foo", "");
        let re2 = call_method_str(&mut state, re, "flags", "im");
        assert_eq!(flags_of(&state, re2), "im");
    }
}
