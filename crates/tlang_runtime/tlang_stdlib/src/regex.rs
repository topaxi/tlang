use regex::Regex;
use tlang_macros::{define_struct, native_fn};
use tlang_memory::{TlangValue, VMState};

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
    let pattern = if flags.is_empty() {
        source
    } else {
        format!("(?{flags}){source}")
    };
    Regex::new(&pattern).unwrap_or_else(|e| state.panic(format!("Invalid regex: {e}")))
}

pub(crate) fn regex_match(state: &mut VMState, regex_val: TlangValue, haystack: &str) -> bool {
    let re = build_regex(state, regex_val);
    re.is_match(haystack)
}

/// Global `re(parts)` function — constructs a Regex from a tagged string literal.
/// Constructs a `Regex` from a tagged string literal.  The parser desugars
/// `re"prefix{x}suffix"` into `re(["prefix", "suffix"], [x])`, so the function
/// interleaves the parts and stringified values to build the full pattern.
///
/// # Panics
///
/// Panics if `parts` is not a list, `values` is not a list, or any element of
/// `parts` is not a string.
#[native_fn(name = "re")]
pub fn re(state: &mut VMState, parts: TlangValue, values: TlangValue) -> TlangValue {
    let parts_list: Vec<TlangValue> = state
        .get_struct(parts)
        .map(|s| s.values().to_vec())
        .unwrap_or_else(|| state.panic("re(): first argument must be a list".to_string()));
    let values_list: Vec<TlangValue> = state
        .get_struct(values)
        .map(|s| s.values().to_vec())
        .unwrap_or_else(|| state.panic("re(): second argument must be a list".to_string()));

    let mut pattern = String::new();
    for (i, &part) in parts_list.iter().enumerate() {
        let s = state
            .get_object(part)
            .and_then(|o| o.as_str())
            .map(|s| s.to_string())
            .unwrap_or_else(|| state.panic("re(): parts element must be a string".to_string()));
        pattern.push_str(&s);
        if i < values_list.len() {
            pattern.push_str(&state.stringify(values_list[i]));
        }
    }

    state.new_regex(pattern, String::new())
}

define_struct! {
    struct Regex { source, flags }

    impl Regex {
        fn test(this, haystack_val) {
            let haystack = vm
                .get_object(haystack_val)
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| vm.panic("Regex::test expects a string argument".to_string()))
                .to_string();
            let re = build_regex(vm, this);
            TlangValue::Bool(re.is_match(&haystack))
        }

        fn exec(this, haystack_val) {
            let haystack = vm
                .get_object(haystack_val)
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| vm.panic("Regex::exec expects a string argument".to_string()))
                .to_string();
            let re = build_regex(vm, this);
            if let Some(m) = re.find(&haystack) {
                let matched = vm.new_string(m.as_str().to_string());
                vm.new_enum(vm.heap.builtin_shapes.option, crate::option::OPTION_SOME, vec![matched])
            } else {
                vm.new_enum(vm.heap.builtin_shapes.option, crate::option::OPTION_NONE, vec![])
            }
        }

        fn replace_all(this, haystack_val, replacement_val) {
            let haystack = vm
                .get_object(haystack_val)
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| vm.panic("Regex::replace_all expects a string first argument".to_string()))
                .to_string();
            let replacement = vm
                .get_object(replacement_val)
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| vm.panic("Regex::replace_all expects a string second argument".to_string()))
                .to_string();
            let re = build_regex(vm, this);
            vm.new_string(re.replace_all(&haystack, replacement.as_str()).to_string())
        }

        fn replace_first(this, haystack_val, replacement_val) {
            let haystack = vm
                .get_object(haystack_val)
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| vm.panic("Regex::replace_first expects a string first argument".to_string()))
                .to_string();
            let replacement = vm
                .get_object(replacement_val)
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| vm.panic("Regex::replace_first expects a string second argument".to_string()))
                .to_string();
            let re = build_regex(vm, this);
            vm.new_string(re.replace(&haystack, replacement.as_str()).to_string())
        }

        fn flags(this, new_flags) {
            if new_flags.is_nil() {
                let f = get_regex_flags(vm, this);
                return vm.new_string(f);
            }
            let source = get_regex_pattern(vm, this);
            let f = vm
                .get_object(new_flags)
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| vm.panic("Regex::flags expects a string argument".to_string()))
                .to_string();
            vm.new_regex(source, f)
        }
    }

    impl Match for Regex {
        fn matches(this, haystack_val) {
            let haystack = vm
                .get_object(haystack_val)
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| vm.panic("Match::matches expects a string as second argument".to_string()))
                .to_string();
            TlangValue::Bool(regex_match(vm, this, &haystack))
        }
    }
}

#[cfg(test)]
mod tests {
    use tlang_memory::{TlangValue, VMState};

    use super::{REGEX_FIELD_FLAGS, REGEX_FIELD_SOURCE};

    fn setup() -> VMState {
        let mut state = VMState::new();
        state.collect_native_inventory();
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
