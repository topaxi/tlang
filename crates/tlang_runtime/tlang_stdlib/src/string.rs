use tlang_macros::{define_struct, native_fn};
use tlang_memory;
use tlang_memory::value::object::TlangObjectKind;
use tlang_memory::{TlangValue, VMState};

// ── StringBuf ──────────────────────────────────────────────────────────────

/// Field index of the inner mutable string inside a StringBuf struct.
const STRINGBUF_BUF: usize = 0;

fn get_inner(vm: &VMState, this: TlangValue) -> TlangValue {
    vm.heap.get_struct(this).unwrap()[STRINGBUF_BUF]
}

/// Global `string::StringBuf(initial?)` constructor.
#[native_fn(name = "string::StringBuf")]
pub fn new_string_buf(vm: &mut VMState, args: &[TlangValue]) -> TlangValue {
    let initial = match args.len() {
        0 => String::new(),
        1 => vm
            .get_object(args[0])
            .and_then(|o| o.as_str())
            .unwrap_or_else(|| vm.panic("string::StringBuf expects a string argument".to_string()))
            .to_string(),
        _ => vm.panic("string::StringBuf expects 0 or 1 argument".to_string()),
    };

    vm.new_string_buf(initial)
}

define_struct! {
    struct StringBuf {}

    impl StringBuf {
        fn push(this, s_val) {
            let inner = get_inner(vm, this);
            let s = vm
                .get_object(s_val)
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| vm.panic("StringBuf::push expects a string argument".to_string()))
                .to_string();
            if let Some(TlangObjectKind::String(buf)) = vm.heap.get_object_mut(inner) {
                buf.push_str(&s);
            }
            this
        }

        fn push_char(this, c_val) {
            let inner = get_inner(vm, this);
            let s = vm
                .get_object(c_val)
                .and_then(|o| o.as_str())
                .unwrap_or_else(|| vm.panic("StringBuf::push_char expects a string argument".to_string()))
                .to_string();
            let ch = s
                .chars()
                .next()
                .unwrap_or_else(|| vm.panic("StringBuf::push_char: empty string".to_string()));
            if let Some(TlangObjectKind::String(buf)) = vm.heap.get_object_mut(inner) {
                buf.push(ch);
            }
            this
        }

        fn clear(this) {
            let inner = get_inner(vm, this);
            if let Some(TlangObjectKind::String(buf)) = vm.heap.get_object_mut(inner) {
                buf.clear();
            }
            this
        }

        fn to_string(this) {
            let inner = get_inner(vm, this);
            let s = vm
                .get_object(inner)
                .and_then(|o| o.as_str())
                .unwrap_or("")
                .to_string();
            vm.new_string(s)
        }

        fn len(this) {
            let inner = get_inner(vm, this);
            let n = vm
                .get_object(inner)
                .and_then(|o| o.as_str())
                .map(|s| s.len())
                .unwrap_or(0);
            TlangValue::from(n)
        }

        fn is_empty(this) {
            let inner = get_inner(vm, this);
            let empty = vm
                .get_object(inner)
                .and_then(|o| o.as_str())
                .map(|s| s.is_empty())
                .unwrap_or(true);
            TlangValue::Bool(empty)
        }
    }
}

// ── string functions ────────────────────────────────────────────────────────

#[native_fn]
pub fn from_char_code(state: &mut VMState, code: TlangValue) -> TlangValue {
    let char_code = code.as_usize() as u32;
    if let Some(ch) = std::char::from_u32(char_code) {
        state.new_string(ch.to_string())
    } else {
        state.panic(format!("Invalid char code: {}", char_code))
    }
}

#[native_fn]
pub fn char_code_at(state: &mut VMState, string: TlangValue, index: TlangValue) -> TlangValue {
    if let Some(ch) = state
        .get_object(string)
        .and_then(|o| o.as_str())
        .and_then(|string| string.chars().nth(index.as_usize()))
    {
        TlangValue::U32(ch as u64)
    } else {
        state.panic(format!("Index out of bounds: {}", index))
    }
}

#[cfg(test)]
mod tests {
    use tlang_memory::VMState;

    use super::{from_char_code, new_string_buf};

    fn setup() -> VMState {
        let mut state = VMState::new();
        state.collect_native_inventory();
        state
    }

    fn stringify(state: &mut VMState, value: tlang_memory::TlangValue) -> String {
        state.stringify(value)
    }

    #[test]
    fn test_from_char_code_uppercase_a() {
        let mut state = VMState::new();
        let result = from_char_code(&mut state, tlang_memory::TlangValue::I64(65));
        assert_eq!(stringify(&mut state, result), "A");
    }

    #[test]
    fn test_from_char_code_lowercase_z() {
        let mut state = VMState::new();
        let result = from_char_code(&mut state, tlang_memory::TlangValue::I64(122));
        assert_eq!(stringify(&mut state, result), "z");
    }

    #[test]
    fn test_from_char_code_digit() {
        let mut state = VMState::new();
        let result = from_char_code(&mut state, tlang_memory::TlangValue::I64(48));
        assert_eq!(stringify(&mut state, result), "0");
    }

    #[test]
    fn test_from_char_code_space() {
        let mut state = VMState::new();
        let result = from_char_code(&mut state, tlang_memory::TlangValue::I64(32));
        assert_eq!(stringify(&mut state, result), " ");
    }

    #[test]
    fn test_from_char_code_newline() {
        let mut state = VMState::new();
        let result = from_char_code(&mut state, tlang_memory::TlangValue::I64(10));
        assert_eq!(stringify(&mut state, result), "\n");
    }

    #[test]
    fn test_new_string_buf_empty() {
        let mut state = setup();
        let buf = state.new_string_buf(String::new());
        assert_eq!(state.get_string_buf(buf), Some(""));
    }

    #[test]
    fn test_new_string_buf_with_initial() {
        let mut state = setup();
        let initial = state.new_string("hello".to_string());
        let buf = new_string_buf(&mut state, &[initial]);
        assert_eq!(state.get_string_buf(buf), Some("hello"));
    }

    #[test]
    fn test_stringify_string_buf() {
        let mut state = setup();
        let buf = state.new_string_buf("world".to_string());
        assert_eq!(state.stringify(buf), "world");
    }

    #[test]
    fn test_is_truthy_non_empty() {
        let mut state = setup();
        let buf = state.new_string_buf("hi".to_string());
        assert!(state.is_truthy(buf));
    }

    #[test]
    fn test_is_truthy_empty() {
        let mut state = setup();
        let buf = state.new_string_buf(String::new());
        assert!(!state.is_truthy(buf));
    }
}
