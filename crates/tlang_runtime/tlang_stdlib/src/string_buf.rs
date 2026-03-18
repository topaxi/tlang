use tlang_macros::{define_struct, native_fn};
use tlang_memory::value::object::TlangObjectKind;
use tlang_memory::{TlangValue, VMState};

/// Field index of the inner mutable string inside a StringBuf struct.
const STRINGBUF_BUF: usize = 0;

fn get_inner(vm: &VMState, this: TlangValue) -> TlangValue {
    vm.heap.get_struct(this).unwrap()[STRINGBUF_BUF]
}

/// Global `StringBuf(initial?)` constructor.
#[native_fn(name = "StringBuf")]
pub fn new_string_buf(vm: &mut VMState, args: &[TlangValue]) -> TlangValue {
    let initial = args
        .first()
        .and_then(|v| vm.get_object(*v))
        .and_then(|o| o.as_str())
        .unwrap_or("")
        .to_string();
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

#[cfg(test)]
mod tests {
    use tlang_memory::VMState;

    fn setup() -> VMState {
        let mut state = VMState::new();
        state.collect_native_inventory();
        state
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
        let buf = state.new_string_buf("hello".to_string());
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
