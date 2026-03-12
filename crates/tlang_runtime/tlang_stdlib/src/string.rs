use tlang_macros::native_fn;
use tlang_memory;
use tlang_memory::{VMState, prelude::*};

#[native_fn]
pub fn from_char_code(state: &mut VMState, code: TlangValue) -> TlangValue {
    let char_code = code.as_usize() as u32;
    if let Some(ch) = std::char::from_u32(char_code) {
        state.new_string(ch.to_string())
    } else {
        state.panic(format!("Invalid char code: {}", char_code))
    }
}

#[cfg(test)]
mod tests {
    use tlang_memory::VMState;

    use super::from_char_code;

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
}
