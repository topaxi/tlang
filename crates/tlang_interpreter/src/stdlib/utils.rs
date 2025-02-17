use tlang_macros::native_fn;

use crate::state::InterpreterState;
use crate::value::TlangValue;

#[native_fn(name = "log")]
pub fn log(state: &mut InterpreterState, args: &[TlangValue]) -> TlangValue {
    println!(
        "{}",
        args.iter()
            .map(|v| state.stringify(*v))
            .collect::<Vec<_>>()
            .join(" ")
    );
    TlangValue::Nil
}
