use smallvec::SmallVec;

use tlang_span::HirId;

use crate::InterpreterState;

use super::TlangValue;

pub type TlangNativeFn = Box<dyn FnMut(&mut InterpreterState, &[TlangValue]) -> NativeFnReturn>;

#[derive(Debug)]
pub enum NativeFnReturn {
    Return(TlangValue),
    DynamicCall(HirId),
    CallObject(Box<(TlangValue, SmallVec<[TlangValue; 4]>)>),
}
