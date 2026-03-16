use std::rc::Rc;

use smallvec::SmallVec;

use tlang_span::HirId;

use crate::VMState;

use super::TlangValue;

pub type TlangNativeFn = Rc<dyn Fn(&mut VMState, &[TlangValue]) -> NativeFnReturn>;

#[derive(Debug)]
pub enum NativeFnReturn {
    Return(TlangValue),
    DynamicCall(HirId),
    CallObject(Box<(TlangValue, SmallVec<[TlangValue; 4]>)>),
}

impl NativeFnReturn {
    pub fn value(&self) -> Option<&TlangValue> {
        match self {
            NativeFnReturn::Return(value) => Some(value),
            _ => None,
        }
    }
}

impl From<TlangValue> for NativeFnReturn {
    fn from(value: TlangValue) -> Self {
        NativeFnReturn::Return(value)
    }
}
