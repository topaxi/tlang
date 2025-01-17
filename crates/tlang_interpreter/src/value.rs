use std::cell::RefCell;
use std::rc::Rc;

use tlang_hir::hir::HirId;

use crate::scope::Scope;
use crate::InterpreterState;

#[derive(Debug)]
pub struct TlangClosure {
    pub id: HirId,
    // Closures hold a reference to the parent scope.
    pub(crate) scope: Rc<RefCell<Scope>>,
}

#[derive(Debug)]
pub struct TlangStruct {
    pub field_values: Vec<TlangValue>,
}

#[derive(Debug, Clone, Copy, Eq, PartialEq, Hash)]
pub struct TlangObjectId(u64);

impl Default for TlangObjectId {
    fn default() -> Self {
        Self::new()
    }
}

impl TlangObjectId {
    pub fn new() -> Self {
        static NEXT_ID: std::sync::atomic::AtomicU64 = std::sync::atomic::AtomicU64::new(0);
        TlangObjectId(NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst))
    }
}

pub type TlangNativeFn = Box<dyn Fn(&InterpreterState, &[TlangValue]) -> TlangValue>;

#[derive(Debug)]
pub enum TlangObjectKind {
    Fn(HirId),
    NativeFn,
    Struct(TlangStruct),
    Closure(TlangClosure),
}

impl TlangObjectKind {
    pub(crate) fn get_struct(&self) -> Option<&TlangStruct> {
        match self {
            TlangObjectKind::Struct(s) => Some(s),
            _ => None,
        }
    }

    pub(crate) fn get_closure(&self) -> Option<&TlangClosure> {
        match self {
            TlangObjectKind::Closure(f) => Some(f),
            _ => None,
        }
    }

    pub(crate) fn get_fn_hir_id(&self) -> Option<HirId> {
        match self {
            TlangObjectKind::Fn(id) => Some(*id),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TlangValue {
    Nil,
    Int(i64),
    Float(f64),
    Bool(bool),
    Object(TlangObjectId),
}

impl TlangValue {
    pub fn new_object() -> Self {
        TlangValue::Object(TlangObjectId::new())
    }

    pub fn get_object_id(&self) -> Option<TlangObjectId> {
        match self {
            TlangValue::Object(id) => Some(*id),
            _ => None,
        }
    }

    pub fn is_truthy(&self) -> bool {
        match self {
            TlangValue::Nil => false,
            TlangValue::Int(i) => *i != 0,
            TlangValue::Float(f) => *f != 0.0,
            TlangValue::Bool(b) => *b,
            TlangValue::Object(_) => true,
        }
    }
}

impl std::fmt::Display for TlangValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TlangValue::Nil => write!(f, "nil"),
            TlangValue::Int(i) => write!(f, "{}", i),
            TlangValue::Float(fl) => write!(f, "{}", fl),
            TlangValue::Bool(b) => write!(f, "{}", b),
            TlangValue::Object(id) => write!(f, "Object({})", id.0),
        }
    }
}
