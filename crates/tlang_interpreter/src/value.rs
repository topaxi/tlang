use std::cell::RefCell;
use std::rc::Rc;

use tlang_hir::hir::HirId;

use crate::scope::Scope;

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

#[derive(Debug)]
pub enum TlangObjectKind {
    Struct(TlangStruct),
    Fn(TlangClosure),
}

impl TlangObjectKind {
    pub(crate) fn get_struct(&self) -> Option<&TlangStruct> {
        match self {
            TlangObjectKind::Struct(s) => Some(s),
            _ => None,
        }
    }

    pub(crate) fn get_fn(&self) -> Option<&TlangClosure> {
        match self {
            TlangObjectKind::Fn(f) => Some(f),
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
}
