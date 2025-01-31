use std::cell::RefCell;
use std::collections::HashMap;
use std::hash::DefaultHasher;
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

#[derive(Debug, PartialEq)]
pub struct TlangStruct {
    pub shape: ShapeKey,
    pub field_values: Vec<TlangValue>,
}

impl TlangStruct {
    #[inline(always)]
    pub fn is_empty(&self) -> bool {
        self.field_values.is_empty()
    }
}

// ShapeKeyDict is a hash value generated from each of the keys in the struct.
type ShapeKeyDict = u64;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
enum ShapeKeyImpl {
    HirId(HirId),
    Native(usize),
    Dict(ShapeKeyDict),
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ShapeKey(ShapeKeyImpl);

impl ShapeKey {
    pub fn new_native() -> Self {
        static NEXT_ID: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
        ShapeKey(ShapeKeyImpl::Native(
            NEXT_ID.fetch_add(1, std::sync::atomic::Ordering::SeqCst),
        ))
    }

    pub fn new_hir_id(id: HirId) -> Self {
        ShapeKey(ShapeKeyImpl::HirId(id))
    }

    pub fn from_dict_keys(keys: &[String]) -> Self {
        let mut hasher = DefaultHasher::new();

        for key in keys.iter() {
            std::hash::Hash::hash(&key, &mut hasher);
        }

        let hash = std::hash::Hasher::finish(&hasher);

        ShapeKey(ShapeKeyImpl::Dict(hash))
    }
}

impl From<HirId> for ShapeKey {
    fn from(id: HirId) -> Self {
        ShapeKey::new_hir_id(id)
    }
}

#[derive(Debug, Clone, Copy)]
pub enum TlangStructMethod {
    Native(TlangObjectId),
    HirId(HirId),
}

pub struct TlangStructShape {
    pub name: String,
    pub field_map: HashMap<String, usize>,
    pub method_map: HashMap<String, TlangStructMethod>,
}

impl TlangStructShape {
    pub fn new(
        name: String,
        fields: Vec<String>,
        methods: HashMap<String, TlangStructMethod>,
    ) -> Self {
        let field_map = fields
            .into_iter()
            .enumerate()
            .map(|(i, f)| (f, i))
            .collect();

        Self {
            name,
            field_map,
            method_map: methods,
        }
    }
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

pub type TlangNativeFn = Box<dyn FnMut(&mut InterpreterState, &[TlangValue]) -> NativeFnReturn>;

#[derive(Debug)]
pub enum NativeFnReturn {
    Return(TlangValue),
    DynamicCall(HirId),
    PartialCall(TlangValue, Vec<TlangValue>),
}

#[derive(Debug)]
pub enum TlangObjectKind {
    Fn(HirId),
    NativeFn,
    String(String),
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

    pub(crate) fn get_shape_key(&self) -> Option<ShapeKey> {
        self.get_struct().map(|s| s.shape)
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

    pub(crate) fn get_str(&self) -> Option<&str> {
        match self {
            TlangObjectKind::String(s) => Some(s),
            _ => None,
        }
    }

    fn is_truthy(&self) -> bool {
        match self {
            TlangObjectKind::Fn(_) => true,
            TlangObjectKind::NativeFn => true,
            TlangObjectKind::String(s) => !s.is_empty(),
            TlangObjectKind::Struct(s) => !s.is_empty(),
            TlangObjectKind::Closure(_) => true,
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

impl std::cmp::Eq for TlangValue {}

impl std::hash::Hash for TlangValue {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        match self {
            TlangValue::Nil => 0.hash(state),
            TlangValue::Int(i) => i.hash(state),
            TlangValue::Float(f) => f.to_bits().hash(state), // do we want to support this?
            TlangValue::Bool(b) => b.hash(state),
            TlangValue::Object(id) => id.hash(state),
        }
    }
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

    pub fn is_object(&self) -> bool {
        matches!(self, TlangValue::Object(_))
    }

    pub fn is_truthy(&self, state: &InterpreterState) -> bool {
        match self {
            TlangValue::Nil => false,
            TlangValue::Int(i) => *i != 0,
            TlangValue::Float(f) => *f != 0.0,
            TlangValue::Bool(b) => *b,
            TlangValue::Object(id) => state
                .get_object_by_id(*id)
                .is_some_and(|kind| kind.is_truthy()),
        }
    }

    pub fn as_usize(&self) -> Option<usize> {
        match self {
            TlangValue::Int(i) => Some(*i as usize),
            TlangValue::Float(f) => Some(*f as usize),
            _ => None,
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
