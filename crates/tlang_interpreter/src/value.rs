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

pub type TlangObjectId = usize;

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

enum ArithmeticOp {
    Add,
    Sub,
    Mul,
    Div,
    Rem,
    Pow,
}

impl TlangValue {
    pub fn new_object(id: TlangObjectId) -> Self {
        TlangValue::Object(id)
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

    #[inline(always)]
    fn apply_arithmetic_op(self, rhs: TlangValue, op: ArithmeticOp) -> TlangValue {
        match (self, rhs) {
            (TlangValue::Int(lhs), TlangValue::Int(rhs)) => match op {
                ArithmeticOp::Add => TlangValue::Int(lhs + rhs),
                ArithmeticOp::Sub => TlangValue::Int(lhs - rhs),
                ArithmeticOp::Mul => TlangValue::Int(lhs * rhs),
                ArithmeticOp::Div => TlangValue::Float(lhs as f64 / rhs as f64),
                ArithmeticOp::Rem => TlangValue::Int(lhs % rhs),
                ArithmeticOp::Pow if rhs >= 0 => TlangValue::Int(lhs.pow(rhs as u32)),
                ArithmeticOp::Pow => TlangValue::Float((lhs as f64).powf(rhs as f64)),
            },
            (TlangValue::Float(lhs), TlangValue::Float(rhs)) => match op {
                ArithmeticOp::Add => TlangValue::Float(lhs + rhs),
                ArithmeticOp::Sub => TlangValue::Float(lhs - rhs),
                ArithmeticOp::Mul => TlangValue::Float(lhs * rhs),
                ArithmeticOp::Div => TlangValue::Float(lhs / rhs),
                ArithmeticOp::Rem => TlangValue::Float(lhs % rhs),
                ArithmeticOp::Pow => TlangValue::Float(lhs.powf(rhs)),
            },
            (TlangValue::Int(lhs), TlangValue::Float(rhs)) => match op {
                ArithmeticOp::Add => TlangValue::Float(lhs as f64 + rhs),
                ArithmeticOp::Sub => TlangValue::Float(lhs as f64 - rhs),
                ArithmeticOp::Mul => TlangValue::Float(lhs as f64 * rhs),
                ArithmeticOp::Div => TlangValue::Float(lhs as f64 / rhs),
                ArithmeticOp::Rem => TlangValue::Float(lhs as f64 % rhs),
                ArithmeticOp::Pow => TlangValue::Float((lhs as f64).powf(rhs)),
            },
            (TlangValue::Float(lhs), TlangValue::Int(rhs)) => match op {
                ArithmeticOp::Add => TlangValue::Float(lhs + rhs as f64),
                ArithmeticOp::Sub => TlangValue::Float(lhs - rhs as f64),
                ArithmeticOp::Mul => TlangValue::Float(lhs * rhs as f64),
                ArithmeticOp::Div => TlangValue::Float(lhs / rhs as f64),
                ArithmeticOp::Rem => TlangValue::Float(lhs % rhs as f64),
                ArithmeticOp::Pow => TlangValue::Float(lhs.powf(rhs as f64)),
            },
            _ => panic!("Unsupported operation"),
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
            TlangValue::Object(id) => write!(f, "Object({})", id),
        }
    }
}

pub trait TlangArithmetic {
    fn add(self, rhs: Self) -> Self;
    fn sub(self, rhs: Self) -> Self;
    fn mul(self, rhs: Self) -> Self;
    fn div(self, rhs: Self) -> Self;
    fn rem(self, rhs: Self) -> Self;
    fn pow(self, rhs: Self) -> Self;
}

impl TlangArithmetic for TlangValue {
    #[inline(always)]
    fn add(self, rhs: Self) -> Self {
        self.apply_arithmetic_op(rhs, ArithmeticOp::Add)
    }

    #[inline(always)]
    fn sub(self, rhs: Self) -> Self {
        self.apply_arithmetic_op(rhs, ArithmeticOp::Sub)
    }

    #[inline(always)]
    fn mul(self, rhs: Self) -> Self {
        self.apply_arithmetic_op(rhs, ArithmeticOp::Mul)
    }

    #[inline(always)]
    fn div(self, rhs: Self) -> Self {
        self.apply_arithmetic_op(rhs, ArithmeticOp::Div)
    }

    #[inline(always)]
    fn rem(self, rhs: Self) -> Self {
        self.apply_arithmetic_op(rhs, ArithmeticOp::Rem)
    }

    #[inline(always)]
    fn pow(self, rhs: Self) -> Self {
        self.apply_arithmetic_op(rhs, ArithmeticOp::Pow)
    }
}
