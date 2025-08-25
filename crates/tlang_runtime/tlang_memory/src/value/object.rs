use std::ops::{Index, IndexMut};

use tlang_span::HirId;

use crate::shape::{ShapeKey, Shaped};

use super::TlangValue;

#[derive(Debug)]
pub struct TlangClosure {
    pub id: HirId,
    // Closures hold a reference to the scope stack at the time of creation.
    pub scope_stack: Vec<crate::scope::Scope>,
}

#[derive(Debug, PartialEq)]
pub struct TlangStruct {
    shape: ShapeKey,
    values: Vec<TlangValue>,
}

impl TlangStruct {
    pub fn new(shape: ShapeKey, values: Vec<TlangValue>) -> Self {
        Self { shape, values }
    }

    pub fn values(&self) -> &[TlangValue] {
        &self.values
    }

    pub fn get(&self, index: usize) -> Option<TlangValue> {
        self.values.get(index).copied()
    }

    #[inline(always)]
    pub const fn is_empty(&self) -> bool {
        self.values.is_empty()
    }

    pub const fn len(&self) -> usize {
        self.values.len()
    }
}

impl Shaped for TlangStruct {
    fn shape(&self) -> ShapeKey {
        self.shape
    }
}

impl Shaped for &TlangStruct {
    fn shape(&self) -> ShapeKey {
        self.shape
    }
}

impl Shaped for &mut TlangStruct {
    fn shape(&self) -> ShapeKey {
        self.shape
    }
}

impl Index<usize> for TlangStruct {
    type Output = TlangValue;

    fn index(&self, index: usize) -> &Self::Output {
        &self.values[index]
    }
}

impl IndexMut<usize> for TlangStruct {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.values[index]
    }
}

#[derive(Debug, PartialEq)]
pub struct TlangEnum {
    shape: ShapeKey,
    pub variant: usize,
    pub field_values: Vec<TlangValue>,
}

impl TlangEnum {
    pub fn new(shape: ShapeKey, variant: usize, values: Vec<TlangValue>) -> Self {
        Self {
            shape,
            variant,
            field_values: values,
        }
    }
}

impl Shaped for TlangEnum {
    fn shape(&self) -> ShapeKey {
        self.shape
    }
}

impl Shaped for &TlangEnum {
    fn shape(&self) -> ShapeKey {
        self.shape
    }
}

#[derive(Debug, Clone, Copy, PartialEq)]
pub struct TlangSlice {
    of: TlangValue,
    start: usize,
    len: usize,
}

impl TlangSlice {
    pub fn new(of: TlangValue, start: usize, len: usize) -> Self {
        Self { of, start, len }
    }

    pub fn of(self) -> TlangValue {
        self.of
    }

    pub fn start(self) -> usize {
        self.start
    }

    pub fn len(self) -> usize {
        self.len
    }

    pub fn range(self) -> std::ops::Range<usize> {
        self.start..self.start + self.len
    }

    pub fn is_empty(self) -> bool {
        self.len == 0
    }
}

pub type TlangObjectId = usize;

#[derive(Debug)]
pub enum TlangObjectKind {
    Fn(HirId),
    NativeFn,
    String(String),
    Struct(TlangStruct),
    Enum(TlangEnum),
    Slice(TlangSlice),
    Closure(TlangClosure),
}

impl TlangObjectKind {
    pub fn get_struct(&self) -> Option<&TlangStruct> {
        match self {
            TlangObjectKind::Struct(s) => Some(s),
            _ => None,
        }
    }

    pub fn get_struct_mut(&mut self) -> Option<&mut TlangStruct> {
        match self {
            TlangObjectKind::Struct(s) => Some(s),
            _ => None,
        }
    }

    pub fn get_enum(&self) -> Option<&TlangEnum> {
        match self {
            TlangObjectKind::Enum(e) => Some(e),
            _ => None,
        }
    }

    pub fn get_slice(&self) -> Option<TlangSlice> {
        match self {
            TlangObjectKind::Slice(s) => Some(*s),
            _ => None,
        }
    }

    pub fn get_str(&self) -> Option<&str> {
        match self {
            TlangObjectKind::String(s) => Some(s),
            _ => None,
        }
    }

    pub fn shape(&self) -> Option<ShapeKey> {
        match self {
            TlangObjectKind::Struct(s) => Some(s.shape()),
            TlangObjectKind::Enum(e) => Some(e.shape()),
            _ => None,
        }
    }

    pub(crate) fn is_truthy(&self) -> bool {
        match self {
            TlangObjectKind::Fn(_) => true,
            TlangObjectKind::NativeFn => true,
            TlangObjectKind::String(s) => !s.is_empty(),
            TlangObjectKind::Struct(s) => !s.is_empty(),
            TlangObjectKind::Slice(s) => !s.is_empty(),
            TlangObjectKind::Closure(_) => true,
            TlangObjectKind::Enum(_) => todo!(),
        }
    }
}
