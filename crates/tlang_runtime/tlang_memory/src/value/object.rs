use std::ops::{Index, IndexMut};

use tlang_span::HirId;

use crate::shape::{ShapeKey, Shaped};

use super::TlangValue;

/// Represents a closure with optimized memory capture.
/// 
/// # Memory Capture Strategy
/// 
/// Closures capture the memory state at creation time to preserve variable values.
/// However, the current implementation has different behavior for global vs local variables:
/// 
/// - **Global variables**: Shared between closures (mutations visible across closures)
/// - **Local variables**: Copied per closure (mutations NOT visible across closures) 
/// 
/// # Performance Optimizations
/// 
/// The `CapturedMemory` enum provides optimized storage:
/// - `None`: No allocations for closures that only use globals/parameters
/// - `Small`: Inline storage for small captures (≤8 values)  
/// - `Large`: Heap allocation for larger captures
#[derive(Debug, Clone)]
pub struct TlangClosure {
    pub id: HirId,
    // Closures hold a reference to the scope stack at the time of creation.
    pub scope_stack: Vec<crate::scope::Scope>,
    // Track captured memory with optimization for common cases
    pub captured_memory: CapturedMemory,
}

#[derive(Debug, Clone)]
pub enum CapturedMemory {
    // No local memory captured - closure only uses globals/parameters (most common case)
    None,
    // Small memory captured - inline storage for performance
    Small(Vec<TlangValue>),
    // Large memory captured - separate storage
    Large(Vec<TlangValue>),
}

impl CapturedMemory {
    pub fn new(memory: Vec<TlangValue>) -> Self {
        if memory.is_empty() {
            CapturedMemory::None
        } else if memory.len() <= 8 {
            // Small optimization - inline small captures
            CapturedMemory::Small(memory)
        } else {
            CapturedMemory::Large(memory)
        }
    }
    
    pub fn as_vec(&self) -> Vec<TlangValue> {
        match self {
            CapturedMemory::None => Vec::new(),
            CapturedMemory::Small(vec) | CapturedMemory::Large(vec) => vec.clone(),
        }
    }
    
    pub fn is_empty(&self) -> bool {
        matches!(self, CapturedMemory::None)
    }
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
