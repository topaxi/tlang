use std::ops::{Index, IndexMut};

use tlang_span::HirId;

use crate::shape::{ShapeKey, Shaped};

use super::TlangValue;

#[derive(Debug)]
pub struct TlangClosure {
    pub id: HirId,
    // Closures hold a reference to the scope stack at the time of creation.
    pub scope_stack: Vec<crate::scope::Scope>,
    // Captured values from parent scopes, stored contiguously.
    // This enables proper memory management for GC - closures own their captured values
    // instead of keeping the parent scope's memory alive.
    pub captured_memory: Vec<TlangValue>,
    // Length of global memory at capture time, used to split captured_memory
    // when restoring for closure invocation.
    pub global_memory_len: usize,
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

    /// Returns an iterator over all `TlangValue` references contained in this object.
    ///
    /// This is used for garbage collection tracing to find all reachable objects.
    /// Note: This only yields values directly contained in the object, not transitively.
    /// For closures, this currently does not yield captured values as closures store
    /// scope metadata rather than captured values directly.
    pub fn referenced_values(&self) -> ReferencedValuesIter<'_> {
        match self {
            TlangObjectKind::Struct(s) => ReferencedValuesIter::Slice(s.values.iter()),
            TlangObjectKind::Enum(e) => ReferencedValuesIter::Slice(e.field_values.iter()),
            TlangObjectKind::Slice(s) => ReferencedValuesIter::Single(Some(s.of)),
            TlangObjectKind::Fn(_)
            | TlangObjectKind::NativeFn
            | TlangObjectKind::String(_)
            | TlangObjectKind::Closure(_) => ReferencedValuesIter::Empty,
        }
    }
}

/// Iterator over referenced values in a `TlangObjectKind`.
///
/// This is an allocation-free iterator that yields all `TlangValue` references
/// directly contained within an object.
pub enum ReferencedValuesIter<'a> {
    /// No referenced values (for primitives, strings, etc.)
    Empty,
    /// A single referenced value (for slices)
    Single(Option<TlangValue>),
    /// Multiple referenced values (for structs, enums)
    Slice(std::slice::Iter<'a, TlangValue>),
}

impl Iterator for ReferencedValuesIter<'_> {
    type Item = TlangValue;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ReferencedValuesIter::Empty => None,
            ReferencedValuesIter::Single(opt) => opt.take(),
            ReferencedValuesIter::Slice(iter) => iter.next().copied(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            ReferencedValuesIter::Empty => (0, Some(0)),
            ReferencedValuesIter::Single(opt) => {
                let len = usize::from(opt.is_some());
                (len, Some(len))
            }
            ReferencedValuesIter::Slice(iter) => iter.size_hint(),
        }
    }
}

impl ExactSizeIterator for ReferencedValuesIter<'_> {}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::shape::ShapeKey;

    #[test]
    fn test_struct_referenced_values() {
        let values = vec![
            TlangValue::I64(1),
            TlangValue::Object(42),
            TlangValue::Bool(true),
        ];
        let s = TlangStruct::new(ShapeKey::Native(0), values.clone());
        let obj = TlangObjectKind::Struct(s);

        let refs: Vec<_> = obj.referenced_values().collect();
        assert_eq!(refs.len(), 3);
        assert_eq!(refs, values);
    }

    #[test]
    fn test_enum_referenced_values() {
        let values = vec![TlangValue::I64(10), TlangValue::Object(99)];
        let e = TlangEnum::new(ShapeKey::Native(0), 0, values.clone());
        let obj = TlangObjectKind::Enum(e);

        let refs: Vec<_> = obj.referenced_values().collect();
        assert_eq!(refs.len(), 2);
        assert_eq!(refs, values);
    }

    #[test]
    fn test_slice_referenced_values() {
        let underlying = TlangValue::Object(123);
        let slice = TlangSlice::new(underlying, 0, 10);
        let obj = TlangObjectKind::Slice(slice);

        let refs: Vec<_> = obj.referenced_values().collect();
        assert_eq!(refs.len(), 1);
        assert_eq!(refs[0], underlying);
    }

    #[test]
    fn test_string_has_no_referenced_values() {
        let obj = TlangObjectKind::String("hello".to_string());

        let refs: Vec<_> = obj.referenced_values().collect();
        assert!(refs.is_empty());
    }

    #[test]
    fn test_fn_has_no_referenced_values() {
        let obj = TlangObjectKind::Fn(tlang_span::HirId::new(1));

        let refs: Vec<_> = obj.referenced_values().collect();
        assert!(refs.is_empty());
    }

    #[test]
    fn test_native_fn_has_no_referenced_values() {
        let obj = TlangObjectKind::NativeFn;

        let refs: Vec<_> = obj.referenced_values().collect();
        assert!(refs.is_empty());
    }

    #[test]
    fn test_iterator_exact_size() {
        let values = vec![TlangValue::I64(1), TlangValue::I64(2), TlangValue::I64(3)];
        let s = TlangStruct::new(ShapeKey::Native(0), values);
        let obj = TlangObjectKind::Struct(s);

        let iter = obj.referenced_values();
        assert_eq!(iter.len(), 3);

        let slice = TlangSlice::new(TlangValue::Object(1), 0, 5);
        let obj = TlangObjectKind::Slice(slice);
        let iter = obj.referenced_values();
        assert_eq!(iter.len(), 1);

        let obj = TlangObjectKind::String("test".to_string());
        let iter = obj.referenced_values();
        assert_eq!(iter.len(), 0);
    }
}
