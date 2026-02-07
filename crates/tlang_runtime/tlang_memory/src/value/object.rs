use std::ops::{Index, IndexMut};

use tlang_span::HirId;

use crate::InterpreterState;
use crate::shape::{ShapeKey, Shaped};

use super::TlangValue;

#[derive(Debug)]
pub struct TlangClosure {
    pub id: HirId,
    // Scope metadata at closure creation time. Used during execution to
    // restore the correct scope context when the closure is called.
    pub scope_stack: Vec<crate::scope::Scope>,
    // Captured cells for mutable upvar bindings.
    // Key: (scope_index, var_index) - where scope_index is the absolute scope index
    // Value: TlangObjectId of a Cell object
    //
    // When a closure captures an upvar, instead of copying the value, we create a
    // Cell object and store references in both the closure and the original memory.
    // This enables mutations to be visible in both places.
    pub captured_cells: std::collections::HashMap<(usize, usize), TlangObjectId>,
    // Captured values from parent scopes, stored contiguously (global + local).
    // Used for GC tracing to find all reachable objects from this closure.
    pub captured_memory: Vec<TlangValue>,
    // Length of global memory at capture time, used to split captured_memory
    // into global and local portions for GC tracing.
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

/// A mutable cell that holds a TlangValue. Used for closure captures
/// to enable mutable bindings that are shared between the closure
/// and its parent scope.
#[derive(Debug)]
pub struct TlangCell {
    pub value: TlangValue,
}

impl TlangCell {
    pub fn new(value: TlangValue) -> Self {
        Self { value }
    }

    pub fn get(&self) -> TlangValue {
        self.value
    }

    pub fn set(&mut self, value: TlangValue) {
        self.value = value;
    }
}

#[derive(Debug)]
pub enum TlangObjectKind {
    Fn(HirId),
    NativeFn,
    String(String),
    Struct(TlangStruct),
    Enum(TlangEnum),
    Slice(TlangSlice),
    Closure(TlangClosure),
    /// A mutable cell for closure captures. When a variable is captured by a
    /// closure, it's wrapped in a cell so mutations are visible to both the
    /// closure and the original scope.
    Cell(TlangCell),
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

    pub(crate) fn is_truthy(&self, state: &InterpreterState) -> bool {
        match self {
            TlangObjectKind::Fn(_) => true,
            TlangObjectKind::NativeFn => true,
            TlangObjectKind::String(s) => !s.is_empty(),
            TlangObjectKind::Struct(s) => !s.is_empty(),
            TlangObjectKind::Slice(s) => !s.is_empty(),
            TlangObjectKind::Closure(_) => true,
            TlangObjectKind::Cell(c) => c.value.is_truthy(state),
            TlangObjectKind::Enum(e) => {
                // Option::None is falsy
                if e.shape() == state.builtin_shapes.option && e.variant == 1 {
                    return false;
                }
                // Result::Err is falsy
                if e.shape() == state.builtin_shapes.result && e.variant == 1 {
                    return false;
                }
                // For enums with field values, all values must be truthy
                if e.field_values.is_empty() {
                    true
                } else {
                    e.field_values.iter().all(|v| v.is_truthy(state))
                }
            }
        }
    }

    /// Get a reference to the cell if this is a Cell variant.
    pub fn get_cell(&self) -> Option<&TlangCell> {
        match self {
            TlangObjectKind::Cell(c) => Some(c),
            _ => None,
        }
    }

    /// Get a mutable reference to the cell if this is a Cell variant.
    pub fn get_cell_mut(&mut self) -> Option<&mut TlangCell> {
        match self {
            TlangObjectKind::Cell(c) => Some(c),
            _ => None,
        }
    }

    /// Returns an iterator over all `TlangValue` references contained in this object.
    ///
    /// This is used for garbage collection tracing to find all reachable objects.
    /// Note: This only yields values directly contained in the object, not transitively.
    /// For closures, this yields all captured values and captured cell object references.
    pub fn referenced_values(&self) -> ReferencedValuesIter<'_> {
        match self {
            TlangObjectKind::Struct(s) => ReferencedValuesIter::Slice(s.values.iter()),
            TlangObjectKind::Enum(e) => ReferencedValuesIter::Slice(e.field_values.iter()),
            TlangObjectKind::Slice(s) => ReferencedValuesIter::Single(Some(s.of)),
            TlangObjectKind::Closure(c) => ReferencedValuesIter::Closure {
                memory_iter: c.captured_memory.iter(),
                cells_iter: c.captured_cells.values(),
            },
            TlangObjectKind::Cell(c) => ReferencedValuesIter::Single(Some(c.value)),
            TlangObjectKind::Fn(_) | TlangObjectKind::NativeFn | TlangObjectKind::String(_) => {
                ReferencedValuesIter::Empty
            }
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
    /// Closure referenced values: both captured memory and captured cell object IDs
    Closure {
        memory_iter: std::slice::Iter<'a, TlangValue>,
        cells_iter: std::collections::hash_map::Values<'a, (usize, usize), TlangObjectId>,
    },
}

impl Iterator for ReferencedValuesIter<'_> {
    type Item = TlangValue;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            ReferencedValuesIter::Empty => None,
            ReferencedValuesIter::Single(opt) => opt.take(),
            ReferencedValuesIter::Slice(iter) => iter.next().copied(),
            ReferencedValuesIter::Closure {
                memory_iter,
                cells_iter,
            } => {
                // First yield all captured memory values
                if let Some(val) = memory_iter.next() {
                    return Some(*val);
                }
                // Then yield captured cell object IDs as Object values
                cells_iter.next().map(|&id| TlangValue::Object(id))
            }
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
            ReferencedValuesIter::Closure {
                memory_iter,
                cells_iter,
            } => {
                let (mem_lower, mem_upper) = memory_iter.size_hint();
                let cells_len = cells_iter.len();
                (mem_lower + cells_len, mem_upper.map(|u| u + cells_len))
            }
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
    fn test_closure_referenced_values() {
        use crate::scope::Scope;
        use tlang_span::HirId;

        // Create a closure with some captured values
        let captured = vec![
            TlangValue::I64(42),
            TlangValue::Object(5), // This is an object reference that GC needs to trace
            TlangValue::Bool(true),
        ];
        let closure = TlangClosure {
            id: HirId::new(1),
            scope_stack: vec![Scope::default()],
            captured_cells: std::collections::HashMap::new(),
            captured_memory: captured.clone(),
            global_memory_len: 1,
        };
        let obj = TlangObjectKind::Closure(closure);

        let refs: Vec<_> = obj.referenced_values().collect();
        assert_eq!(refs.len(), 3);
        assert_eq!(refs, captured);
    }

    #[test]
    fn test_closure_referenced_values_includes_cells() {
        use crate::scope::Scope;
        use std::collections::HashMap;
        use tlang_span::HirId;

        // Create a closure with captured memory and captured cells
        let captured_memory = vec![TlangValue::I64(42)];
        let mut captured_cells = HashMap::new();
        captured_cells.insert((0, 0), 100); // Cell at scope 0, var 0 -> object ID 100
        captured_cells.insert((1, 2), 200); // Cell at scope 1, var 2 -> object ID 200

        let closure = TlangClosure {
            id: HirId::new(1),
            scope_stack: vec![Scope::default()],
            captured_cells,
            captured_memory: captured_memory.clone(),
            global_memory_len: 0,
        };
        let obj = TlangObjectKind::Closure(closure);

        let refs: Vec<_> = obj.referenced_values().collect();
        // Should include: 1 captured memory value + 2 cell object references
        assert_eq!(refs.len(), 3);
        assert!(refs.contains(&TlangValue::I64(42)));
        assert!(refs.contains(&TlangValue::Object(100)));
        assert!(refs.contains(&TlangValue::Object(200)));
    }

    #[test]
    fn test_closure_empty_captured_memory() {
        use crate::scope::Scope;
        use tlang_span::HirId;

        // Closure with no captured values
        let closure = TlangClosure {
            id: HirId::new(1),
            scope_stack: vec![Scope::default()],
            captured_cells: std::collections::HashMap::new(),
            captured_memory: vec![],
            global_memory_len: 0,
        };
        let obj = TlangObjectKind::Closure(closure);

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

    #[test]
    fn test_enum_truthiness_option_some() {
        use crate::InterpreterState;

        let state = InterpreterState::new();
        let option_shape = state.builtin_shapes.option;

        // Option::Some(truthy value) should be truthy
        let some_truthy = TlangEnum::new(option_shape, 0, vec![TlangValue::I64(42)]);
        let obj = TlangObjectKind::Enum(some_truthy);
        assert!(obj.is_truthy(&state), "Option::Some(42) should be truthy");

        // Option::Some(falsy value) should be falsy (value-based truthiness)
        let some_falsy = TlangEnum::new(option_shape, 0, vec![TlangValue::I64(0)]);
        let obj = TlangObjectKind::Enum(some_falsy);
        assert!(!obj.is_truthy(&state), "Option::Some(0) should be falsy");
    }

    #[test]
    fn test_enum_truthiness_option_none() {
        use crate::InterpreterState;

        let state = InterpreterState::new();
        let option_shape = state.builtin_shapes.option;

        // Option::None should be falsy (variant 1 = None)
        let none = TlangEnum::new(option_shape, 1, vec![]);
        let obj = TlangObjectKind::Enum(none);
        assert!(!obj.is_truthy(&state), "Option::None should be falsy");
    }

    #[test]
    fn test_enum_truthiness_result_ok() {
        use crate::InterpreterState;

        let state = InterpreterState::new();
        let result_shape = state.builtin_shapes.result;

        // Result::Ok(truthy value) should be truthy
        let ok_truthy = TlangEnum::new(result_shape, 0, vec![TlangValue::I64(42)]);
        let obj = TlangObjectKind::Enum(ok_truthy);
        assert!(obj.is_truthy(&state), "Result::Ok(42) should be truthy");

        // Result::Ok(falsy value) should be falsy (value-based truthiness)
        let ok_falsy = TlangEnum::new(result_shape, 0, vec![TlangValue::I64(0)]);
        let obj = TlangObjectKind::Enum(ok_falsy);
        assert!(!obj.is_truthy(&state), "Result::Ok(0) should be falsy");
    }

    #[test]
    fn test_enum_truthiness_result_err() {
        use crate::InterpreterState;

        let state = InterpreterState::new();
        let result_shape = state.builtin_shapes.result;

        // Result::Err should be falsy (variant 1 = Err)
        let err = TlangEnum::new(result_shape, 1, vec![TlangValue::I64(42)]);
        let obj = TlangObjectKind::Enum(err);
        assert!(!obj.is_truthy(&state), "Result::Err should be falsy");
    }

    #[test]
    fn test_enum_truthiness_custom_enum() {
        use crate::InterpreterState;

        let state = InterpreterState::new();

        // Custom enum with truthy field values should be truthy
        let custom_truthy = TlangEnum::new(ShapeKey::Native(999), 0, vec![TlangValue::I64(42)]);
        let obj = TlangObjectKind::Enum(custom_truthy);
        assert!(
            obj.is_truthy(&state),
            "Custom enum with truthy value should be truthy"
        );

        // Custom enum with falsy field values should be falsy
        let custom_falsy = TlangEnum::new(ShapeKey::Native(999), 0, vec![TlangValue::I64(0)]);
        let obj = TlangObjectKind::Enum(custom_falsy);
        assert!(
            !obj.is_truthy(&state),
            "Custom enum with falsy value should be falsy"
        );

        // Custom unit variant (no field values) should be truthy
        let custom_unit = TlangEnum::new(ShapeKey::Native(999), 0, vec![]);
        let obj = TlangObjectKind::Enum(custom_unit);
        assert!(
            obj.is_truthy(&state),
            "Custom unit variant should be truthy"
        );
    }

    #[test]
    fn test_cell_basic_operations() {
        // Test cell creation and value access
        let cell = TlangCell::new(TlangValue::I64(42));
        assert_eq!(cell.get(), TlangValue::I64(42));
    }

    #[test]
    fn test_cell_mutation() {
        // Test cell value mutation
        let mut cell = TlangCell::new(TlangValue::I64(0));
        assert_eq!(cell.get(), TlangValue::I64(0));

        cell.set(TlangValue::I64(100));
        assert_eq!(cell.get(), TlangValue::I64(100));
    }

    #[test]
    fn test_cell_referenced_values() {
        // Cell containing an object reference should yield that reference
        let cell = TlangCell::new(TlangValue::Object(42));
        let obj = TlangObjectKind::Cell(cell);

        let refs: Vec<_> = obj.referenced_values().collect();
        assert_eq!(refs.len(), 1);
        assert_eq!(refs[0], TlangValue::Object(42));
    }

    #[test]
    fn test_cell_with_primitive() {
        // Cell containing a primitive should yield that primitive
        let cell = TlangCell::new(TlangValue::Bool(true));
        let obj = TlangObjectKind::Cell(cell);

        let refs: Vec<_> = obj.referenced_values().collect();
        assert_eq!(refs.len(), 1);
        assert_eq!(refs[0], TlangValue::Bool(true));
    }
}
