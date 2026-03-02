use std::collections::{HashMap, HashSet};

use slab::Slab;

use crate::shape::builtin::BuiltinShapes;
use crate::shape::{ShapeKey, Shaped, TlangShape, TlangStructMethod};
use crate::value::{
    TlangValue,
    function::TlangNativeFn,
    object::{TlangEnum, TlangObjectId, TlangObjectKind, TlangSlice, TlangStruct},
};

// ─── NativeFnMeta ─────────────────────────────────────────────────────────────

#[derive(Debug)]
pub struct NativeFnMeta {
    pub name: String,
}

// ─── MemoryStats ──────────────────────────────────────────────────────────────

/// Statistics about memory usage in the interpreter.
///
/// This struct tracks object allocations and deallocations to help
/// monitor memory usage and debug potential memory leaks.
#[derive(Debug, Default, Clone, Copy)]
pub struct MemoryStats {
    /// Total number of objects allocated since interpreter creation.
    pub objects_allocated: usize,
    /// Total number of objects deallocated (via GC or explicit removal).
    pub objects_deallocated: usize,
    /// Number of garbage collection cycles performed.
    pub gc_collections: usize,
}

impl MemoryStats {
    /// Returns the current number of live objects (allocated - deallocated).
    pub fn live_objects(&self) -> usize {
        self.objects_allocated
            .saturating_sub(self.objects_deallocated)
    }
}

// ─── Heap ─────────────────────────────────────────────────────────────────────

/// GC-managed object store together with shapes and native-function registries.
pub struct Heap {
    pub(crate) objects: Slab<TlangObjectKind>,
    shapes: HashMap<ShapeKey, TlangShape>,
    pub builtin_shapes: BuiltinShapes,
    pub(crate) native_fns: HashMap<TlangObjectId, TlangNativeFn>,
    pub(crate) native_fns_meta: HashMap<TlangObjectId, NativeFnMeta>,
    temp_roots: Vec<TlangValue>,
    pub(crate) next_gc_threshold: usize,
    stress_gc: bool,
    pub(crate) memory_stats: MemoryStats,
}

impl Heap {
    pub(crate) const GC_INITIAL_THRESHOLD: usize = 1000;

    pub(crate) fn new() -> Self {
        Self {
            objects: Slab::with_capacity(1000),
            shapes: HashMap::with_capacity(100),
            builtin_shapes: BuiltinShapes::default(),
            native_fns: HashMap::with_capacity(100),
            native_fns_meta: HashMap::with_capacity(100),
            temp_roots: Vec::with_capacity(10),
            next_gc_threshold: Self::GC_INITIAL_THRESHOLD,
            stress_gc: false,
            memory_stats: MemoryStats::default(),
        }
    }

    pub fn temp_roots_mark(&self) -> usize {
        self.temp_roots.len()
    }

    pub fn push_temp_root(&mut self, value: TlangValue) {
        if value.is_object() {
            self.temp_roots.push(value);
        }
    }

    pub fn temp_roots_restore(&mut self, mark: usize) {
        self.temp_roots.truncate(mark);
    }

    pub fn set_stress_gc(&mut self, stress: bool) {
        self.stress_gc = stress;
    }

    pub fn should_collect(&self) -> bool {
        self.stress_gc || self.objects.len() >= self.next_gc_threshold
    }

    /// Low-level object allocator. Callers are responsible for triggering GC beforehand
    /// when needed. Prefer [`crate::InterpreterState::new_object`] for automatic GC scheduling.
    pub fn alloc_object(&mut self, kind: TlangObjectKind) -> TlangValue {
        self.memory_stats.objects_allocated += 1;
        TlangValue::new_object(self.objects.insert(kind))
    }

    /// Mark all objects reachable from the provided roots.
    /// Returns a set of reachable object IDs.
    pub fn mark_reachable(
        &self,
        roots: impl Iterator<Item = TlangValue>,
    ) -> HashSet<TlangObjectId> {
        let mut reachable = HashSet::new();
        let mut values = roots.collect::<Vec<_>>();

        while let Some(value) = values.pop() {
            if let TlangValue::Object(id) = value {
                if !reachable.insert(id) {
                    continue;
                }

                if let Some(obj) = self.objects.get(id) {
                    values.extend(obj.referenced_values());
                }
            }
        }

        reachable
    }

    /// Remove all objects not in the reachable set.
    /// Returns the number of objects collected.
    pub fn sweep_unreachable(&mut self, reachable: &HashSet<TlangObjectId>) -> usize {
        let garbage = self
            .objects
            .iter()
            .map(|(id, _)| id)
            .filter(|id| !reachable.contains(id))
            .collect::<Vec<_>>();

        let count = garbage.len();

        for id in garbage {
            if let Some(TlangObjectKind::NativeFn) = self.remove_object(id) {
                self.native_fns.remove(&id);
                self.native_fns_meta.remove(&id);
            }
        }

        count
    }

    /// Returns the current memory statistics.
    pub fn memory_stats(&self) -> &MemoryStats {
        &self.memory_stats
    }

    /// Returns the number of currently allocated objects in the object store.
    pub fn object_count(&self) -> usize {
        self.objects.len()
    }

    /// Removes an object from the object store by its ID.
    pub fn remove_object(&mut self, id: TlangObjectId) -> Option<TlangObjectKind> {
        let removed = self.objects.try_remove(id);
        if removed.is_some() {
            self.memory_stats.objects_deallocated += 1;
        }
        removed
    }

    /// Checks if an object with the given ID exists in the object store.
    pub fn contains_object(&self, id: TlangObjectId) -> bool {
        self.objects.contains(id)
    }

    #[inline(always)]
    pub fn get_object_by_id_mut(&mut self, id: TlangObjectId) -> Option<&mut TlangObjectKind> {
        self.objects.get_mut(id)
    }

    #[inline(always)]
    pub fn get_object_by_id(&self, id: TlangObjectId) -> Option<&TlangObjectKind> {
        self.objects.get(id)
    }

    pub fn get_object(&self, value: TlangValue) -> Option<&TlangObjectKind> {
        value
            .get_object_id()
            .and_then(|id| self.get_object_by_id(id))
    }

    pub fn get_object_mut(&mut self, value: TlangValue) -> Option<&mut TlangObjectKind> {
        value
            .get_object_id()
            .and_then(|id| self.get_object_by_id_mut(id))
    }

    pub fn get_struct(&self, value: TlangValue) -> Option<&TlangStruct> {
        self.get_object(value).and_then(|obj| obj.get_struct())
    }

    pub fn get_struct_mut(&mut self, value: TlangValue) -> Option<&mut TlangStruct> {
        self.get_object_mut(value)
            .and_then(|obj| obj.get_struct_mut())
    }

    pub fn get_enum(&self, value: TlangValue) -> Option<&TlangEnum> {
        self.get_object(value).and_then(|obj| obj.get_enum())
    }

    pub fn get_slice(&self, value: TlangValue) -> Option<TlangSlice> {
        self.get_object(value).and_then(|obj| obj.get_slice())
    }

    /// # Panics
    pub fn get_slice_value(&self, slice: TlangSlice, index: usize) -> TlangValue {
        let list = self.get_struct(slice.of()).unwrap();
        list.get(slice.start() + index).unwrap()
    }

    /// # Panics
    pub fn get_slice_values(&self, slice: TlangSlice) -> &[TlangValue] {
        let list = self.get_struct(slice.of()).unwrap();
        &list.values()[slice.range()]
    }

    pub fn has_shape(&self, key: ShapeKey) -> bool {
        self.shapes.contains_key(&key)
    }

    pub fn set_shape(&mut self, key: ShapeKey, shape: TlangShape) {
        self.shapes.insert(key, shape);
    }

    #[inline(always)]
    pub fn get_shape<T>(&self, shaped: &T) -> Option<&TlangShape>
    where
        T: Shaped,
    {
        self.get_shape_by_key(shaped.shape())
    }

    pub fn get_shape_by_key(&self, key: ShapeKey) -> Option<&TlangShape> {
        match key {
            ShapeKey::Native(_) => self.builtin_shapes.get_shape(key),
            _ => self.shapes.get(&key),
        }
    }

    pub fn get_struct_field_index(&self, shape: ShapeKey, field: &str) -> Option<usize> {
        self.shapes
            .get(&shape)
            .and_then(|s| s.get_struct_shape())
            .and_then(|shape| shape.get_field_index(field))
    }

    pub fn set_struct_method(
        &mut self,
        shape: ShapeKey,
        method_name: &str,
        method: TlangStructMethod,
    ) {
        self.shapes
            .get_mut(&shape)
            .and_then(|s| s.get_struct_shape_mut())
            .and_then(|shape| shape.method_map.insert(method_name.to_string(), method));
    }

    pub fn set_enum_method(
        &mut self,
        shape: ShapeKey,
        method_name: &str,
        method: TlangStructMethod,
    ) {
        self.shapes
            .get_mut(&shape)
            .and_then(|s| s.get_enum_shape_mut())
            .and_then(|shape| shape.method_map.insert(method_name.to_string(), method));
    }

    pub fn define_struct_shape(
        &mut self,
        shape_key: ShapeKey,
        name: String,
        fields: Vec<String>,
        methods: HashMap<String, TlangStructMethod>,
    ) -> ShapeKey {
        let shape = TlangShape::new_struct_shape(name, fields, methods);
        self.set_shape(shape_key, shape);
        shape_key
    }

    /// Returns an iterator over GC roots owned by the heap itself
    /// (temp_roots and native_fn object IDs).
    pub fn owned_gc_roots(&self) -> impl Iterator<Item = TlangValue> + '_ {
        let temp_roots = self.temp_roots.iter().copied();
        let native_fns = self.native_fns.keys().copied().map(TlangValue::Object);
        temp_roots.chain(native_fns)
    }
}
