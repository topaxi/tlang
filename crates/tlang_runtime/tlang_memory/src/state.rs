use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use log::debug;
use slab::Slab;
use smallvec::SmallVec;
use tlang_hir::hir;
use tlang_span::HirId;

use crate::resolver::Resolver;
use crate::scope::{Scope, ScopeStack};
use crate::shape::{
    ShapeKey, Shaped, TlangEnumShape, TlangEnumVariant, TlangShape, TlangStructMethod,
    TlangStructShape,
};
use crate::value::{
    TlangValue,
    function::{NativeFnReturn, TlangNativeFn},
    object::{
        TlangCell, TlangClosure, TlangEnum, TlangObjectId, TlangObjectKind, TlangSlice, TlangStruct,
    },
};

pub enum CallStackKind {
    Root,
    Function(Rc<hir::FunctionDeclaration>),
    NativeFn(String),
}

pub struct CallStackEntry {
    pub kind: CallStackKind,
    pub tail_call: Option<TailCall>,
    pub current_span: tlang_span::Span,
}

impl CallStackEntry {
    pub fn new_call(fn_decl: &Rc<hir::FunctionDeclaration>) -> Self {
        Self {
            kind: CallStackKind::Function(fn_decl.clone()),
            tail_call: None,
            current_span: fn_decl.span,
        }
    }

    pub fn replace_fn_decl(&mut self, fn_decl: Rc<hir::FunctionDeclaration>) {
        self.kind = CallStackKind::Function(fn_decl);
    }

    pub fn set_tail_call(&mut self, tail_call: TailCall) {
        self.tail_call = Some(tail_call);
    }

    pub fn get_fn_decl(&self) -> Option<Rc<hir::FunctionDeclaration>> {
        match &self.kind {
            CallStackKind::Function(fn_decl) => Some(fn_decl.clone()),
            _ => None,
        }
    }
}

pub struct BuiltinShapes {
    pub list: ShapeKey,
    pub list_iterator: ShapeKey,
    pub option: ShapeKey,
    pub result: ShapeKey,

    store: Slab<TlangShape>,
}

impl Default for BuiltinShapes {
    fn default() -> Self {
        Self::new()
    }
}

impl BuiltinShapes {
    pub fn new() -> Self {
        let mut store = Slab::with_capacity(3);

        let option = Self::create_option_shape(&mut store);
        let result = Self::create_result_shape(&mut store);
        let list = Self::create_list_shape(&mut store);
        let list_iterator = Self::create_list_iterator_shape(&mut store);

        Self {
            list,
            list_iterator,
            option,
            result,
            store,
        }
    }

    fn create_option_shape(store: &mut Slab<TlangShape>) -> ShapeKey {
        ShapeKey::new_native(store.insert(TlangShape::new_enum_shape(
            "Option".to_string(),
            vec![
                TlangEnumVariant {
                    name: "Some".to_string(),
                    field_map: HashMap::from([("0".to_string(), 0)]),
                },
                TlangEnumVariant {
                    name: "None".to_string(),
                    field_map: HashMap::new(),
                },
            ],
            HashMap::new(),
        )))
    }

    fn create_result_shape(store: &mut Slab<TlangShape>) -> ShapeKey {
        ShapeKey::new_native(store.insert(TlangShape::new_enum_shape(
            "Result".to_string(),
            vec![
                TlangEnumVariant {
                    name: "Ok".to_string(),
                    field_map: HashMap::from([("0".to_string(), 0)]),
                },
                TlangEnumVariant {
                    name: "Err".to_string(),
                    field_map: HashMap::from([("0".to_string(), 0)]),
                },
            ],
            HashMap::new(),
        )))
    }

    fn create_list_shape(store: &mut Slab<TlangShape>) -> ShapeKey {
        ShapeKey::new_native(store.insert(TlangShape::new_struct_shape(
            "List".to_string(),
            vec![],
            HashMap::new(),
        )))
    }

    fn create_list_iterator_shape(store: &mut Slab<TlangShape>) -> ShapeKey {
        ShapeKey::new_native(store.insert(TlangShape::new_struct_shape(
            "ListIterator".to_string(),
            vec!["list".to_string(), "index".to_string()],
            HashMap::new(),
        )))
    }

    /// # Panics
    pub fn get_list_shape(&self) -> &TlangStructShape {
        self.store
            .get(self.list.get_native_index())
            .and_then(|s| s.get_struct_shape())
            .unwrap()
    }

    /// # Panics
    pub fn get_list_shape_mut(&mut self) -> &mut TlangStructShape {
        self.store
            .get_mut(self.list.get_native_index())
            .and_then(|s| s.get_struct_shape_mut())
            .unwrap()
    }

    /// # Panics
    pub fn get_list_iterator_shape(&self) -> &TlangStructShape {
        self.store
            .get(self.list_iterator.get_native_index())
            .and_then(|s| s.get_struct_shape())
            .unwrap()
    }

    /// # Panics
    pub fn get_list_iterator_shape_mut(&mut self) -> &mut TlangStructShape {
        self.store
            .get_mut(self.list_iterator.get_native_index())
            .and_then(|s| s.get_struct_shape_mut())
            .unwrap()
    }

    /// # Panics
    pub fn get_option_shape(&self) -> &TlangEnumShape {
        self.store
            .get(self.option.get_native_index())
            .and_then(|s| s.get_enum_shape())
            .unwrap()
    }

    /// # Panics
    pub fn get_option_shape_mut(&mut self) -> &mut TlangEnumShape {
        self.store
            .get_mut(self.option.get_native_index())
            .and_then(|s| s.get_enum_shape_mut())
            .unwrap()
    }

    /// # Panics
    pub fn get_result_shape(&self) -> &TlangEnumShape {
        self.store
            .get(self.result.get_native_index())
            .and_then(|s| s.get_enum_shape())
            .unwrap()
    }

    /// # Panics
    pub fn get_result_shape_mut(&mut self) -> &mut TlangEnumShape {
        self.store
            .get_mut(self.result.get_native_index())
            .and_then(|s| s.get_enum_shape_mut())
            .unwrap()
    }
}

#[derive(Debug)]
pub struct TailCall {
    pub callee: TlangValue,
    pub args: SmallVec<[TlangValue; 4]>,
}

#[derive(Debug)]
pub struct NativeFnMeta {
    pub name: String,
}

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

pub struct InterpreterState {
    pub scope_stack: ScopeStack,
    closures: HashMap<HirId, Rc<hir::FunctionDeclaration>>,
    objects: Slab<TlangObjectKind>,
    shapes: HashMap<ShapeKey, TlangShape>,
    fn_decls: HashMap<HirId, Rc<hir::FunctionDeclaration>>,
    struct_decls: HashMap<String, Rc<hir::StructDeclaration>>,
    enum_decls: HashMap<String, Rc<hir::EnumDeclaration>>,
    call_stack: Vec<CallStackEntry>,
    globals: HashMap<String, TlangValue>,
    pub builtin_shapes: BuiltinShapes,
    native_fns: HashMap<TlangObjectId, TlangNativeFn>,
    native_fns_meta: HashMap<TlangObjectId, NativeFnMeta>,
    /// Memory statistics for debugging and monitoring.
    memory_stats: MemoryStats,
}

impl Resolver for InterpreterState {
    fn resolve_value(&self, path: &hir::Path) -> Option<TlangValue> {
        if !path.res.is_value() {
            return None;
        }

        let value = self
            .scope_stack
            .resolve_value(path)
            .or_else(|| self.globals.get(&path.to_string()).copied());

        debug!(
            "Resolved path: \"{}\" ({:?}), got: {:?}",
            path,
            path.res,
            value.map(|v| self.stringify(v))
        );

        value
    }
}

impl InterpreterState {
    pub fn new() -> Self {
        let mut call_stack = Vec::with_capacity(1000);

        call_stack.push(CallStackEntry {
            kind: CallStackKind::Root,
            tail_call: None,
            current_span: tlang_span::Span::default(),
        });

        Self {
            scope_stack: ScopeStack::default(),
            closures: HashMap::with_capacity(100),
            objects: Slab::with_capacity(1000),
            struct_decls: HashMap::with_capacity(100),
            enum_decls: HashMap::with_capacity(100),
            fn_decls: HashMap::with_capacity(1000),
            shapes: HashMap::with_capacity(100),
            call_stack,
            globals: HashMap::with_capacity(100),
            builtin_shapes: BuiltinShapes::default(),
            native_fns: HashMap::with_capacity(100),
            native_fns_meta: HashMap::with_capacity(100),
            memory_stats: MemoryStats::default(),
        }
    }

    /// Returns an iterator over all GC root values.
    pub fn gc_roots(&self) -> impl Iterator<Item = TlangValue> + '_ {
        let globals = self.globals.values().copied();
        let scope_memory = self.scope_stack.memory_iter();
        let native_fns = self.native_fns.keys().copied().map(TlangValue::Object);

        globals.chain(scope_memory).chain(native_fns)
    }

    /// Mark all objects reachable from GC roots.
    /// Returns a set of reachable object IDs.
    pub fn mark_reachable(&self) -> HashSet<TlangObjectId> {
        let mut reachable = HashSet::new();
        let mut values = self.gc_roots().collect::<Vec<_>>();

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
            self.remove_object(id);
        }

        count
    }

    /// Runs a complete garbage collection cycle.
    /// Returns the number of objects collected.
    pub fn collect_garbage(&mut self) -> usize {
        let unreachable = self.mark_reachable();
        let collected = self.sweep_unreachable(&unreachable);

        self.memory_stats.gc_collections += 1;
        self.memory_stats.objects_deallocated += collected;

        log::debug!(
            "GC: collected {} objects, {} remain",
            collected,
            self.objects.len()
        );

        collected
    }

    pub fn should_collect(&self) -> bool {
        true
    }

    pub fn get_fn_decl(&self, id: HirId) -> Option<Rc<hir::FunctionDeclaration>> {
        self.fn_decls.get(&id).cloned()
    }

    pub fn set_fn_decl(&mut self, id: HirId, decl: Rc<hir::FunctionDeclaration>) {
        self.fn_decls.insert(id, decl);
    }

    pub fn get_struct_decl(&self, path: &hir::Path) -> Option<Rc<hir::StructDeclaration>> {
        self.struct_decls.get(&path.to_string()).cloned()
    }

    pub fn set_struct_decl(&mut self, path_name: String, decl: Rc<hir::StructDeclaration>) {
        self.struct_decls.insert(path_name, decl);
    }

    pub fn get_enum_decl(&self, path: &hir::Path) -> Option<Rc<hir::EnumDeclaration>> {
        self.enum_decls.get(&path.to_string()).cloned()
    }

    pub fn set_enum_decl(&mut self, path_name: String, decl: Rc<hir::EnumDeclaration>) {
        self.enum_decls.insert(path_name, decl);
    }

    /// # Panics
    #[allow(clippy::needless_pass_by_value)]
    pub fn panic(&self, message: String) -> ! {
        let mut call_stack = String::new();

        for entry in self.call_stack.iter().rev() {
            match &entry.kind {
                CallStackKind::Function(decl) => {
                    call_stack.push_str(&format!(
                        "  at function {}:{}\n",
                        decl.name(),
                        entry.current_span.start
                    ));
                }
                CallStackKind::NativeFn(name) => {
                    call_stack.push_str(&format!("  at native function {name}:0:0\n"));
                }
                CallStackKind::Root => {
                    call_stack.push_str(&format!("  at root {}\n", entry.current_span.start));
                }
            }
        }

        panic!("{message}\n{call_stack}")
    }

    /// # Panics
    pub fn set_current_span(&mut self, span: tlang_span::Span) {
        self.call_stack.last_mut().unwrap().current_span = span;
    }

    pub fn push_call_stack(&mut self, entry: CallStackEntry) {
        self.call_stack.push(entry);
    }

    /// # Panics
    pub fn pop_call_stack(&mut self) -> CallStackEntry {
        self.call_stack.pop().unwrap()
    }

    /// # Panics
    pub fn current_call_frame(&mut self) -> &CallStackEntry {
        self.call_stack.last().unwrap()
    }

    /// # Panics
    pub fn current_call_frame_mut(&mut self) -> &mut CallStackEntry {
        self.call_stack.last_mut().unwrap()
    }

    pub fn enter_scope<T>(&mut self, meta: &T)
    where
        T: hir::HirScope,
    {
        self.scope_stack.push(meta);
    }

    pub fn exit_scope(&mut self) {
        self.scope_stack.pop();
    }

    pub fn current_scope(&self) -> &Scope {
        self.scope_stack.current_scope()
    }

    pub fn push_value(&mut self, value: TlangValue) {
        self.scope_stack.push_value(value);
    }

    /// Allocate a variable index for let bindings and set the value at that position
    pub fn set_let_binding(&mut self, value: TlangValue) -> usize {
        let index = self.scope_stack.allocate_let_binding_index();
        self.scope_stack.set_local(index, value);
        index
    }

    /// Initialize variable index counter after function parameters are pushed
    pub fn init_var_index_after_params(&mut self, param_count: usize) {
        self.scope_stack.init_var_index_after_params(param_count);
    }

    /// Check if we're currently in the global scope
    pub fn is_global_scope(&self) -> bool {
        self.scope_stack.scopes.len() == 1
    }

    /// Check if the current scope has allocated slots for variables
    pub fn current_scope_has_slots(&self) -> bool {
        self.scope_stack.current_scope_has_slots()
    }

    pub fn set_global(&mut self, name: String, value: TlangValue) {
        self.globals.insert(name, value);
    }

    pub fn new_object(&mut self, kind: TlangObjectKind) -> TlangValue {
        self.memory_stats.objects_allocated += 1;
        TlangValue::new_object(self.objects.insert(kind))
    }

    /// Create a new cell containing the given value.
    /// Cells are used for closure captures to enable mutable bindings.
    pub fn new_cell(&mut self, value: TlangValue) -> TlangValue {
        self.new_object(TlangObjectKind::Cell(TlangCell::new(value)))
    }

    /// Get the value inside a cell.
    pub fn get_cell_value(&self, cell_id: TlangObjectId) -> Option<TlangValue> {
        self.objects
            .get(cell_id)
            .and_then(|obj| obj.get_cell())
            .map(|cell| cell.get())
    }

    /// Set the value inside a cell.
    pub fn set_cell_value(&mut self, cell_id: TlangObjectId, value: TlangValue) {
        let Some(obj) = self.objects.get_mut(cell_id) else {
            debug_assert!(false, "set_cell_value called with invalid cell_id");
            return;
        };

        let Some(cell) = obj.get_cell_mut() else {
            debug_assert!(false, "set_cell_value called on non-cell object");
            return;
        };

        cell.set(value);
    }

    /// Returns the current memory statistics.
    pub fn memory_stats(&self) -> &MemoryStats {
        &self.memory_stats
    }

    /// Returns the number of currently allocated objects in the object store.
    pub fn object_count(&self) -> usize {
        self.objects.len()
    }

    pub fn new_enum(
        &mut self,
        shape: ShapeKey,
        variant: usize,
        values: Vec<TlangValue>,
    ) -> TlangValue {
        self.new_object(TlangObjectKind::Enum(TlangEnum::new(
            shape, variant, values,
        )))
    }

    pub fn new_closure(&mut self, decl: &hir::FunctionDeclaration) -> TlangValue {
        self.closures
            .entry(decl.hir_id)
            .or_insert_with(|| decl.clone().into());

        // Capture all memory values from the current scope stack for GC tracing.
        let global_memory_len = self.scope_stack.global_memory_len();
        let captured_memory = self.scope_stack.capture_all_memory();

        self.new_object(TlangObjectKind::Closure(TlangClosure {
            id: decl.hir_id,
            scope_stack: self.scope_stack.scopes.clone(),
            captured_cells: std::collections::HashMap::new(),
            captured_memory,
            global_memory_len,
        }))
    }

    /// # Panics
    pub fn new_native_fn<F>(&mut self, name: &str, f: F) -> TlangValue
    where
        F: Fn(&mut InterpreterState, &[TlangValue]) -> NativeFnReturn + 'static,
    {
        let fn_object = self.new_object(TlangObjectKind::NativeFn);

        self.native_fns
            .insert(fn_object.get_object_id().unwrap(), Rc::new(f));
        self.native_fns_meta.insert(
            fn_object.get_object_id().unwrap(),
            NativeFnMeta {
                name: name.to_string(),
            },
        );

        fn_object
    }

    pub fn new_native_method<F>(&mut self, name: &str, f: F) -> TlangStructMethod
    where
        F: Fn(&mut InterpreterState, TlangValue, &[TlangValue]) -> NativeFnReturn + 'static,
    {
        TlangStructMethod::from(
            self.new_native_fn(name, move |state, args| f(state, args[0], &args[1..])),
        )
    }

    pub fn call_native_fn(
        &mut self,
        fn_id: TlangObjectId,
        args: &[TlangValue],
    ) -> Option<NativeFnReturn> {
        let native_fn = self.native_fns.get(&fn_id)?.clone();
        Some(native_fn(self, args))
    }

    pub fn get_closure_decl(&self, id: HirId) -> Option<Rc<hir::FunctionDeclaration>> {
        self.closures.get(&id).cloned()
    }

    /// Removes an object from the object store by its ID.
    ///
    /// This is used during garbage collection to deallocate unreachable objects.
    /// Returns the removed object if it existed, or None if the ID was invalid.
    ///
    /// # Important
    ///
    /// The caller must ensure that no other code holds references (via `TlangValue::Object`)
    /// to this object ID, as accessing a removed object will cause a panic.
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

    pub fn new_list(&mut self, values: Vec<TlangValue>) -> TlangValue {
        self.new_object(TlangObjectKind::Struct(TlangStruct::new(
            self.builtin_shapes.list,
            values,
        )))
    }

    pub fn new_string(&mut self, value: String) -> TlangValue {
        self.new_object(TlangObjectKind::String(value))
    }

    pub fn new_slice(&mut self, of: TlangValue, start: usize, len: usize) -> TlangValue {
        self.new_object(TlangObjectKind::Slice(TlangSlice::new(of, start, len)))
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
            ShapeKey::Native(idx) => self.builtin_shapes.store.get(idx),
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

    pub fn debug_stringify_scope_stack(&self) -> String {
        let mut out = "[\n".to_string();
        for scope in self.scope_stack.iter() {
            out.push_str("  {\n");
            for entry in self.scope_stack.get_scope_locals(scope) {
                out.push_str("    ");
                out.push_str(self.stringify(*entry).as_str());
                out.push_str(",\n");
            }
            out.push_str("  },\n");
        }
        out.push_str("]\n");
        out
    }

    fn stringify_struct_as_list(&self, s: &TlangStruct) -> String {
        let values = s
            .values()
            .iter()
            .map(|v| self.stringify(*v))
            .collect::<Vec<String>>()
            .join(", ");
        format!("[{values}]")
    }

    fn stringify_struct(&self, s: &TlangStruct) -> String {
        if s.shape() == self.builtin_shapes.list {
            return self.stringify_struct_as_list(s);
        }

        let shape = self
            .get_shape(s)
            .and_then(|s| s.get_struct_shape())
            .unwrap();

        if shape.has_fields() && shape.has_consecutive_integer_fields() {
            return format!("{} {}", shape.name, self.stringify_struct_as_list(s));
        }

        let mut result = String::new();

        result.push_str(&shape.name);
        result.push_str(" { ");

        if !shape.has_fields() {
            result.push('}');
            return result;
        }

        let mut fields = shape.get_fields();
        fields.sort();
        result.push_str(
            &fields
                .into_iter()
                .map(|(field, idx)| format!("{}: {}", field, self.stringify(s[idx])))
                .collect::<Vec<String>>()
                .join(", "),
        );

        result.push_str(" }");
        result
    }

    fn stringify_enum(&self, e: &TlangEnum) -> String {
        let shape = self.get_shape(e).and_then(|s| s.get_enum_shape()).unwrap();
        let variant = &shape.variants[e.variant];
        let mut result = String::new();
        result.push_str(&shape.name);
        result.push_str("::");
        result.push_str(&variant.name);

        if variant.field_map.is_empty() {
            return result;
        }

        result.push('(');
        for (i, (field, idx)) in variant.field_map.iter().enumerate() {
            if i > 0 {
                result.push_str(", ");
            }
            result.push_str(field);
            result.push_str(": ");
            result.push_str(&self.stringify(e.field_values[*idx]));
        }
        result.push(')');
        result
    }

    /// # Panics
    pub fn stringify(&self, value: TlangValue) -> String {
        match value {
            TlangValue::Object(id) => match self.get_object_by_id(id) {
                None => value.to_string(),
                Some(TlangObjectKind::String(s)) => s.clone(),
                Some(TlangObjectKind::Struct(s)) => self.stringify_struct(s),
                Some(TlangObjectKind::Enum(e)) => self.stringify_enum(e),
                Some(TlangObjectKind::Slice(s)) => {
                    let values = self
                        .get_slice_values(*s)
                        .iter()
                        .map(|v| self.stringify(*v))
                        .collect::<Vec<String>>()
                        .join(", ");
                    format!("&[{values}]")
                }
                Some(TlangObjectKind::Closure(s)) => {
                    let fn_decl = self.closures.get(&s.id).unwrap();

                    format!(
                        "fn {}#{}({})",
                        fn_decl.name(),
                        s.id,
                        fn_decl
                            .parameters
                            .iter()
                            .map(|p| p.name.as_str())
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
                }
                Some(TlangObjectKind::Fn(id)) => {
                    let fn_decl = self.fn_decls.get(id).unwrap();

                    format!(
                        "fn {}#{}({})",
                        fn_decl.name(),
                        id,
                        fn_decl
                            .parameters
                            .iter()
                            .map(|p| p.name.as_str())
                            .collect::<Vec<_>>()
                            .join(", "),
                    )
                }
                Some(TlangObjectKind::NativeFn) => {
                    if let Some(meta) = self.native_fns_meta.get(&id) {
                        format!("fn {}(...) {{ *native* }}", meta.name)
                    } else {
                        "fn anonymous(...) { *native* }".to_string()
                    }
                }
                Some(TlangObjectKind::Cell(cell)) => {
                    // Stringify the value inside the cell
                    self.stringify(cell.get())
                }
            },
            _ => value.to_string(),
        }
    }
}

impl Default for InterpreterState {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_memory_stats_initial() {
        let state = InterpreterState::new();
        let stats = state.memory_stats();

        assert_eq!(stats.objects_allocated, 0);
        assert_eq!(stats.objects_deallocated, 0);
        assert_eq!(stats.live_objects(), 0);
    }

    #[test]
    fn test_memory_stats_after_allocation() {
        let mut state = InterpreterState::new();

        // Allocate some objects
        state.new_string("hello".to_string());
        state.new_string("world".to_string());
        state.new_list(vec![TlangValue::I64(1), TlangValue::I64(2)]);

        let stats = state.memory_stats();
        assert_eq!(stats.objects_allocated, 3);
        assert_eq!(stats.objects_deallocated, 0);
        assert_eq!(stats.live_objects(), 3);
        assert_eq!(state.object_count(), 3);
    }

    #[test]
    fn test_object_removal() {
        let mut state = InterpreterState::new();

        // Allocate objects
        let obj1 = state.new_string("first".to_string());
        let obj2 = state.new_string("second".to_string());
        let _obj3 = state.new_string("third".to_string());

        assert_eq!(state.object_count(), 3);

        // Remove an object
        let id1 = obj1.get_object_id().unwrap();
        let removed = state.remove_object(id1);
        assert!(removed.is_some());

        let stats = state.memory_stats();
        assert_eq!(stats.objects_allocated, 3);
        assert_eq!(stats.objects_deallocated, 1);
        assert_eq!(stats.live_objects(), 2);
        assert_eq!(state.object_count(), 2);

        // Object should no longer exist
        assert!(!state.contains_object(id1));
        assert!(state.get_object(obj1).is_none());

        // Other objects should still exist
        assert!(state.get_object(obj2).is_some());
    }

    #[test]
    fn test_remove_nonexistent_object() {
        let mut state = InterpreterState::new();

        // Try to remove an object that doesn't exist
        let removed = state.remove_object(999);
        assert!(removed.is_none());

        // Stats should not change
        let stats = state.memory_stats();
        assert_eq!(stats.objects_deallocated, 0);
    }

    #[test]
    fn test_contains_object() {
        let mut state = InterpreterState::new();

        let obj = state.new_string("test".to_string());
        let id = obj.get_object_id().unwrap();

        assert!(state.contains_object(id));
        assert!(!state.contains_object(999));
    }

    #[test]
    fn test_gc_roots_includes_globals() {
        let value = TlangValue::I64(42);
        let mut state = InterpreterState::new();
        state.set_global("test".to_string(), value);

        let roots: Vec<_> = state.gc_roots().collect();
        assert!(roots.contains(&value));
    }

    #[test]
    fn test_mark_phase_finds_reachable() {
        let mut state = InterpreterState::new();

        // Create a reachable object using new_string helper
        let obj = state.new_string("hello".to_string());
        state.set_global("my_string".to_string(), obj);

        let marked = state.mark_reachable();

        if let TlangValue::Object(id) = obj {
            assert!(marked.contains(&id));
        }
    }

    #[test]
    fn test_mark_phase_ignores_unreachable() {
        let mut state = InterpreterState::new();

        // Create an unreachable object (no root reference)
        let orphan = state.new_string("orphan".to_string());
        let orphan_id = orphan.get_object_id().unwrap();

        let marked = state.mark_reachable();

        // Orphan should NOT be in marked set
        assert!(!marked.contains(&orphan_id));
    }

    #[test]
    fn test_sweep_removes_unreachable() {
        let mut state = InterpreterState::new();

        // Create an object but don't make it a root
        let orphan = state.new_string("orphan".to_string());
        let orphan_id = orphan.get_object_id().unwrap();

        // Run GC phases
        let marked = state.mark_reachable();
        let collected = state.sweep_unreachable(&marked);

        assert_eq!(collected, 1);
        assert!(!state.contains_object(orphan_id));
    }
}
