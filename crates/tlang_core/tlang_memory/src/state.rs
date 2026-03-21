use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use log::debug;
use tlang_hir as hir;
use tlang_span::HirId;

use crate::execution::{CallStackEntry, CallStackKind, ExecutionContext};
use crate::heap::{Heap, MemoryStats, NativeFnMeta};
use crate::program::Program;
use crate::resolver::Resolver;
use crate::scope::Scope;
use crate::shape::{ProtocolId, ShapeKey, Shaped, TlangShape, TlangStructMethod};
use crate::value::{
    TlangValue,
    function::NativeFnReturn,
    object::{
        TlangCell, TlangClosure, TlangEnum, TlangObjectId, TlangObjectKind, TlangSlice, TlangStruct,
    },
};

/// Function pointer type for calling a `TlangValue` that references a callable
/// (function, closure, or native function). Registered by the VM to allow
/// native functions to invoke user-defined callables.
type CallFn = fn(&mut VMState, TlangValue, &[TlangValue]) -> TlangValue;

pub struct VMState {
    pub heap: Heap,
    pub program: Program,
    pub execution: ExecutionContext,
    call_fn: CallFn,
}

impl Resolver for VMState {
    fn resolve_value(&self, path: &hir::Path) -> Option<TlangValue> {
        if !path.res.is_value() {
            return None;
        }

        let value =
            self.execution
                .scope_stack
                .resolve_value(path)
                .or_else(|| match path.res.slot() {
                    hir::Slot::Global(i) => self.program.global_slots.get(i).copied(),
                    _ => self.program.globals.get(&path.to_string()).copied(),
                });

        debug!(
            "Resolved path: \"{}\" ({:?}), got: {:?}",
            path, path.res, value
        );

        value
    }
}

impl VMState {
    /// # Panics
    /// Must call `register_call_fn` before using `call`, or else the default call handler
    /// will panic on any attempt to call a value.
    pub fn new() -> Self {
        Self {
            heap: Heap::new(),
            program: Program::new(),
            execution: ExecutionContext::new(),
            call_fn: |_vm, callee, args| {
                panic!(
                    "Call handler not registered: attempted to call {:?} with args {:?}",
                    callee, args
                )
            },
        }
    }

    // ── GC ──────────────────────────────────────────────────────────────────

    /// Returns an iterator over all GC root values across all three sub-states.
    pub fn gc_roots(&self) -> impl Iterator<Item = TlangValue> + '_ {
        self.heap
            .owned_gc_roots()
            .chain(self.program.gc_roots())
            .chain(self.execution.gc_roots())
    }

    /// Mark all objects reachable from GC roots.
    /// Returns a set of reachable object IDs.
    pub fn mark_reachable(&self) -> HashSet<TlangObjectId> {
        self.heap.mark_reachable(self.gc_roots())
    }

    /// Remove all objects not in the reachable set.
    /// Returns the number of objects collected.
    pub fn sweep_unreachable(&mut self, reachable: &HashSet<TlangObjectId>) -> usize {
        self.heap.sweep_unreachable(reachable)
    }

    /// Runs a complete garbage collection cycle.
    /// Returns the number of objects collected.
    /// # Panics
    pub fn collect_garbage(&mut self) -> usize {
        let reachable = self.mark_reachable();
        let collected = self.heap.sweep_unreachable(&reachable);

        self.heap.memory_stats.gc_collections += 1;
        self.heap.memory_stats.objects_deallocated += collected;
        self.heap.next_gc_threshold =
            (self.heap.object_count() * 2).max(Heap::GC_INITIAL_THRESHOLD);

        log::debug!(
            "GC: collected {} objects, {} remain",
            collected,
            self.heap.object_count()
        );

        #[cfg(debug_assertions)]
        for value in self.gc_roots() {
            if let TlangValue::Object(id) = value {
                assert!(
                    self.heap.contains_object(id),
                    "GC BUG: root references collected object {id}"
                );
            }
        }

        collected
    }

    // ── Object allocation ───────────────────────────────────────────────────

    /// Allocate a new heap object, triggering GC if the threshold is reached.
    pub fn new_object(&mut self, kind: TlangObjectKind) -> TlangValue {
        if self.heap.should_collect() {
            self.collect_garbage();
        }
        self.heap.alloc_object(kind)
    }

    /// Create a new cell containing the given value.
    /// Cells are used for closure captures to enable mutable bindings.
    pub fn new_cell(&mut self, value: TlangValue) -> TlangValue {
        self.new_object(TlangObjectKind::Cell(TlangCell::new(value)))
    }

    /// Get the value inside a cell.
    pub fn get_cell_value(&self, cell_id: TlangObjectId) -> Option<TlangValue> {
        self.heap
            .get_object_by_id(cell_id)
            .and_then(|obj| obj.as_cell())
            .map(|cell| cell.get())
    }

    /// Set the value inside a cell.
    pub fn set_cell_value(&mut self, cell_id: TlangObjectId, value: TlangValue) {
        let Some(obj) = self.heap.get_object_by_id_mut(cell_id) else {
            debug_assert!(false, "set_cell_value called with invalid cell_id");
            return;
        };

        let Some(cell) = obj.as_cell_mut() else {
            debug_assert!(false, "set_cell_value called on non-cell object");
            return;
        };

        cell.set(value);
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
        self.program
            .closures
            .entry(decl.hir_id)
            .or_insert_with(|| decl.clone().into());

        // Capture all memory values from the current scope stack for GC tracing.
        let global_memory_len = self.execution.scope_stack.global_memory_len();
        let captured_memory = self.execution.scope_stack.capture_all_memory();

        self.new_object(TlangObjectKind::Closure(TlangClosure {
            id: decl.hir_id,
            scope_stack: self.execution.scope_stack.scopes.clone(),
            captured_cells: std::collections::HashMap::new(),
            captured_memory,
            global_memory_len,
        }))
    }

    /// # Panics
    pub fn new_native_fn<F>(&mut self, name: &str, f: F) -> TlangValue
    where
        F: Fn(&mut VMState, &[TlangValue]) -> NativeFnReturn + 'static,
    {
        let fn_object = self.new_object(TlangObjectKind::NativeFn);
        let id = fn_object.get_object_id().unwrap();
        self.heap.native_fns.insert(id, Rc::new(f));
        self.heap.native_fns_meta.insert(
            id,
            NativeFnMeta {
                name: name.to_string(),
            },
        );
        fn_object
    }

    pub fn new_native_method<F>(&mut self, name: &str, f: F) -> TlangStructMethod
    where
        F: Fn(&mut VMState, TlangValue, &[TlangValue]) -> NativeFnReturn + 'static,
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
        let native_fn = self.heap.native_fns.get(&fn_id)?.clone();
        let name = self.heap.native_fns_meta.get(&fn_id)?.name.clone();

        self.execution
            .push_call_stack(CallStackEntry::new_native_call(&name));
        let result = native_fn(self, args);
        self.execution.pop_call_stack();

        Some(result)
    }

    pub fn new_list(&mut self, values: Vec<TlangValue>) -> TlangValue {
        self.new_object(TlangObjectKind::Struct(TlangStruct::new(
            self.heap.builtin_shapes.list,
            values,
        )))
    }

    pub fn new_regex(&mut self, source: String, flags: String) -> TlangValue {
        let source_val = self.new_string(source);
        let flags_val = self.new_string(flags);
        self.new_object(TlangObjectKind::Struct(TlangStruct::new(
            self.heap.builtin_shapes.regex,
            vec![source_val, flags_val],
        )))
    }

    pub fn new_string_buf(&mut self, initial: String) -> TlangValue {
        let inner = self.new_string(initial);
        self.new_object(TlangObjectKind::Struct(TlangStruct::new(
            self.heap.builtin_shapes.string_buf,
            vec![inner],
        )))
    }

    /// Returns the current string contents of a StringBuf value, or `None` if
    /// the value is not a StringBuf.
    pub fn get_string_buf(&self, value: TlangValue) -> Option<&str> {
        let s = self.heap.get_struct(value)?;
        if s.shape() != self.heap.builtin_shapes.string_buf {
            return None;
        }
        self.heap.get_object(s[0])?.as_str()
    }

    pub fn new_string(&mut self, value: String) -> TlangValue {
        self.new_object(TlangObjectKind::String(value))
    }

    pub fn new_slice(&mut self, of: TlangValue, start: usize, len: usize) -> TlangValue {
        self.new_object(TlangObjectKind::Slice(TlangSlice::new(of, start, len)))
    }

    // ── Heap delegates ──────────────────────────────────────────────────────

    pub fn temp_roots_mark(&self) -> usize {
        self.heap.temp_roots_mark()
    }

    pub fn push_temp_root(&mut self, value: TlangValue) {
        self.heap.push_temp_root(value);
    }

    pub fn temp_roots_restore(&mut self, mark: usize) {
        self.heap.temp_roots_restore(mark);
    }

    pub fn set_stress_gc(&mut self, stress: bool) {
        self.heap.set_stress_gc(stress);
    }

    pub fn should_collect(&self) -> bool {
        self.heap.should_collect()
    }

    /// Returns the current memory statistics.
    pub fn memory_stats(&self) -> &MemoryStats {
        self.heap.memory_stats()
    }

    /// Returns the number of currently allocated objects in the object store.
    pub fn object_count(&self) -> usize {
        self.heap.object_count()
    }

    #[inline(always)]
    pub fn get_object_by_id_mut(&mut self, id: TlangObjectId) -> Option<&mut TlangObjectKind> {
        self.heap.get_object_by_id_mut(id)
    }

    #[inline(always)]
    pub fn get_object_by_id(&self, id: TlangObjectId) -> Option<&TlangObjectKind> {
        self.heap.get_object_by_id(id)
    }

    pub fn get_object(&self, value: TlangValue) -> Option<&TlangObjectKind> {
        self.heap.get_object(value)
    }

    pub fn get_object_mut(&mut self, value: TlangValue) -> Option<&mut TlangObjectKind> {
        self.heap.get_object_mut(value)
    }

    pub fn get_struct(&self, value: TlangValue) -> Option<&TlangStruct> {
        self.heap.get_struct(value)
    }

    pub fn get_struct_mut(&mut self, value: TlangValue) -> Option<&mut TlangStruct> {
        self.heap.get_struct_mut(value)
    }

    pub fn get_enum(&self, value: TlangValue) -> Option<&TlangEnum> {
        self.heap.get_enum(value)
    }

    pub fn get_slice(&self, value: TlangValue) -> Option<TlangSlice> {
        self.heap.get_slice(value)
    }

    /// # Panics
    pub fn get_slice_value(&self, slice: TlangSlice, index: usize) -> TlangValue {
        self.heap.get_slice_value(slice, index)
    }

    /// # Panics
    pub fn get_slice_values(&self, slice: TlangSlice) -> &[TlangValue] {
        self.heap.get_slice_values(slice)
    }

    pub fn define_struct_shape(
        &mut self,
        shape_key: ShapeKey,
        name: String,
        fields: Vec<String>,
        methods: HashMap<String, TlangStructMethod>,
    ) -> ShapeKey {
        self.heap
            .define_struct_shape(shape_key, name, fields, methods)
    }

    pub fn has_shape(&self, key: ShapeKey) -> bool {
        self.heap.has_shape(key)
    }

    pub fn set_shape(&mut self, key: ShapeKey, shape: TlangShape) {
        self.heap.set_shape(key, shape);
    }

    #[inline(always)]
    pub fn get_shape<T>(&self, shaped: &T) -> Option<&TlangShape>
    where
        T: Shaped,
    {
        self.heap.get_shape(shaped)
    }

    pub fn get_shape_by_key(&self, key: ShapeKey) -> Option<&TlangShape> {
        self.heap.get_shape_by_key(key)
    }

    /// Returns the type name of a value for protocol dispatch.
    pub fn type_name_of(&self, value: TlangValue) -> &str {
        match value {
            TlangValue::Nil => "Nil",
            TlangValue::Bool(_) => "Bool",
            TlangValue::I8(_)
            | TlangValue::I16(_)
            | TlangValue::I32(_)
            | TlangValue::I64(_)
            | TlangValue::U8(_)
            | TlangValue::U16(_)
            | TlangValue::U32(_)
            | TlangValue::U64(_) => "Int",
            TlangValue::F32(_) | TlangValue::F64(_) => "Float",
            TlangValue::Object(_) => {
                if let Some(obj) = self.get_object(value) {
                    if let Some(shape_key) = obj.shape()
                        && let Some(shape) = self.get_shape_by_key(shape_key)
                    {
                        return shape.name();
                    }
                    if obj.as_str().is_some() {
                        return "String";
                    }
                    if obj.as_slice().is_some() {
                        return "Slice";
                    }
                }
                "Object"
            }
        }
    }

    /// Returns the ShapeKey for the type of a value, if available.
    /// Returns None for primitive types (Int, Float, Bool, Nil).
    /// Returns `Some` for shaped objects, String objects (using the builtin
    /// `string` shape), and Slice objects (using the builtin `slice` shape).
    pub fn type_shape_key_of(&self, value: TlangValue) -> Option<ShapeKey> {
        if !value.is_object() {
            return None;
        }
        if let Some(obj) = self.get_object(value) {
            if let Some(shape_key) = obj.shape() {
                return Some(shape_key);
            }
            if obj.as_str().is_some() {
                return Some(self.heap.builtin_shapes.string);
            }
            if obj.as_slice().is_some() {
                return Some(self.heap.builtin_shapes.slice);
            }
        }
        None
    }

    pub fn get_struct_field_index(&self, shape: ShapeKey, field: &str) -> Option<usize> {
        self.heap.get_struct_field_index(shape, field)
    }

    pub fn set_struct_method(
        &mut self,
        shape: ShapeKey,
        method_name: &str,
        method: TlangStructMethod,
    ) {
        self.heap.set_struct_method(shape, method_name, method);
    }

    pub fn set_enum_method(
        &mut self,
        shape: ShapeKey,
        method_name: &str,
        method: TlangStructMethod,
    ) {
        self.heap.set_enum_method(shape, method_name, method);
    }

    // ── Program delegates ───────────────────────────────────────────────────

    pub fn get_fn_decl(&self, id: HirId) -> Option<Rc<hir::FunctionDeclaration>> {
        self.program.get_fn_decl(id)
    }

    pub fn set_fn_decl(&mut self, id: HirId, decl: Rc<hir::FunctionDeclaration>) {
        self.program.set_fn_decl(id, decl);
    }

    /// Returns the HirIds and names of all registered function declarations.
    pub fn fn_decl_hir_ids(&self) -> Vec<(HirId, String)> {
        self.program
            .fn_decls
            .iter()
            .map(|(id, decl)| (*id, decl.name()))
            .collect()
    }

    pub fn get_struct_decl(&self, path: &hir::Path) -> Option<Rc<hir::StructDeclaration>> {
        self.program.get_struct_decl(path)
    }

    pub fn get_struct_decl_by_name(&self, name: &str) -> Option<Rc<hir::StructDeclaration>> {
        self.program.get_struct_decl_by_name(name)
    }

    pub fn set_struct_decl(&mut self, path_name: String, decl: Rc<hir::StructDeclaration>) {
        self.program.set_struct_decl(path_name, decl);
    }

    pub fn get_enum_decl(&self, path: &hir::Path) -> Option<Rc<hir::EnumDeclaration>> {
        self.program.get_enum_decl(path)
    }

    pub fn get_enum_decl_by_name(&self, name: &str) -> Option<Rc<hir::EnumDeclaration>> {
        self.program.get_enum_decl_by_name(name)
    }

    pub fn set_enum_decl(&mut self, path_name: String, decl: Rc<hir::EnumDeclaration>) {
        self.program.set_enum_decl(path_name, decl);
    }

    pub fn register_protocol(&mut self, id: ProtocolId, name: String, methods: Vec<String>) {
        self.program.register_protocol(id, name, methods);
    }

    pub fn register_protocol_impl(
        &mut self,
        protocol: ProtocolId,
        target_type: ShapeKey,
        method: &str,
        fn_value: TlangValue,
    ) {
        self.program
            .register_protocol_impl(protocol, target_type, method, fn_value);
    }

    pub fn get_protocol_impl(
        &self,
        protocol: ProtocolId,
        target_type: Option<ShapeKey>,
        method: &str,
    ) -> Option<TlangValue> {
        self.program
            .get_protocol_impl(protocol, target_type, method)
    }

    pub fn call_protocol_method(
        &mut self,
        protocol: ProtocolId,
        target_type: Option<ShapeKey>,
        method: &str,
        args: &[TlangValue],
    ) -> Option<TlangValue> {
        self.get_protocol_impl(protocol, target_type, method)
            .map(|fn_value| self.call(fn_value, args))
    }

    pub fn is_protocol(&self, name: &str) -> bool {
        self.program.is_protocol_by_name(name)
    }

    pub fn protocol_id_by_name(&self, name: &str) -> Option<ProtocolId> {
        self.program.protocol_id_by_name(name)
    }

    pub fn lookup_builtin_shape(&self, name: &str) -> Option<ShapeKey> {
        self.heap.builtin_shapes.lookup(name)
    }

    // ── Inventory-driven registration helpers ───────────────────────────

    /// Register a native enum shape from an inventory descriptor.
    pub fn register_native_enum(&mut self, def: &crate::NativeEnumDef) {
        use crate::shape::TlangEnumVariant;

        // Skip if already registered (e.g. pre-created in BuiltinShapes::new()).
        if self.heap.builtin_shapes.lookup(def.name()).is_some() {
            return;
        }

        let variants = def
            .variants()
            .iter()
            .map(|v| {
                let field_map = v
                    .fields
                    .iter()
                    .enumerate()
                    .map(|(i, f)| (f.to_string(), i))
                    .collect();
                TlangEnumVariant::new(v.name.to_string(), field_map)
            })
            .collect();

        self.heap
            .builtin_shapes
            .insert_enum_shape(def.name().to_string(), variants);
    }

    /// Register a native struct shape from an inventory descriptor.
    pub fn register_native_struct(&mut self, def: &crate::NativeStructDef) {
        // Skip if already registered.
        if self.heap.builtin_shapes.lookup(def.name()).is_some() {
            return;
        }

        let fields = def.fields().iter().map(|f| f.to_string()).collect();

        self.heap
            .builtin_shapes
            .insert_struct_shape(def.name().to_string(), fields);
    }

    /// Register a native method on a shape from an inventory descriptor.
    pub fn register_native_method(&mut self, def: &crate::NativeMethodDef) {
        let fn_ptr = def.fn_ptr();
        let method = self.new_native_method(
            &format!("{}::{}", def.type_name(), def.method_name()),
            move |state, this, args| {
                fn_ptr(state, &{
                    let mut v = Vec::with_capacity(1 + args.len());
                    v.push(this);
                    v.extend_from_slice(args);
                    v
                })
            },
        );

        if let Some(shape_key) = self.heap.builtin_shapes.lookup(def.type_name())
            && let Some(shape) = self.heap.builtin_shapes.get_shape_mut(shape_key)
        {
            shape.add_method(def.method_name().to_string(), method);
        }
    }

    /// Register a native protocol impl from an inventory descriptor.
    /// # Panics
    /// When the protocol name is not found in the program's registered protocols, or
    /// when the type name is not found in the heap's builtin shapes (unless there is a default).
    pub fn register_native_protocol_impl(&mut self, def: &crate::NativeProtocolImplDef) {
        let fn_value = self.new_native_fn(
            &format!("{}::{}::{}", def.protocol(), def.type_name(), def.method()),
            def.fn_ptr(),
        );
        let protocol_id = self
            .program
            .protocol_id_by_name(def.protocol())
            .unwrap_or_else(|| panic!("Protocol '{}' not registered", def.protocol()));
        let target_type = if def.type_name() == "*" {
            ShapeKey::Wildcard
        } else {
            self.heap
                .builtin_shapes
                .lookup(def.type_name())
                .unwrap_or_else(|| {
                    panic!(
                        "Type '{}' not found in builtin shapes for protocol impl {}::{}",
                        def.type_name(),
                        def.protocol(),
                        def.method()
                    )
                })
        };
        self.register_protocol_impl(protocol_id, target_type, def.method(), fn_value);
    }

    /// Collect and register all inventory-submitted native definitions.
    ///
    /// This handles enum/struct shapes, protocols, methods (with priority-based
    /// dedup), protocol implementations, native functions, and auto-registers
    /// zero-field enum variant globals (e.g. `Option::None`).
    pub fn collect_native_inventory(&mut self) {
        use crate::{
            NativeEnumDef, NativeFnDef, NativeMethodDef, NativeProtocolDef, NativeProtocolImplDef,
            NativeStructDef,
        };

        // 1. Register enum/struct shapes (skips pre-created ones).
        for def in inventory::iter::<NativeEnumDef> {
            self.register_native_enum(def);
        }
        for def in inventory::iter::<NativeStructDef> {
            self.register_native_struct(def);
        }

        // 2. Register protocols.
        // Native protocol IDs are assigned sequentially. The inventory crate
        // guarantees deterministic iteration order within a single build, so
        // IDs remain stable across runs of the same binary.
        for (native_protocol_counter, def) in
            inventory::iter::<NativeProtocolDef>.into_iter().enumerate()
        {
            let protocol_id = ProtocolId::Native(native_protocol_counter as u32);
            self.register_protocol(
                protocol_id,
                def.name().to_string(),
                def.methods().iter().map(|(m, _)| m.to_string()).collect(),
            );
        }

        // 3. Register native functions (sorted by name for deterministic order).
        let mut fn_defs: Vec<&NativeFnDef> = inventory::iter::<NativeFnDef>.into_iter().collect();
        fn_defs.sort_by_key(|def| def.name());
        for native_fn_def in &fn_defs {
            let name = native_fn_def.name();
            let fn_object = self.new_native_fn(&name, native_fn_def.fn_ptr());
            debug!("Defining global native function: {name}");
            self.set_global(name, fn_object);
        }

        // 4. Register methods (highest priority wins per type+method key).
        // Sort by (key ASC, priority DESC) so dedup_by (which keeps the first of each group)
        // retains the highest-priority entry.
        let mut method_defs: Vec<&NativeMethodDef> =
            inventory::iter::<NativeMethodDef>.into_iter().collect();
        method_defs.sort_by_key(|d| {
            (
                d.type_name(),
                d.method_name(),
                std::cmp::Reverse(d.priority()),
            )
        });
        method_defs
            .dedup_by(|a, b| a.type_name() == b.type_name() && a.method_name() == b.method_name());
        for def in &method_defs {
            self.register_native_method(def);
        }

        // 5. Register protocol impls (highest priority wins).
        let mut impl_defs: Vec<&NativeProtocolImplDef> = inventory::iter::<NativeProtocolImplDef>
            .into_iter()
            .collect();
        impl_defs.sort_by_key(|d| {
            (
                d.protocol(),
                d.type_name(),
                d.method(),
                std::cmp::Reverse(d.priority()),
            )
        });
        impl_defs.dedup_by(|a, b| {
            a.protocol() == b.protocol()
                && a.type_name() == b.type_name()
                && a.method() == b.method()
        });
        for def in &impl_defs {
            self.register_native_protocol_impl(def);
        }

        // 6. Auto-register zero-field enum variants as globals (e.g. Option::None).
        for def in inventory::iter::<NativeEnumDef> {
            if let Some(shape_key) = self.heap.builtin_shapes.lookup(def.name()) {
                for (idx, variant) in def.variants().iter().enumerate() {
                    if variant.fields.is_empty() {
                        let value = self.new_enum(shape_key, idx, vec![]);
                        self.set_global(format!("{}::{}", def.name(), variant.name), value);
                    }
                }
            }
        }
    }

    pub fn get_closure_decl(&self, id: HirId) -> Option<Rc<hir::FunctionDeclaration>> {
        self.program.get_closure_decl(id)
    }

    pub fn init_global_slots<'a>(
        &mut self,
        slot_entries: impl IntoIterator<Item = (&'a str, usize)>,
    ) {
        self.program.init_global_slots(slot_entries);
    }

    pub fn set_global(&mut self, name: String, value: TlangValue) {
        self.program.set_global(name, value);
    }

    /// Ensure a global slot exists for the given name and index.
    /// Extends the slots vec if needed.
    ///
    /// If the name is already mapped to an existing slot (e.g. a builtin
    /// like `log`), the existing mapping is preserved and not overwritten.
    /// Cross-module imports are accessed by slot index directly, so the
    /// name→slot registration is only needed for new, previously unknown names.
    pub fn ensure_global_slot(&mut self, name: &str, slot: usize) {
        if self.program.global_slots.len() <= slot {
            self.program.global_slots.resize(slot + 1, TlangValue::Nil);
        }
        self.program
            .global_slot_map
            .entry(name.to_string())
            .or_insert(slot);
    }

    /// Set a value directly at a specific global slot index.
    /// # Panics
    /// If the slot index is out of bounds of the allocated global slots.
    pub fn set_global_slot(&mut self, slot: usize, value: TlangValue) {
        assert!(
            slot < self.program.global_slots.len(),
            "attempted to set global slot {slot} but only {} slots allocated — this is a compiler bug",
            self.program.global_slots.len()
        );
        self.program.global_slots[slot] = value;
    }

    pub fn get_global(&self, name: &str) -> Option<TlangValue> {
        if let Some(&slot) = self.program.global_slot_map.get(name) {
            Some(self.program.global_slots[slot])
        } else {
            self.program.globals.get(name).copied()
        }
    }

    /// Register HirIds of compile-time-constant expressions for the constant pool.
    pub fn register_constant_pool_ids(&mut self, ids: std::collections::HashSet<HirId>) {
        self.program.constant_pool_ids.extend(ids);
    }

    /// Returns `true` if the given HirId is a constant pool expression.
    pub fn is_constant_pool_expr(&self, hir_id: HirId) -> bool {
        self.program.constant_pool_ids.contains(&hir_id)
    }

    /// Returns the cached constant value for the given HirId, if it exists.
    pub fn get_constant(&self, hir_id: HirId) -> Option<TlangValue> {
        self.program.constant_pool.get(&hir_id).copied()
    }

    /// Stores a value in the constant pool, keyed by HirId.
    pub fn set_constant(&mut self, hir_id: HirId, value: TlangValue) {
        self.program.constant_pool.insert(hir_id, value);
    }

    // ── ExecutionContext delegates ───────────────────────────────────────────

    /// # Panics
    #[allow(clippy::needless_pass_by_value)]
    pub fn panic(&self, message: String) -> ! {
        let mut call_stack = String::new();

        for entry in self.execution.call_stack.iter().rev() {
            match &entry.kind {
                CallStackKind::Function(decl) => {
                    call_stack.push_str(&format!(
                        "  at function {}:{}\n",
                        decl.name(),
                        entry.current_span.start
                    ));
                }
                CallStackKind::NativeFn(name) => {
                    call_stack.push_str(&format!("  at native function {name}\n"));
                }
                CallStackKind::Root => {
                    call_stack.push_str(&format!("  at root {}\n", entry.current_span.start));
                }
            }
        }

        panic!("{message}\n{call_stack}")
    }

    pub fn push_call_stack(&mut self, entry: CallStackEntry) {
        self.execution.push_call_stack(entry);
    }

    /// # Panics
    pub fn pop_call_stack(&mut self) -> CallStackEntry {
        self.execution.pop_call_stack()
    }

    /// # Panics
    pub fn current_call_frame(&self) -> &CallStackEntry {
        self.execution.current_call_frame()
    }

    /// # Panics
    pub fn current_call_frame_mut(&mut self) -> &mut CallStackEntry {
        self.execution.current_call_frame_mut()
    }

    /// # Panics
    pub fn set_current_span(&mut self, span: tlang_span::Span) {
        self.execution.set_current_span(span);
    }

    pub fn enter_scope<T>(&mut self, meta: &T)
    where
        T: hir::HirScope,
    {
        self.execution.enter_scope(meta);
    }

    pub fn exit_scope(&mut self) {
        self.execution.exit_scope();
    }

    pub fn current_scope(&self) -> &Scope {
        self.execution.current_scope()
    }

    pub fn push_value(&mut self, value: TlangValue) {
        self.execution.push_value(value);
    }

    pub fn set_let_binding(&mut self, value: TlangValue) -> usize {
        self.execution.set_let_binding(value)
    }

    pub fn init_var_index_after_params(&mut self, param_count: usize) {
        self.execution.init_var_index_after_params(param_count);
    }

    pub fn is_global_scope(&self) -> bool {
        self.execution.is_global_scope()
    }

    pub fn current_scope_has_slots(&self) -> bool {
        self.execution.current_scope_has_slots()
    }

    // ── Cross-cutting helpers ───────────────────────────────────────────────

    /// Register the call handler function pointer. Called by the `VM`
    /// during initialization so that native functions can invoke callables
    /// via [`VMState::call`].
    pub fn register_call_fn(&mut self, call_fn: CallFn) {
        self.call_fn = call_fn;
    }

    /// Call a `TlangValue` that references a callable (function, closure, or
    /// native function) with the given arguments.
    ///
    /// This is the primary way for native functions to invoke user-defined
    /// callables. The call handler must be registered by the `VM`
    /// before this method is used.
    ///
    /// # Panics
    /// Panics if no call handler has been registered.
    pub fn call(&mut self, callee: TlangValue, args: &[TlangValue]) -> TlangValue {
        (self.call_fn)(self, callee, args)
    }

    /// # Panics
    pub fn is_truthy(&mut self, value: TlangValue) -> bool {
        if !value.is_object() {
            return value.is_truthy();
        }

        let type_shape_key = self.type_shape_key_of(value);
        if let Some(truthy_id) = self.protocol_id_by_name("Truthy")
            && let Some(truthy_fn) = self.get_protocol_impl(truthy_id, type_shape_key, "truthy")
        {
            let result = if matches!(self.get_object(truthy_fn), Some(TlangObjectKind::NativeFn)) {
                let id = truthy_fn.get_object_id().unwrap();
                *self.call_native_fn(id, &[value]).unwrap().value().unwrap()
            } else {
                self.call(truthy_fn, &[value])
            };
            return result.is_truthy();
        }

        match self
            .heap
            .get_object_by_id(value.get_object_id().unwrap())
            .unwrap()
        {
            TlangObjectKind::Fn(_) | TlangObjectKind::NativeFn | TlangObjectKind::Closure(_) => {
                true
            }
            TlangObjectKind::String(s) => !s.is_empty(),
            TlangObjectKind::Slice(s) => !s.is_empty(),
            TlangObjectKind::Cell(c) => self.is_truthy(c.value),
            TlangObjectKind::Struct(s) => {
                if s.shape() == self.heap.builtin_shapes.string_buf {
                    return self
                        .heap
                        .get_object(s[0])
                        .and_then(|o| o.as_str())
                        .map(|str| !str.is_empty())
                        .unwrap_or(false);
                }
                !s.is_empty()
            }
            TlangObjectKind::Enum(e) => {
                e.field_values.is_empty()
                    || e.field_values.clone().iter().all(|v| self.is_truthy(*v))
            }
        }
    }

    pub fn debug_stringify_scope_stack(&mut self) -> String {
        let scope_stack = self.execution.scope_stack.clone();
        let mut out = "[\n".to_string();
        for scope in scope_stack.iter() {
            out.push_str("  {\n");
            for entry in scope_stack.get_scope_locals(scope) {
                out.push_str("    ");
                out.push_str(self.stringify(*entry).as_str());
                out.push_str(",\n");
            }
            out.push_str("  },\n");
        }
        out.push_str("]\n");
        out
    }

    fn stringify_list_values(&mut self, values: &[TlangValue]) -> String {
        let values = values
            .iter()
            .map(|v| self.stringify(*v))
            .collect::<Vec<String>>()
            .join(", ");
        format!("[{values}]")
    }

    fn stringify_struct_data(&mut self, shape_key: ShapeKey, values: &[TlangValue]) -> String {
        if shape_key == self.heap.builtin_shapes.list {
            return self.stringify_list_values(values);
        }

        if shape_key == self.heap.builtin_shapes.regex {
            let source = self.stringify(values[0]);
            let flags = self.stringify(values[1]);
            return format!("/{source}/{flags}");
        }

        if shape_key == self.heap.builtin_shapes.string_buf {
            return self
                .heap
                .get_object(values[0])
                .and_then(|o| o.as_str())
                .unwrap_or("")
                .to_string();
        }

        let shape = self
            .heap
            .get_shape_by_key(shape_key)
            .and_then(|s| s.get_struct_shape())
            .unwrap();

        let shape_name = shape.name.clone();
        let has_fields = shape.has_fields();
        let is_list_like = has_fields && shape.has_consecutive_integer_fields();
        let fields = if has_fields {
            shape.get_fields()
        } else {
            vec![]
        };

        if is_list_like {
            return format!("{} {}", shape_name, self.stringify_list_values(values));
        }

        let mut result = String::new();
        result.push_str(&shape_name);
        result.push_str(" { ");

        if !has_fields {
            result.push('}');
            return result;
        }

        let mut fields = fields;
        fields.sort();
        result.push_str(
            &fields
                .into_iter()
                .map(|(field, idx)| format!("{}: {}", field, self.stringify(values[idx])))
                .collect::<Vec<String>>()
                .join(", "),
        );

        result.push_str(" }");
        result
    }

    fn stringify_enum_data(
        &mut self,
        shape_key: ShapeKey,
        variant: usize,
        field_values: &[TlangValue],
    ) -> String {
        let shape = self
            .heap
            .get_shape_by_key(shape_key)
            .and_then(|s| s.get_enum_shape())
            .unwrap();
        let variant_shape = &shape.variants[variant];

        let shape_name = shape.name.clone();
        let variant_name = variant_shape.name.clone();
        let field_entries: Vec<(String, usize)> = variant_shape
            .field_map
            .iter()
            .map(|(k, v)| (k.clone(), *v))
            .collect();

        let mut result = String::new();
        result.push_str(&shape_name);
        result.push_str("::");
        result.push_str(&variant_name);

        if field_entries.is_empty() {
            return result;
        }

        result.push('(');
        for (i, (field, idx)) in field_entries.iter().enumerate() {
            if i > 0 {
                result.push_str(", ");
            }
            result.push_str(field);
            result.push_str(": ");
            result.push_str(&self.stringify(field_values[*idx]));
        }
        result.push(')');
        result
    }

    /// # Panics
    pub fn stringify(&mut self, value: TlangValue) -> String {
        match value {
            TlangValue::Object(_id) => {
                // Check for a concrete Display::to_string impl (not the default wildcard).
                // Use type_shape_key_of to get the ShapeKey for all object types
                // (including String/Slice which don't have shapes on TlangObjectKind).
                let display_fn = {
                    let type_shape_key = self.type_shape_key_of(value);
                    if let Some(display_id) = self.program.protocol_id_by_name("Display") {
                        self.program.get_concrete_protocol_impl(
                            display_id,
                            type_shape_key,
                            "to_string",
                        )
                    } else {
                        None
                    }
                };

                if let Some(fn_value) = display_fn {
                    let result = self.call(fn_value, &[value]);
                    return self.stringify(result);
                }

                self.stringify_default(value)
            }
            _ => value.to_string(),
        }
    }

    /// Default stringify without Display protocol dispatch.
    /// Used by the native default `Display::to_string` implementation to
    /// avoid infinite recursion.
    pub fn stringify_default(&mut self, value: TlangValue) -> String {
        match value {
            TlangValue::Object(id) => self.stringify_object(id),
            _ => value.to_string(),
        }
    }

    fn stringify_object(&mut self, id: TlangObjectId) -> String {
        let obj = self
            .heap
            .get_object_by_id(id)
            .unwrap_or_else(|| self.panic(format!("Object with id {} not found", id)));

        // Extract all data we need from the object reference before dropping
        // the borrow, so we can call &mut self methods (stringify, etc.).
        enum ObjData {
            String(String),
            Struct(ShapeKey, Vec<TlangValue>),
            Enum(ShapeKey, usize, Vec<TlangValue>),
            Slice(TlangSlice),
            Closure(HirId),
            Fn(HirId),
            NativeFn,
            Cell(TlangValue),
        }

        let data = match obj {
            TlangObjectKind::String(s) => ObjData::String(s.clone()),
            TlangObjectKind::Struct(s) => ObjData::Struct(s.shape(), s.values().to_vec()),
            TlangObjectKind::Enum(e) => ObjData::Enum(e.shape(), e.variant, e.field_values.clone()),
            TlangObjectKind::Slice(s) => ObjData::Slice(*s),
            TlangObjectKind::Closure(c) => ObjData::Closure(c.id),
            TlangObjectKind::Fn(hir_id) => ObjData::Fn(*hir_id),
            TlangObjectKind::NativeFn => ObjData::NativeFn,
            TlangObjectKind::Cell(cell) => ObjData::Cell(cell.get()),
        };

        match data {
            ObjData::String(s) => s,
            ObjData::Struct(shape_key, values) => self.stringify_struct_data(shape_key, &values),
            ObjData::Enum(shape_key, variant, field_values) => {
                self.stringify_enum_data(shape_key, variant, &field_values)
            }
            ObjData::Slice(s) => {
                let values: Vec<TlangValue> = self.heap.get_slice_values(s).to_vec();
                let parts: Vec<String> = values.iter().map(|v| self.stringify(*v)).collect();
                format!("&[{}]", parts.join(", "))
            }
            ObjData::Closure(closure_id) => {
                let fn_decl = self.program.closures.get(&closure_id).unwrap().clone();
                format!(
                    "fn {}#{}({})",
                    fn_decl.name(),
                    closure_id,
                    fn_decl
                        .parameters
                        .iter()
                        .map(|p| p.name.as_str())
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            }
            ObjData::Fn(hir_id) => {
                let fn_decl = self.program.fn_decls.get(&hir_id).unwrap().clone();
                format!(
                    "fn {}#{}({})",
                    fn_decl.name(),
                    hir_id,
                    fn_decl
                        .parameters
                        .iter()
                        .map(|p| p.name.as_str())
                        .collect::<Vec<_>>()
                        .join(", "),
                )
            }
            ObjData::NativeFn => {
                if let Some(meta) = self.heap.native_fns_meta.get(&id) {
                    format!("fn {}(...) {{ *native* }}", meta.name)
                } else {
                    "fn anonymous(...) { *native* }".to_string()
                }
            }
            ObjData::Cell(inner) => self.stringify(inner),
        }
    }
}

impl Default for VMState {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_memory_stats_initial() {
        let state = VMState::new();
        let stats = state.memory_stats();

        assert_eq!(stats.objects_allocated, 0);
        assert_eq!(stats.objects_deallocated, 0);
        assert_eq!(stats.live_objects(), 0);
    }

    #[test]
    fn test_memory_stats_after_allocation() {
        let mut state = VMState::new();

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
        let mut state = VMState::new();

        // Allocate objects
        let obj1 = state.new_string("first".to_string());
        let obj2 = state.new_string("second".to_string());
        let _obj3 = state.new_string("third".to_string());

        assert_eq!(state.object_count(), 3);

        // Remove an object
        let id1 = obj1.get_object_id().unwrap();
        let removed = state.heap.remove_object(id1);
        assert!(removed.is_some());

        let stats = state.memory_stats();
        assert_eq!(stats.objects_allocated, 3);
        assert_eq!(stats.objects_deallocated, 1);
        assert_eq!(stats.live_objects(), 2);
        assert_eq!(state.object_count(), 2);

        // Object should no longer exist
        assert!(!state.heap.contains_object(id1));
        assert!(state.get_object(obj1).is_none());

        // Other objects should still exist
        assert!(state.get_object(obj2).is_some());
    }

    #[test]
    fn test_remove_nonexistent_object() {
        let mut state = VMState::new();

        // Try to remove an object that doesn't exist
        let removed = state.heap.remove_object(999.into());
        assert!(removed.is_none());

        // Stats should not change
        let stats = state.memory_stats();
        assert_eq!(stats.objects_deallocated, 0);
    }

    #[test]
    fn test_contains_object() {
        let mut state = VMState::new();

        let obj = state.new_string("test".to_string());
        let id = obj.get_object_id().unwrap();

        assert!(state.heap.contains_object(id));
        assert!(!state.heap.contains_object(999.into()));
    }

    #[test]
    fn test_gc_roots_includes_globals() {
        let value = TlangValue::I64(42);
        let mut state = VMState::new();
        state.set_global("test".to_string(), value);

        let roots: Vec<_> = state.gc_roots().collect();
        assert!(roots.contains(&value));
    }

    #[test]
    fn test_mark_phase_finds_reachable() {
        let mut state = VMState::new();

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
        let mut state = VMState::new();

        // Create an unreachable object (no root reference)
        let orphan = state.new_string("orphan".to_string());
        let orphan_id = orphan.get_object_id().unwrap();

        let marked = state.mark_reachable();

        // Orphan should NOT be in marked set
        assert!(!marked.contains(&orphan_id));
    }

    #[test]
    fn test_sweep_removes_unreachable() {
        let mut state = VMState::new();

        // Create an object but don't make it a root
        let orphan = state.new_string("orphan".to_string());
        let orphan_id = orphan.get_object_id().unwrap();

        // Run GC phases
        let marked = state.mark_reachable();
        let collected = state.sweep_unreachable(&marked);

        assert_eq!(collected, 1);
        assert!(!state.heap.contains_object(orphan_id));
    }

    #[test]
    fn test_gc_collects_unreachable_objects() {
        let mut state = VMState::new();

        // Create objects but don't make them roots
        let _orphan1 = state.new_string("orphan1".to_string());
        let _orphan2 = state.new_string("orphan2".to_string());

        assert_eq!(state.object_count(), 2);

        // Run GC - both should be collected
        let collected = state.collect_garbage();

        assert_eq!(collected, 2);
        assert_eq!(state.object_count(), 0);
        assert_eq!(state.memory_stats().gc_collections, 1);
    }

    #[test]
    fn test_gc_preserves_reachable_objects() {
        let mut state = VMState::new();

        // Create an object and make it a root via globals
        let reachable = state.new_string("keep_me".to_string());
        state.set_global("my_string".to_string(), reachable);

        // Create unreachable objects
        let _orphan = state.new_string("orphan".to_string());

        assert_eq!(state.object_count(), 2);

        // Run GC - only orphan should be collected
        let collected = state.collect_garbage();

        assert_eq!(collected, 1);
        assert_eq!(state.object_count(), 1);

        // The reachable object should still exist
        let id = reachable.get_object_id().unwrap();
        assert!(state.heap.contains_object(id));
    }

    #[test]
    fn test_gc_collects_temporary_objects() {
        let mut state = VMState::new();

        // Simulate a loop that creates temporary objects
        for _ in 0..10_000 {
            let _temp = state.new_string("temp".to_string());
            // Don't store temp anywhere - it becomes garbage
        }

        // After GC, heap should be empty (no roots)
        state.collect_garbage();
        assert_eq!(state.object_count(), 0);
    }

    #[test]
    fn test_ensure_global_slot_does_not_overwrite_existing_mapping() {
        let mut state = VMState::new();

        // Simulate a builtin registered at slot 0 with slot-based lookup.
        state.init_global_slots([("log", 0)]);
        let log_value = TlangValue::I64(42);
        state.set_global_slot(0, log_value);

        // A cross-module import tries to register the same name "log" at a
        // different slot (slot 1). The existing mapping must be preserved so
        // the builtin is not silently shadowed.
        state.ensure_global_slot("log", 1);

        // The slot 1 should be allocated (resize happened), but the name "log"
        // must still point to slot 0, keeping the builtin value intact.
        assert_eq!(
            state.get_global("log"),
            Some(log_value),
            "ensure_global_slot must not overwrite an existing name→slot mapping"
        );
    }

    #[test]
    fn test_ensure_global_slot_registers_new_name() {
        let mut state = VMState::new();

        // Register a new name that doesn't yet exist in the slot map.
        state.ensure_global_slot("my_fn", 0);

        // The slot vec should be extended to accommodate index 0.
        let val = TlangValue::I64(99);
        state.set_global_slot(0, val);

        assert_eq!(state.get_global("my_fn"), Some(val));
    }

    #[test]
    fn test_ensure_global_slot_idempotent_same_slot() {
        let mut state = VMState::new();

        // Registering the same name at the same slot twice must be a no-op.
        state.ensure_global_slot("fn_a", 0);
        state.ensure_global_slot("fn_a", 0);

        let val = TlangValue::I64(7);
        state.set_global_slot(0, val);
        assert_eq!(state.get_global("fn_a"), Some(val));
    }
}
