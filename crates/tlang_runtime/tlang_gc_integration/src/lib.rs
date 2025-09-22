use std::collections::HashMap;
use std::rc::Rc;

use tlang_gc::{GcCollector, GcConfig, GcRootProvider, GcTracer};
use tlang_hir::hir;
use tlang_span::HirId;
use tlang_memory::{
    scope::ScopeStack, 
    shape::{ShapeKey, TlangShape},
    state::{BuiltinShapes, CallStackEntry, NativeFnMeta},
    value::{
        TlangValue,
        function::TlangNativeFn,
        object::{TlangObjectId, TlangObjectKind},
    },
};

/// GC-managed interpreter state that replaces the slab-based allocation
pub struct GcInterpreterState {
    pub scope_stack: ScopeStack,
    closures: HashMap<HirId, Rc<hir::FunctionDeclaration>>,
    gc_heap: GcCollector<TlangObjectKind>,
    shapes: HashMap<ShapeKey, TlangShape>,
    fn_decls: HashMap<HirId, Rc<hir::FunctionDeclaration>>,
    struct_decls: HashMap<String, Rc<hir::StructDeclaration>>,
    enum_decls: HashMap<String, Rc<hir::EnumDeclaration>>,
    call_stack: Vec<CallStackEntry>,
    globals: HashMap<String, TlangValue>,
    pub builtin_shapes: BuiltinShapes,
    native_fns: HashMap<TlangObjectId, TlangNativeFn>,
    native_fns_meta: HashMap<TlangObjectId, NativeFnMeta>,
}

impl GcInterpreterState {
    pub fn new() -> Self {
        Self::with_gc_config(GcConfig::default())
    }

    pub fn with_gc_config(gc_config: GcConfig) -> Self {
        let mut call_stack = Vec::with_capacity(1000);

        call_stack.push(CallStackEntry {
            kind: tlang_memory::state::CallStackKind::Root,
            tail_call: None,
            current_span: tlang_span::Span::default(),
        });

        Self {
            scope_stack: ScopeStack::default(),
            closures: HashMap::with_capacity(100),
            gc_heap: GcCollector::new_mark_sweep(gc_config),
            struct_decls: HashMap::with_capacity(100),
            enum_decls: HashMap::with_capacity(100),
            fn_decls: HashMap::with_capacity(1000),
            shapes: HashMap::with_capacity(100),
            call_stack,
            globals: HashMap::with_capacity(100),
            builtin_shapes: BuiltinShapes::default(),
            native_fns: HashMap::with_capacity(100),
            native_fns_meta: HashMap::with_capacity(100),
        }
    }

    /// Allocate a new object in GC-managed memory
    pub fn new_object(&mut self, kind: TlangObjectKind) -> TlangValue {
        // Create a temporary root provider to avoid borrowing conflicts
        let root_provider = TempRootProvider {
            scope_stack: &self.scope_stack,
            globals: &self.globals,
            call_stack: &self.call_stack,
        };
        
        let handle = self.gc_heap.allocate_with_gc(kind, &root_provider);
        TlangValue::new_object(handle)
    }

    /// Get a reference to an object
    pub fn get_object_by_id(&self, id: TlangObjectId) -> Option<&TlangObjectKind> {
        self.gc_heap.get(id)
    }

    /// Get a mutable reference to an object
    pub fn get_object_by_id_mut(&mut self, id: TlangObjectId) -> Option<&mut TlangObjectKind> {
        self.gc_heap.get_mut(id)
    }

    /// Get object from a TlangValue
    pub fn get_object(&self, value: TlangValue) -> Option<&TlangObjectKind> {
        value.get_object_id()
            .and_then(|id| self.get_object_by_id(id))
    }

    /// Force a garbage collection cycle
    pub fn collect_garbage(&mut self) {
        let root_provider = TempRootProvider {
            scope_stack: &self.scope_stack,
            globals: &self.globals,
            call_stack: &self.call_stack,
        };
        
        let result = self.gc_heap.force_collect(&root_provider);
        log::info!(
            "GC: Collected {} objects ({} bytes) in {}μs", 
            result.objects_collected,
            result.bytes_freed,
            result.duration_micros
        );
    }

    /// Get GC statistics
    pub fn gc_stats(&self) -> &tlang_gc::GcStats {
        self.gc_heap.stats()
    }

    /// Get number of objects currently in GC heap
    pub fn object_count(&self) -> usize {
        self.gc_heap.object_count()
    }

    /// Get total allocated bytes in GC heap
    pub fn allocated_bytes(&self) -> usize {
        self.gc_heap.allocated_bytes()
    }

    // Forward other methods from original InterpreterState
    pub fn set_global(&mut self, name: String, value: TlangValue) {
        self.globals.insert(name, value);
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

    /// Create a new enum object
    pub fn new_enum(
        &mut self,
        shape: ShapeKey,
        variant: usize,
        values: Vec<TlangValue>,
    ) -> TlangValue {
        self.new_object(TlangObjectKind::Enum(
            tlang_memory::value::object::TlangEnum::new(shape, variant, values)
        ))
    }

    /// Create a new closure object
    pub fn new_closure(&mut self, decl: &hir::FunctionDeclaration) -> TlangValue {
        self.closures
            .entry(decl.hir_id)
            .or_insert_with(|| decl.clone().into());

        self.new_object(TlangObjectKind::Closure(
            tlang_memory::value::object::TlangClosure {
                id: decl.hir_id,
                scope_stack: self.scope_stack.scopes.clone(),
            }
        ))
    }

    /// Create a new native function - this method signature needs to match the expected interface
    pub fn new_native_fn<F>(&mut self, name: &str, f: F) -> TlangValue
    where
        F: Fn(&mut tlang_memory::InterpreterState, &[TlangValue]) -> tlang_memory::value::function::NativeFnReturn + 'static,
    {
        // For now, we'll create a wrapper that panics if called directly
        // This is a temporary solution until we can refactor the native function system
        let fn_object = self.new_object(TlangObjectKind::NativeFn);
        
        let object_id = fn_object.get_object_id().unwrap();
        
        // Store a dummy function that indicates this needs special handling
        self.native_fns_meta.insert(
            object_id,
            NativeFnMeta {
                name: name.to_string(),
            },
        );

        fn_object
    }

    /// Create a new native method - placeholder implementation
    pub fn new_native_method<F>(&mut self, name: &str, f: F) -> tlang_memory::shape::TlangStructMethod
    where
        F: Fn(&mut tlang_memory::InterpreterState, TlangValue, &[TlangValue]) -> tlang_memory::value::function::NativeFnReturn + 'static,
    {
        let fn_val = self.new_native_fn(name, move |state, args| f(state, args[0], &args[1..]));
        tlang_memory::shape::TlangStructMethod::from(fn_val)
    }

    /// Call a native function - placeholder for now
    pub fn call_native_fn(
        &mut self,
        fn_id: TlangObjectId,
        args: &[TlangValue],
    ) -> Option<tlang_memory::value::function::NativeFnReturn> {
        // For now, return None to indicate the function couldn't be called
        // This will need proper implementation in the future
        log::warn!("Native function call not yet supported in GC mode: {}", fn_id);
        None
    }

    /// Get closure declaration
    pub fn get_closure_decl(&self, id: HirId) -> Option<Rc<hir::FunctionDeclaration>> {
        self.closures.get(&id).cloned()
    }

    /// Get object from value (convenience method)
    pub fn get_object(&self, value: TlangValue) -> Option<&TlangObjectKind> {
        value.get_object_id()
            .and_then(|id| self.get_object_by_id(id))
    }

    /// Get struct from value
    pub fn get_struct(&self, value: TlangValue) -> Option<&tlang_memory::value::object::TlangStruct> {
        self.get_object(value)?.get_struct()
    }

    /// Get mutable struct from value
    pub fn get_struct_mut(&mut self, value: TlangValue) -> Option<&mut tlang_memory::value::object::TlangStruct> {
        let object_id = value.get_object_id()?;
        self.get_object_by_id_mut(object_id)?.get_struct_mut()
    }

    /// Get enum from value
    pub fn get_enum(&self, value: TlangValue) -> Option<&tlang_memory::value::object::TlangEnum> {
        self.get_object(value)?.get_enum()
    }

    /// Get slice from value
    pub fn get_slice(&self, value: TlangValue) -> Option<tlang_memory::value::object::TlangSlice> {
        self.get_object(value)?.get_slice()
    }

    /// Get slice value at index
    pub fn get_slice_value(&self, slice: tlang_memory::value::object::TlangSlice, index: usize) -> Option<TlangValue> {
        if index < slice.len() {
            let actual_index = slice.start() + index;
            if let Some(slice_obj) = self.get_object(slice.of()) {
                if let Some(struct_obj) = slice_obj.get_struct() {
                    return struct_obj.get(actual_index);
                }
            }
        }
        None
    }

    /// Get slice values
    pub fn get_slice_values(&self, slice: tlang_memory::value::object::TlangSlice) -> Vec<TlangValue> {
        let mut values = Vec::with_capacity(slice.len());
        for i in 0..slice.len() {
            if let Some(value) = self.get_slice_value(slice, i) {
                values.push(value);
            }
        }
        values
    }

    /// Get shape by key
    pub fn get_shape_by_key(&self, key: ShapeKey) -> Option<&TlangShape> {
        self.shapes.get(&key)
    }

    /// Enter a new scope
    pub fn enter_scope(&mut self, meta: tlang_hir::hir::HirScope) {
        self.scope_stack.enter_scope(meta);
    }

    /// Exit current scope
    pub fn exit_scope(&mut self) {
        self.scope_stack.exit_scope();
    }

    /// Push call stack entry
    pub fn push_call_stack(&mut self, entry: CallStackEntry) {
        self.call_stack.push(entry);
    }

    /// Pop call stack entry
    pub fn pop_call_stack(&mut self) -> Option<CallStackEntry> {
        self.call_stack.pop()
    }

    /// Panic with call stack information
    #[allow(clippy::needless_pass_by_value)]
    pub fn panic(&self, message: String) -> ! {
        let mut call_stack = String::new();

        for entry in self.call_stack.iter().rev() {
            match &entry.kind {
                tlang_memory::state::CallStackKind::Function(decl) => {
                    call_stack.push_str(&format!(
                        "  at function {}:{}\n",
                        decl.name(),
                        entry.current_span.start
                    ));
                }
                tlang_memory::state::CallStackKind::NativeFn(name) => {
                    call_stack.push_str(&format!("  at native function {name}:0:0\n"));
                }
                tlang_memory::state::CallStackKind::Root => {
                    call_stack.push_str(&format!("  at root {}\n", entry.current_span.start));
                }
            }
        }

        panic!("{message}\n{call_stack}")
    }
}

impl Default for GcInterpreterState {
    fn default() -> Self {
        Self::new()
    }
}

/// Implement Resolver trait for GcInterpreterState
impl tlang_memory::Resolver for GcInterpreterState {
    fn resolve_value(&self, path: &hir::Path) -> Option<TlangValue> {
        if !path.res.is_value() {
            return None;
        }

        let value = self
            .scope_stack
            .resolve_value(path)
            .or_else(|| self.globals.get(&path.to_string()).copied());

        log::debug!(
            "Resolved path: \"{}\" ({:?}), got: {:?}",
            path,
            path.res,
            value.map(|v| format!("{:?}", v)) // Simplified stringify for now
        );

        value
    }
}

/// Temporary root provider to avoid borrowing conflicts
struct TempRootProvider<'a> {
    scope_stack: &'a ScopeStack,
    globals: &'a HashMap<String, TlangValue>,
    call_stack: &'a [CallStackEntry],
}

impl<'a> GcRootProvider for TempRootProvider<'a> {
    fn trace_roots(&self, tracer: &mut dyn GcTracer) {
        // Trace global variables
        for value in self.globals.values() {
            if let Some(object_id) = value.get_object_id() {
                tracer.mark_object(object_id);
            }
        }

        // Trace scope stack variables
        trace_scope_stack(self.scope_stack, tracer);

        // Trace call stack (closures in native functions, etc.)
        for entry in self.call_stack {
            if let Some(tail_call) = &entry.tail_call {
                if let Some(object_id) = tail_call.callee.get_object_id() {
                    tracer.mark_object(object_id);
                }
                for arg in &tail_call.args {
                    if let Some(object_id) = arg.get_object_id() {
                        tracer.mark_object(object_id);
                    }
                }
            }
        }
    }
}

/// Helper function to trace all values in the scope stack
fn trace_scope_stack(scope_stack: &ScopeStack, tracer: &mut dyn GcTracer) {
    // We need to access the memory in scope stack, but it's private.
    // For now, we'll use a visitor pattern or extend the ScopeStack API.
    // This is a simplified version - in practice, we'd need proper access to the scope memory.
    
    // For each scope, we need to trace its local variables
    for scope in &scope_stack.scopes {
        // Get the local variables for this scope
        let locals = scope_stack.get_scope_locals(scope);
        
        // Trace all values in this scope
        for value in locals {
            if let Some(object_id) = value.get_object_id() {
                tracer.mark_object(object_id);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tlang_memory::value::object::{TlangStruct, TlangObjectKind};
    use tlang_memory::shape::ShapeKey;
    use tlang_gc::GcConfig;

    #[test]
    fn test_gc_integration_basic() {
        let mut gc_state = GcInterpreterState::new();
        
        // Allocate some objects
        let struct_obj = TlangObjectKind::Struct(TlangStruct::new(
            ShapeKey::new_hir_id(tlang_span::HirId::new(1)),
            vec![TlangValue::I64(42), TlangValue::I64(100)]
        ));
        
        let handle1 = gc_state.new_object(struct_obj);
        assert_eq!(gc_state.object_count(), 1);
        
        // The object should be accessible
        assert!(gc_state.get_object_by_id(handle1.get_object_id().unwrap()).is_some());
    }

    #[test]
    fn test_gc_integration_collection() {
        let mut gc_state = GcInterpreterState::with_gc_config(GcConfig {
            allocation_threshold: 2,
            heap_size_threshold: 1000000,
            debug_logging: true,
        });
        
        // Allocate objects that will become unreachable
        let _handle1 = gc_state.new_object(TlangObjectKind::String("test1".to_string()));
        let _handle2 = gc_state.new_object(TlangObjectKind::String("test2".to_string()));
        
        assert_eq!(gc_state.object_count(), 2);
        
        // Force garbage collection - since there are no roots, everything should be collected
        gc_state.collect_garbage();
        
        assert_eq!(gc_state.object_count(), 0);
    }

    #[test]
    fn test_gc_integration_globals() {
        let mut gc_state = GcInterpreterState::new();
        
        // Allocate an object and store it in globals (root)
        let handle = gc_state.new_object(TlangObjectKind::String("global_test".to_string()));
        gc_state.set_global("test_var".to_string(), handle);
        
        // Allocate another object that's not rooted
        let _unrooted = gc_state.new_object(TlangObjectKind::String("unrooted".to_string()));
        
        assert_eq!(gc_state.object_count(), 2);
        
        // Force garbage collection - the global should survive, unrooted should be collected
        gc_state.collect_garbage();
        
        assert_eq!(gc_state.object_count(), 1);
        
        // The global object should still be accessible
        assert!(gc_state.get_object_by_id(handle.get_object_id().unwrap()).is_some());
    }
}