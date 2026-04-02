use std::collections::{HashMap, HashSet};
use std::rc::Rc;

use tlang_hir as hir;
use tlang_span::HirId;

use crate::shape::{ProtocolId, ShapeKey};
use crate::value::TlangValue;

/// Static program metadata: HIR declarations and the global variable namespace.
pub struct Program {
    pub(crate) fn_decls: HashMap<HirId, Rc<hir::FunctionDeclaration>>,
    struct_decls: HashMap<String, Rc<hir::StructDeclaration>>,
    enum_decls: HashMap<String, Rc<hir::EnumDeclaration>>,
    /// Lazy cache of function declarations for closures, keyed by HirId.
    /// Populated on first closure creation; semantically identical to fn_decls.
    pub(crate) closures: HashMap<HirId, Rc<hir::FunctionDeclaration>>,
    /// Indexed storage for builtin/global symbols with pre-assigned slots.
    pub(crate) global_slots: Vec<TlangValue>,
    /// Maps builtin symbol names to their slot indices in `global_slots`.
    pub(crate) global_slot_map: HashMap<String, usize>,
    /// Fallback HashMap for dynamic globals (user-defined top-level fns, JS bindings).
    pub(crate) globals: HashMap<String, TlangValue>,
    /// Protocol definitions: protocol ID → list of method names
    pub(crate) protocols: HashMap<ProtocolId, Vec<String>>,
    /// Protocol constraints: protocol ID → list of constraint protocol IDs
    pub(crate) protocol_constraints: HashMap<ProtocolId, Vec<ProtocolId>>,
    /// Reverse lookup: method_name → protocol ID
    pub(crate) protocol_method_to_protocol: HashMap<String, ProtocolId>,
    /// Protocol implementations: (protocol_id, type_shape_key, method_name) → fn value.
    /// `ShapeKey::Wildcard` represents a default implementation that applies to all types
    /// without a more specific implementation registered.
    pub(crate) protocol_impls: HashMap<(ProtocolId, ShapeKey, String), TlangValue>,
    /// Name-based index for looking up protocol IDs by name at call sites
    pub(crate) protocol_name_to_id: HashMap<String, ProtocolId>,
    /// HirIds of expressions whose values are compile-time constants (e.g. tagged
    /// string parts lists). These are evaluated once and cached for the program's
    /// lifetime, giving singleton semantics (same object identity on every access).
    pub(crate) constant_pool_ids: HashSet<HirId>,
    /// Lazily populated cache of constant values, keyed by HirId.
    pub(crate) constant_pool: HashMap<HirId, TlangValue>,
}

impl Program {
    pub(crate) fn new() -> Self {
        Self {
            fn_decls: HashMap::with_capacity(1000),
            struct_decls: HashMap::with_capacity(100),
            enum_decls: HashMap::with_capacity(100),
            closures: HashMap::with_capacity(100),
            global_slots: Vec::new(),
            global_slot_map: HashMap::new(),
            globals: HashMap::with_capacity(100),
            protocols: HashMap::new(),
            protocol_constraints: HashMap::new(),
            protocol_method_to_protocol: HashMap::new(),
            protocol_impls: HashMap::new(),
            protocol_name_to_id: HashMap::new(),
            constant_pool_ids: HashSet::new(),
            constant_pool: HashMap::new(),
        }
    }

    pub fn get_fn_decl(&self, id: HirId) -> Option<Rc<hir::FunctionDeclaration>> {
        self.fn_decls.get(&id).cloned()
    }

    pub fn set_fn_decl(&mut self, id: HirId, decl: Rc<hir::FunctionDeclaration>) {
        self.fn_decls.insert(id, decl);
    }

    pub fn get_struct_decl_by_name(&self, name: &str) -> Option<Rc<hir::StructDeclaration>> {
        self.struct_decls.get(name).cloned()
    }

    pub fn get_enum_decl_by_name(&self, name: &str) -> Option<Rc<hir::EnumDeclaration>> {
        self.enum_decls.get(name).cloned()
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

    pub fn get_closure_decl(&self, id: HirId) -> Option<Rc<hir::FunctionDeclaration>> {
        self.closures.get(&id).cloned()
    }

    /// Initialize the global slots Vec from name→slot mappings for builtin symbols.
    /// Must be called before any `set_global` calls for builtin symbols.
    pub fn init_global_slots<'a>(
        &mut self,
        slot_entries: impl IntoIterator<Item = (&'a str, usize)>,
    ) {
        let entries: Vec<(&'a str, usize)> = slot_entries.into_iter().collect();
        let max_slot = entries
            .iter()
            .map(|(_, i)| i)
            .max()
            .map(|m| m + 1)
            .unwrap_or(0);
        self.global_slot_map.clear();
        self.global_slots = vec![TlangValue::Nil; max_slot];
        for (name, i) in entries {
            self.global_slot_map.insert(name.to_string(), i);
        }
    }

    pub fn set_global(&mut self, name: String, value: TlangValue) {
        if let Some(&slot) = self.global_slot_map.get(&name) {
            self.global_slots[slot] = value;
        } else {
            self.globals.insert(name, value);
        }
    }

    /// Returns an iterator over GC roots owned by the program (global slots and globals).
    pub fn gc_roots(&self) -> impl Iterator<Item = TlangValue> + '_ {
        self.global_slots
            .iter()
            .copied()
            .chain(self.globals.values().copied())
            .chain(self.protocol_impls.values().copied())
            .chain(self.constant_pool.values().copied())
    }

    pub fn register_protocol(
        &mut self,
        id: ProtocolId,
        name: String,
        methods: Vec<String>,
        constraints: Vec<ProtocolId>,
    ) {
        for method in &methods {
            self.protocol_method_to_protocol.insert(method.clone(), id);
        }
        self.protocols.insert(id, methods);
        self.protocol_constraints.insert(id, constraints);
        self.protocol_name_to_id.insert(name, id);
    }

    pub fn register_protocol_impl(
        &mut self,
        protocol: ProtocolId,
        target_type: ShapeKey,
        method: &str,
        fn_value: TlangValue,
    ) {
        self.protocol_impls
            .insert((protocol, target_type, method.to_string()), fn_value);
    }

    pub fn get_protocol_impl(
        &self,
        protocol: ProtocolId,
        target_type: Option<ShapeKey>,
        method: &str,
    ) -> Option<TlangValue> {
        // First try an exact match for the concrete type (if known)
        if let Some(key) = target_type
            && let Some(val) = self
                .protocol_impls
                .get(&(protocol, key, method.to_string()))
        {
            return Some(*val);
        }
        // Fallback to default/wildcard impl
        self.protocol_impls
            .get(&(protocol, ShapeKey::Wildcard, method.to_string()))
            .copied()
    }

    /// Lookup a concrete (type-specific) protocol impl, without falling back
    /// to the default wildcard impl.
    pub fn get_concrete_protocol_impl(
        &self,
        protocol: ProtocolId,
        target_type: Option<ShapeKey>,
        method: &str,
    ) -> Option<TlangValue> {
        let key = target_type?;
        self.protocol_impls
            .get(&(protocol, key, method.to_string()))
            .copied()
    }

    pub fn is_protocol_by_name(&self, name: &str) -> bool {
        self.protocol_name_to_id.contains_key(name)
    }

    pub fn protocol_id_by_name(&self, name: &str) -> Option<ProtocolId> {
        self.protocol_name_to_id.get(name).copied()
    }

    pub fn get_protocol_for_method(&self, method_name: &str) -> Option<ProtocolId> {
        self.protocol_method_to_protocol.get(method_name).copied()
    }
}
