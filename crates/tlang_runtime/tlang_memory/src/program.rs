use std::collections::HashMap;
use std::rc::Rc;

use tlang_hir as hir;
use tlang_span::HirId;

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
    /// Protocol definitions: protocol_name → list of method names
    pub(crate) protocols: HashMap<String, Vec<String>>,
    /// Reverse lookup: method_name → protocol_name (for global aliases)
    pub(crate) protocol_method_to_protocol: HashMap<String, String>,
    /// Protocol implementations: (protocol_name, type_name, method_name) → fn value
    pub(crate) protocol_impls: HashMap<(String, String, String), TlangValue>,
    /// Singleton parts lists for tagged string literals, keyed by the HirId of the
    /// `ExprKind::Literal(TaggedString)` expression node.  Mirrors the JS tagged
    /// template literal guarantee that the strings array is the same object at every
    /// call to the same template site.
    pub(crate) tagged_string_parts: HashMap<HirId, TlangValue>,
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
            protocol_method_to_protocol: HashMap::new(),
            protocol_impls: HashMap::new(),
            tagged_string_parts: HashMap::new(),
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
            .chain(self.tagged_string_parts.values().copied())
    }

    /// Returns the cached parts list for the given tagged string call site, or inserts
    /// a new one created by `make` and returns it.
    pub fn get_or_insert_tagged_parts(
        &mut self,
        call_site: HirId,
        make: impl FnOnce() -> TlangValue,
    ) -> TlangValue {
        *self
            .tagged_string_parts
            .entry(call_site)
            .or_insert_with(make)
    }

    pub fn register_protocol(&mut self, name: String, methods: Vec<String>) {
        for method in &methods {
            self.protocol_method_to_protocol
                .insert(method.clone(), name.clone());
        }
        self.protocols.insert(name, methods);
    }

    pub fn register_protocol_impl(
        &mut self,
        protocol: &str,
        target_type: &str,
        method: &str,
        fn_value: TlangValue,
    ) {
        self.protocol_impls.insert(
            (
                protocol.to_string(),
                target_type.to_string(),
                method.to_string(),
            ),
            fn_value,
        );
    }

    pub fn get_protocol_impl(
        &self,
        protocol: &str,
        target_type: &str,
        method: &str,
    ) -> Option<TlangValue> {
        self.protocol_impls
            .get(&(
                protocol.to_string(),
                target_type.to_string(),
                method.to_string(),
            ))
            .copied()
    }

    pub fn is_protocol(&self, name: &str) -> bool {
        self.protocols.contains_key(name)
    }

    pub fn get_protocol_for_method(&self, method_name: &str) -> Option<&str> {
        self.protocol_method_to_protocol
            .get(method_name)
            .map(|s| s.as_str())
    }
}
