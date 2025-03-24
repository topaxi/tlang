use std::cell::RefCell;
use std::collections::HashMap;

use log::debug;
use tlang_ast::node::Ident;
use tlang_hir::hir::{self, HirId};

#[derive(Debug, Clone)]
pub struct Binding {
    name: String,
    res: hir::Res,
}

impl Binding {
    pub(crate) fn new_local(name: String, hir_id: HirId, index: usize) -> Self {
        Self {
            name,
            res: hir::Res::Local(hir_id, index),
        }
    }

    pub(crate) fn new_def(
        name: String,
        def_kind: hir::DefKind,
        hir_id: HirId,
        index: usize,
    ) -> Self {
        Self {
            name,
            res: hir::Res::Def(def_kind, hir_id, index),
        }
    }

    pub(crate) fn new_fn_def(name: String, hir_id: HirId, index: usize) -> Self {
        Self::new_def(name, hir::DefKind::Fn, hir_id, index)
    }

    pub(crate) fn new_struct_def(name: String, hir_id: HirId, index: usize) -> Self {
        Self::new_def(name, hir::DefKind::Struct, hir_id, index)
    }

    pub(crate) fn new_enum_def(name: String, hir_id: HirId, index: usize) -> Self {
        Self::new_def(name, hir::DefKind::Enum, hir_id, index)
    }

    pub(crate) fn new_enum_variant_def(
        enum_name: &str,
        variant_name: &str,
        hir_id: HirId,
        index: usize,
    ) -> Self {
        Self::new_def(
            enum_name.to_string() + "::" + variant_name,
            hir::DefKind::Variant,
            hir_id,
            index,
        )
    }

    pub(crate) fn new_upvar(name: String, scope: usize, index: usize) -> Self {
        Self {
            name,
            res: hir::Res::Upvar(scope, index),
        }
    }

    pub(crate) fn name(&self) -> &str {
        &self.name
    }

    pub(crate) fn res(&self) -> hir::Res {
        self.res
    }
}

impl From<&str> for Binding {
    fn from(name: &str) -> Self {
        Self {
            name: name.to_string(),
            res: hir::Res::Unknown,
        }
    }
}

impl From<String> for Binding {
    fn from(name: String) -> Self {
        Self {
            name,
            res: hir::Res::Unknown,
        }
    }
}

impl From<&String> for Binding {
    fn from(name: &String) -> Self {
        Self {
            name: name.clone(),
            res: hir::Res::Unknown,
        }
    }
}

impl From<&Ident> for Binding {
    fn from(ident: &Ident) -> Self {
        Self {
            name: ident.as_str().to_string(),
            res: hir::Res::Unknown,
        }
    }
}

#[derive(Debug, Default)]
pub struct Scope {
    bindings: Vec<Binding>,
    definitions: HashMap<String, Binding>,
    upvars: RefCell<HashMap<String, Binding>>,
}

impl Scope {
    pub(crate) fn new() -> Self {
        Self {
            bindings: Vec::new(),
            definitions: HashMap::new(),
            upvars: HashMap::new().into(),
        }
    }

    fn create_binding(&mut self, name: String, binding: Binding) {
        debug!("Creating binding for {}: {:?}", name, binding);

        self.bindings.push(binding);
    }

    fn create_upvar(&self, name: String, binding: Binding) {
        self.upvars.borrow_mut().insert(name, binding);
    }

    pub(crate) fn def_local(&mut self, name: &str, hir_id: HirId) {
        let index = self.bindings.len();

        self.create_binding(
            name.to_string(),
            Binding::new_local(name.to_string(), hir_id, index),
        );
    }

    pub(crate) fn def_fn_local(&mut self, name: &str, hir_id: HirId) {
        let index = self.bindings.len();

        self.create_binding(
            name.to_string(),
            Binding::new_fn_def(name.to_string(), hir_id, index),
        );
    }

    pub(crate) fn def_struct_local(&mut self, name: &str, hir_id: HirId) {
        let index = self.definitions.len();

        self.definitions.insert(
            name.to_string(),
            Binding::new_struct_def(name.to_string(), hir_id, index),
        );
    }

    pub(crate) fn def_enum_local(&mut self, name: &str, hir_id: HirId) {
        let index = self.definitions.len();

        self.definitions.insert(
            name.to_string(),
            Binding::new_enum_def(name.to_string(), hir_id, index),
        );
    }

    pub(crate) fn def_tagged_enum_variant_local(
        &mut self,
        enum_name: &str,
        variant_name: &str,
        hir_id: HirId,
    ) {
        let index = self.definitions.len();

        self.definitions.insert(
            enum_name.to_string() + "::" + variant_name,
            Binding::new_enum_variant_def(enum_name, variant_name, hir_id, index),
        );
    }

    /// Define a local binding for an (untagged) enum variant.
    pub(crate) fn def_untagged_enum_variant_local(
        &mut self,
        enum_name: &str,
        variant_name: &str,
        hir_id: HirId,
    ) {
        self.def_local(&(enum_name.to_string() + "::" + variant_name), hir_id);
    }

    pub(crate) fn def_upvar(&self, name: &str, scope: usize, index: usize) -> Binding {
        let binding = Binding::new_upvar(name.to_string(), scope, index);

        self.create_upvar(name.to_string(), binding.clone());

        binding
    }

    pub(crate) fn lookup(&self, name: &str) -> Option<&Binding> {
        self.bindings.iter().rev().find(|b| b.name() == name)
    }

    pub(crate) fn lookup_definition(&self, name: &str) -> Option<&Binding> {
        self.definitions.get(name)
    }

    pub(crate) fn locals(&self) -> usize {
        self.bindings.len()
    }

    pub(crate) fn upvars(&self) -> usize {
        self.upvars.borrow().len()
    }
}
