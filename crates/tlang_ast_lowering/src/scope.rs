use std::collections::HashMap;

use tlang_ast::node::Ident;
use tlang_hir::hir::{self, HirId};

#[derive(Debug)]
pub struct Binding {
    name: String,
    res: hir::Res,
}

impl Binding {
    pub(crate) fn new_local(name: String, hir_id: HirId) -> Self {
        Self {
            name,
            res: hir::Res::Local(hir_id),
        }
    }

    pub(crate) fn new_def(name: String, def_kind: hir::DefKind, hir_id: HirId) -> Self {
        Self {
            name,
            res: hir::Res::Def(def_kind, hir_id),
        }
    }

    pub(crate) fn new_fn_def(name: String, hir_id: HirId) -> Self {
        Self::new_def(name, hir::DefKind::Fn, hir_id)
    }

    pub(crate) fn new_struct_def(name: String, hir_id: HirId) -> Self {
        Self::new_def(name, hir::DefKind::Struct, hir_id)
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
    bindings: HashMap<String, Binding>,
}

impl Scope {
    pub(crate) fn new() -> Self {
        Self {
            bindings: HashMap::new(),
        }
    }

    pub(crate) fn def_local(&mut self, name: &str, hir_id: HirId) {
        self.bindings.insert(
            name.to_string(),
            Binding::new_local(name.to_string(), hir_id),
        );
    }

    pub(crate) fn def_fn_local(&mut self, name: &str, hir_id: HirId) {
        self.bindings.insert(
            name.to_string(),
            Binding::new_fn_def(name.to_string(), hir_id),
        );
    }

    pub(crate) fn def_struct_local(&mut self, name: &str, hir_id: HirId) {
        self.bindings.insert(
            name.to_string(),
            Binding::new_struct_def(name.to_string(), hir_id),
        );
    }

    pub(crate) fn lookup_name(&self, name: &str) -> Option<&str> {
        self.bindings.get(name).map(|s| s.name.as_str())
    }

    pub(crate) fn lookup(&self, name: &str) -> Option<&Binding> {
        self.bindings.get(name)
    }
}
