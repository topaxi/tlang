use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use tlang_hir::hir::{self, HirId};

use crate::resolver::Resolver;
use crate::value::TlangValue;

#[derive(Debug, Default)]
pub(crate) struct Scope {
    pub parent: Option<Rc<RefCell<Scope>>>,
    pub values: HashMap<String, TlangValue>,
    pub return_value: Option<TlangValue>,
    pub fn_decls: HashMap<HirId, Rc<hir::FunctionDeclaration>>,
    pub struct_decls: HashMap<HirId, Rc<hir::StructDeclaration>>,
}

impl Resolver for Scope {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue> {
        let path_name = path.join("::");

        match self.values.get(&path_name) {
            Some(value) => Some(*value),
            None => match &self.parent {
                Some(parent) => parent.borrow().resolve_path(path),
                None => None,
            },
        }
    }

    fn resolve_fn_decl(&self, id: hir::HirId) -> Option<Rc<hir::FunctionDeclaration>> {
        if let Some(decl) = self.fn_decls.get(&id) {
            return Some(decl.clone());
        }

        if let Some(parent) = &self.parent {
            return parent.borrow().resolve_fn_decl(id);
        }

        None
    }
}

impl Scope {
    pub fn new_child(parent: Rc<RefCell<Scope>>) -> Self {
        Self {
            parent: Some(parent),
            ..Default::default()
        }
    }

    pub fn insert_value(&mut self, name: String, value: TlangValue) {
        self.values.insert(name, value);
    }
}
