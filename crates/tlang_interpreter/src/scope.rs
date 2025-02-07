use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use tlang_hir::hir::{self, HirId};

use crate::resolver::Resolver;
use crate::value::TlangValue;

#[derive(Debug, Default)]
pub(crate) struct Scope {
    pub parent: Option<Rc<RefCell<Scope>>>,
    // Value bindings in user code, this includes references to user defined functions.
    pub bindings: HashMap<HirId, TlangValue>,
    // Values in scope, reachable using Path strings
    pub values: HashMap<String, TlangValue>,
    pub return_value: Option<TlangValue>,
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

impl Resolver for Scope {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue> {
        // Local variables or function bindings resolve to values.
        if let hir::Res::Local(hir_id) | hir::Res::Def(hir::DefKind::Fn, hir_id) = path.res {
            if let Some(value) = self.bindings.get(&hir_id) {
                return Some(*value);
            }

            if let Some(parent) = &self.parent {
                return parent.borrow().resolve_path(path);
            }

            return None;
        }

        // Fallback to unknown bindings, maybe this lives in global scope, maybe this is from a
        // different module, or maybe this got declared through a native way during runtime.
        log::warn!(
            "Unable to resolve path via local bindings: {} (res: {:?})",
            path.join(""),
            path.res
        );

        let path_name = path.join("::");

        if let Some(value) = self.values.get(&path_name) {
            return Some(*value);
        }

        if let Some(parent) = &self.parent {
            return parent.borrow().resolve_path(path);
        }

        None
    }
}
