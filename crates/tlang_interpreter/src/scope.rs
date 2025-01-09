use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use tlang_hir::hir;

use crate::resolver::Resolver;
use crate::value::TlangValue;

#[derive(Debug, Default)]
pub(crate) struct Scope {
    pub parent: Option<Rc<RefCell<Scope>>>,
    pub values: HashMap<String, TlangValue>,
    pub return_value: Option<TlangValue>,
    pub fn_decls: HashMap<String, Rc<hir::FunctionDeclaration>>,
    pub struct_decls: HashMap<String, Rc<hir::StructDeclaration>>,
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

    fn resolve_fn(&self, path: &hir::Path) -> Option<Rc<hir::FunctionDeclaration>> {
        let path_name = path.join("::");

        match self.fn_decls.get(&path_name) {
            Some(decl) => Some(decl.clone()),
            None => match &self.parent {
                Some(parent) => parent.borrow().resolve_fn(path),
                None => None,
            },
        }
    }

    fn resolve_struct(&self, path: &hir::Path) -> Option<Rc<hir::StructDeclaration>> {
        let path_name = path.join("::");

        match self.struct_decls.get(&path_name) {
            Some(decl) => Some(decl.clone()),
            None => match &self.parent {
                Some(parent) => parent.borrow().resolve_struct(path),
                None => None,
            },
        }
    }
}

type TlangNativeFn = Box<dyn Fn(&[TlangValue]) -> TlangValue>;

#[derive(Default)]
pub(crate) struct RootScope {
    pub scope: Rc<Scope>,
    pub native_fn_decls: HashMap<String, TlangNativeFn>,
    pub native_struct_decls: HashMap<String, ()>,
}

impl Resolver for RootScope {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue> {
        self.scope.resolve_path(path)
    }
    fn resolve_fn(&self, path: &hir::Path) -> Option<Rc<hir::FunctionDeclaration>> {
        self.scope.resolve_fn(path)
    }
    fn resolve_struct(&self, path: &hir::Path) -> Option<Rc<hir::StructDeclaration>> {
        self.scope.resolve_struct(path)
    }
}
