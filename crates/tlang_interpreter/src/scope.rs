use std::cell::RefCell;
use std::rc::Rc;

use log::debug;
use tlang_hir::hir::{self, HirScope};

use crate::resolver::Resolver;
use crate::value::TlangValue;

#[derive(Debug, Clone)]
pub(crate) struct ScopeStack {
    scopes: Vec<Rc<RefCell<Scope>>>,
}

impl ScopeStack {
    pub fn new() -> Self {
        let mut scopes = Vec::with_capacity(10);

        scopes.push(Rc::new(RefCell::new(Scope::default())));

        Self { scopes }
    }

    pub fn push<T>(&mut self, meta: &T)
    where
        T: HirScope,
    {
        debug!(
            "Pushing scope with: {} locals and {} upvars",
            meta.locals(),
            meta.upvars()
        );

        self.scopes.push(Rc::new(RefCell::new(Scope::new(
            meta.locals(),
            meta.upvars(),
        ))));
    }

    pub fn pop(&mut self) {
        self.scopes.pop();

        debug!("Popping scope");
    }

    pub fn current_scope(&self) -> Rc<RefCell<Scope>> {
        self.scopes.last().unwrap().clone()
    }

    pub fn root_scope(&self) -> Rc<RefCell<Scope>> {
        self.scopes.first().unwrap().clone()
    }

    pub fn as_root(&self) -> Self {
        let mut scopes = Vec::with_capacity(10);
        scopes.push(self.root_scope());
        Self { scopes }
    }

    fn resolve(&self, res: &hir::Res) -> Option<TlangValue> {
        match res {
            hir::Res::Local(_, index)
            | hir::Res::Def(hir::DefKind::Fn, _, index)
            | hir::Res::Def(hir::DefKind::Variant, _, index) => {
                self.current_scope().borrow().locals.get(*index).cloned()
            }
            hir::Res::Upvar(relative_scope_index, index) => {
                let scope_index = self.scopes.len() - 1 - relative_scope_index;
                self.scopes[scope_index]
                    .borrow()
                    .locals
                    .get(*index)
                    .cloned()
            }
            _ => None,
        }
    }
}

impl Default for ScopeStack {
    fn default() -> Self {
        Self::new()
    }
}

impl Resolver for ScopeStack {
    fn resolve_path(&self, path: &hir::Path) -> Option<TlangValue> {
        self.resolve(&path.res)
    }
}

#[derive(Debug, Default)]
pub(crate) struct Scope {
    // Value bindings in user code, this includes references to user defined functions.
    pub locals: Vec<TlangValue>,
}

impl Scope {
    pub fn new(locals: usize, _upvars: usize) -> Self {
        Self {
            locals: Vec::with_capacity(locals),
        }
    }

    pub fn push_value(&mut self, value: TlangValue) {
        self.locals.push(value);
    }
}
