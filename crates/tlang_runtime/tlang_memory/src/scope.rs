use std::cell::RefCell;
use std::rc::Rc;

use log::debug;
use tlang_hir::hir::{self, HirScope};

use crate::resolver::Resolver;
use crate::value::TlangValue;

#[derive(Debug, Clone)]
pub struct ScopeStack {
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

    /// # Panics
    pub fn current_scope(&self) -> Rc<RefCell<Scope>> {
        self.scopes.last().unwrap().clone()
    }

    /// # Panics
    pub fn root_scope(&self) -> Rc<RefCell<Scope>> {
        self.scopes.first().unwrap().clone()
    }

    pub fn as_root(&self) -> Self {
        let mut scopes = Vec::with_capacity(10);
        scopes.push(self.root_scope());
        Self { scopes }
    }

    pub fn clear_current_scope(&self) {
        self.current_scope().borrow_mut().clear();
    }

    fn get_local(&self, index: usize) -> Option<TlangValue> {
        self.current_scope().borrow().get(index)
    }

    fn get_upvar(&self, scope_index: usize, index: usize) -> Option<TlangValue> {
        self.scopes[scope_index].borrow().get(index)
    }

    fn resolve_value(&self, res: &hir::Res) -> Option<TlangValue> {
        match res {
            hir::Res::Local(_, index) => self.get_local(*index),
            hir::Res::Upvar(_, relative_scope_index, index) => {
                let scope_index = self.scopes.len() - 1 - relative_scope_index;

                self.get_upvar(scope_index, *index)
            }
            _ => None,
        }
    }

    /// # Panics
    pub fn update_value(&self, res: &hir::Res, value: TlangValue) {
        match res {
            hir::Res::Local(_, index) => self.current_scope().borrow_mut().set(*index, value),
            hir::Res::Upvar(_, relative_scope_index, index) => {
                let scope_index = self.scopes.len() - 1 - relative_scope_index;
                self.scopes[scope_index].borrow_mut().set(*index, value);
            }
            _ => panic!("Cannot update value for {res:?}"),
        }
    }

    pub fn iter(&self) -> impl Iterator<Item = Rc<RefCell<Scope>>> {
        self.scopes.iter().cloned()
    }
}

impl Default for ScopeStack {
    fn default() -> Self {
        Self::new()
    }
}

impl Resolver for ScopeStack {
    fn resolve_value(&self, path: &hir::Path) -> Option<TlangValue> {
        self.resolve_value(&path.res)
    }
}

#[derive(Debug, Default)]
pub struct Scope {
    // Value bindings in user code, this includes references to user defined functions.
    locals: Vec<TlangValue>,
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

    pub fn get(&self, index: usize) -> Option<TlangValue> {
        self.locals.get(index).copied()
    }

    pub fn set(&mut self, index: usize, value: TlangValue) {
        self.locals[index] = value;
    }

    pub fn get_locals(&self) -> &[TlangValue] {
        &self.locals
    }

    pub(crate) fn clear(&mut self) {
        self.locals.clear();
    }
}
