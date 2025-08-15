use std::cell::RefCell;
use std::rc::Rc;

use log::debug;
use tlang_hir::hir::{self, HirScope, ScopeIndex};

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
        self.scopes
            .last()
            .cloned()
            .expect("No current scope available")
    }

    /// # Panics
    pub fn root_scope(&self) -> Rc<RefCell<Scope>> {
        self.scopes
            .first()
            .cloned()
            .expect("No root scope available")
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

    fn set_local(&self, index: usize, value: TlangValue) {
        self.current_scope().borrow_mut().set(index, value);
    }

    fn get_upvar(&self, relative_scope_index: u16, index: usize) -> Option<TlangValue> {
        let scope_index = self.scope_index(relative_scope_index);

        self.scopes[scope_index].borrow().get(index)
    }

    fn set_upvar(&self, relative_scope_index: u16, index: usize, value: TlangValue) {
        let scope_index = self.scope_index(relative_scope_index);

        self.scopes[scope_index].borrow_mut().set(index, value);
    }

    fn scope_index(&self, relative_scope_index: ScopeIndex) -> usize {
        self.scopes.len() - 1 - (relative_scope_index as usize)
    }

    fn resolve_value(&self, res: &hir::Res) -> Option<TlangValue> {
        match res.slot() {
            hir::Slot::Local(index) => self.get_local(index),
            hir::Slot::Upvar(index, relative_scope_index) => {
                self.get_upvar(relative_scope_index, index)
            }
            _ => None,
        }
    }

    /// # Panics
    pub fn update_value(&self, res: &hir::Res, value: TlangValue) {
        match res.slot() {
            hir::Slot::Local(index) => self.set_local(index, value),
            hir::Slot::Upvar(index, relative_scope_index) => {
                self.set_upvar(relative_scope_index, index, value);
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
