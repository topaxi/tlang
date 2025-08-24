use std::cell::RefCell;
use std::rc::Rc;

use log::debug;
use tlang_hir::hir::{self, HirScope, ScopeIndex};

use crate::resolver::Resolver;
use crate::value::TlangValue;

#[derive(Debug, Clone)]
pub struct ScopeStack {
    scopes: Vec<Rc<RefCell<Scope>>>,
    // Central continuous memory for all scope values
    memory: Vec<TlangValue>,
}

impl ScopeStack {
    pub fn new() -> Self {
        let mut scopes = Vec::with_capacity(10);

        let root_scope = Rc::new(RefCell::new(Scope::default()));
        root_scope.borrow_mut().set_offset(0);
        scopes.push(root_scope);

        Self {
            scopes,
            memory: Vec::with_capacity(1000), // Start with reasonable capacity
        }
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

        let new_scope = Rc::new(RefCell::new(Scope::new(meta.locals(), meta.upvars())));

        // Set the offset for the new scope to the end of current memory
        new_scope.borrow_mut().set_offset(self.memory.len());

        self.scopes.push(new_scope);
    }

    pub fn pop(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            // Truncate memory to remove the values from the popped scope
            let scope_borrow = scope.borrow();
            let new_len = scope_borrow.offset();
            drop(scope_borrow); // Release the borrow before truncating
            self.memory.truncate(new_len);
        }

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
        Self {
            scopes,
            memory: self.memory.clone(), // Share the same memory
        }
    }

    pub fn clear_current_scope(&mut self) {
        let current_scope = self.current_scope();
        let mut scope_borrow = current_scope.borrow_mut();
        let offset = scope_borrow.offset();

        // Truncate memory to remove values from current scope
        self.memory.truncate(offset);

        // Clear the scope
        scope_borrow.clear();
    }

    pub fn push_value(&mut self, value: TlangValue) {
        let current_scope = self.current_scope();
        let mut scope_borrow = current_scope.borrow_mut();

        // Add value to central memory
        self.memory.push(value);

        // Update scope length
        scope_borrow.increment_length();
    }

    fn get_local(&self, index: usize) -> Option<TlangValue> {
        let current_scope = self.current_scope();
        let scope_borrow = current_scope.borrow();
        let offset = scope_borrow.offset();
        let absolute_index = offset + index;

        self.memory.get(absolute_index).copied()
    }

    fn set_local(&mut self, index: usize, value: TlangValue) {
        let current_scope = self.current_scope();
        let scope_borrow = current_scope.borrow();
        let offset = scope_borrow.offset();
        let absolute_index = offset + index;

        if absolute_index < self.memory.len() {
            self.memory[absolute_index] = value;
        }
    }

    fn get_upvar(&self, relative_scope_index: u16, index: usize) -> Option<TlangValue> {
        let scope_index = self.scope_index(relative_scope_index);
        let scope = &self.scopes[scope_index];
        let scope_borrow = scope.borrow();
        let offset = scope_borrow.offset();
        let absolute_index = offset + index;

        self.memory.get(absolute_index).copied()
    }

    fn set_upvar(&mut self, relative_scope_index: u16, index: usize, value: TlangValue) {
        let scope_index = self.scope_index(relative_scope_index);
        let scope = &self.scopes[scope_index];
        let scope_borrow = scope.borrow();
        let offset = scope_borrow.offset();
        let absolute_index = offset + index;

        if absolute_index < self.memory.len() {
            self.memory[absolute_index] = value;
        }
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
    pub fn update_value(&mut self, res: &hir::Res, value: TlangValue) {
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

    pub fn get_scope_locals(&self, scope: &Rc<RefCell<Scope>>) -> &[TlangValue] {
        let scope_borrow = scope.borrow();
        let offset = scope_borrow.offset();
        let length = scope_borrow.length();

        &self.memory[offset..offset + length]
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
    // Offset into the central memory vector where this scope's locals start
    offset: usize,
    // Number of local values in this scope
    length: usize,
}

impl Scope {
    pub fn new(_locals: usize, _upvars: usize) -> Self {
        Self {
            offset: 0, // Will be set when the scope is actually created
            length: 0, // Starts empty, grows as values are pushed
        }
    }

    pub fn set_offset(&mut self, offset: usize) {
        self.offset = offset;
    }

    pub fn offset(&self) -> usize {
        self.offset
    }

    pub fn length(&self) -> usize {
        self.length
    }

    pub fn increment_length(&mut self) {
        self.length += 1;
    }

    pub fn clear(&mut self) {
        self.length = 0;
    }
}
