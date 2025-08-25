use log::debug;
use tlang_hir::hir::{self, HirScope, ScopeIndex};

use crate::resolver::Resolver;
use crate::value::TlangValue;

#[derive(Debug, Clone)]
pub struct ScopeStack {
    scopes: Vec<Scope>,
    // Central continuous memory for all scope values
    memory: Vec<TlangValue>,
}

impl ScopeStack {
    pub fn new() -> Self {
        let mut scopes = Vec::with_capacity(10);

        let mut root_scope = Scope::default();
        root_scope.set_offset(0);
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

        let mut new_scope = Scope::new(meta.locals(), meta.upvars());

        // Set the offset for the new scope to the end of current memory
        new_scope.set_offset(self.memory.len());

        self.scopes.push(new_scope);
    }

    pub fn pop(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            // Truncate memory to remove the values from the popped scope
            let new_len = scope.offset();
            self.memory.truncate(new_len);
        }

        debug!("Popping scope");
    }

    /// # Panics
    pub fn current_scope(&self) -> &Scope {
        self.scopes.last().expect("No current scope available")
    }

    /// # Panics  
    pub fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes.last_mut().expect("No current scope available")
    }

    /// # Panics
    pub fn root_scope(&self) -> &Scope {
        self.scopes.first().expect("No root scope available")
    }

    pub fn as_root(&self) -> Self {
        let mut scopes = Vec::with_capacity(10);
        scopes.push(*self.root_scope());
        Self {
            scopes,
            memory: self.memory.clone(), // Share the same memory
        }
    }

    pub fn clear_current_scope(&mut self) {
        let offset = self.current_scope().offset();

        // Truncate memory to remove values from current scope
        self.memory.truncate(offset);

        // Clear the scope
        self.current_scope_mut().clear();
    }

    pub fn push_value(&mut self, value: TlangValue) {
        // Add value to central memory
        self.memory.push(value);

        // Update scope length
        self.current_scope_mut().increment_length();
    }

    fn get_local(&self, index: usize) -> Option<TlangValue> {
        let current_scope = self.current_scope();
        let offset = current_scope.offset();
        let absolute_index = offset + index;

        self.memory.get(absolute_index).copied()
    }

    fn set_local(&mut self, index: usize, value: TlangValue) {
        let current_scope = self.current_scope();
        let offset = current_scope.offset();
        let absolute_index = offset + index;

        if absolute_index < self.memory.len() {
            self.memory[absolute_index] = value;
        }
    }

    fn get_upvar(&self, relative_scope_index: u16, index: usize) -> Option<TlangValue> {
        let scope_index = self.scope_index(relative_scope_index);
        let scope = &self.scopes[scope_index];
        let offset = scope.offset();
        let absolute_index = offset + index;

        self.memory.get(absolute_index).copied()
    }

    fn set_upvar(&mut self, relative_scope_index: u16, index: usize, value: TlangValue) {
        let scope_index = self.scope_index(relative_scope_index);
        let scope = &self.scopes[scope_index];
        let offset = scope.offset();
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

    pub fn iter(&self) -> impl Iterator<Item = &Scope> {
        self.scopes.iter()
    }

    pub fn get_scope_locals(&self, scope: &Scope) -> &[TlangValue] {
        let offset = scope.offset();
        let length = scope.length();

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

#[derive(Debug, Default, Clone, Copy)]
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
