use log::debug;
use tlang_hir::hir::{self, HirScope, ScopeIndex};

use crate::resolver::Resolver;
use crate::value::TlangValue;

#[derive(Debug, Clone)]
pub struct ScopeStack {
    pub scopes: Vec<Scope>,
    // Global scope memory - can grow independently without affecting other scopes
    global_memory: Vec<TlangValue>,
    // Central continuous memory for local scopes only (non-global)
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
            global_memory: Vec::with_capacity(100), // Separate global memory
            memory: Vec::with_capacity(1000), // Start with reasonable capacity for local scopes
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

        let locals_count = meta.locals();
        let mut new_scope = Scope::new(locals_count);

        // Local scopes (non-global) get offset in the local memory vector
        // Global scope (index 0) uses separate global_memory
        if !self.scopes.is_empty() {
            // Set the offset for the new local scope to the end of current local memory
            new_scope.set_offset(self.memory.len());

            // Pre-allocate memory for the exact number of locals in local memory
            self.memory
                .resize(self.memory.len() + locals_count, TlangValue::Nil);
        }
        // Global scope doesn't need offset setup as it uses global_memory directly

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

    pub fn clear_current_scope(&mut self) {
        // Reset the used counter in the current scope to 0
        // This effectively clears the scope without deallocating memory
        self.current_scope_mut().clear_used();
    }

    /// # Panics
    ///
    /// Panics if the scope has reached its pre-allocated capacity and is not the root scope.
    pub fn push_value(&mut self, value: TlangValue) {
        let scope_index = self.scopes.len() - 1;
        let (offset, used, length) = {
            let current_scope = self.current_scope();
            (
                current_scope.offset(),
                current_scope.used(),
                current_scope.length(),
            )
        };

        // Handle global scope (index 0) separately
        if scope_index == 0 {
            // Global scope uses global_memory and can grow freely
            if used >= self.global_memory.len() {
                self.global_memory.push(value);
            } else {
                self.global_memory[used] = value;
            }
        } else {
            // Local scopes use the continuous memory vector
            let absolute_index = offset + used;

            if used >= length {
                // Non-root scopes should not exceed their pre-allocated capacity
                log::warn!(
                    "Local scope exceeded pre-allocated capacity: used={}, allocated={}. \
                     This indicates HIR scope analysis failed to track all local variables correctly. \
                     This is a memory safety violation - scope boundaries are corrupted.",
                    used,
                    length
                );
                // For now, allow growth but this is unsafe
                self.memory.push(value);
                self.current_scope_mut().increment_length();
            } else {
                // Assign value to the next available slot
                self.memory[absolute_index] = value;
            }
        }

        // Increment the used counter
        self.current_scope_mut().increment_used();
    }

    fn get_local(&self, index: usize) -> Option<TlangValue> {
        let scope_index = self.scopes.len() - 1;

        if scope_index == 0 {
            // Global scope uses global_memory
            self.global_memory.get(index).copied()
        } else {
            // Local scopes use memory vector with offset
            let current_scope = self.current_scope();
            let offset = current_scope.offset();
            let absolute_index = offset + index;
            self.memory.get(absolute_index).copied()
        }
    }

    fn set_local(&mut self, index: usize, value: TlangValue) {
        let scope_index = self.scopes.len() - 1;

        if scope_index == 0 {
            // Global scope uses global_memory
            if index < self.global_memory.len() {
                self.global_memory[index] = value;
            }
        } else {
            // Local scopes use memory vector with offset
            let current_scope = self.current_scope();
            let offset = current_scope.offset();
            let absolute_index = offset + index;
            if absolute_index < self.memory.len() {
                self.memory[absolute_index] = value;
            }
        }
    }

    fn get_upvar(&self, relative_scope_index: u16, index: usize) -> Option<TlangValue> {
        let scope_index = self.scope_index(relative_scope_index);

        if scope_index == 0 {
            // Global scope uses global_memory
            self.global_memory.get(index).copied()
        } else {
            // Local scopes use memory vector with offset
            let scope = &self.scopes[scope_index];
            let offset = scope.offset();
            let absolute_index = offset + index;
            self.memory.get(absolute_index).copied()
        }
    }

    fn set_upvar(&mut self, relative_scope_index: u16, index: usize, value: TlangValue) {
        let scope_index = self.scope_index(relative_scope_index);

        if scope_index == 0 {
            // Global scope uses global_memory
            if index < self.global_memory.len() {
                self.global_memory[index] = value;
            }
        } else {
            // Local scopes use memory vector with offset
            let scope = &self.scopes[scope_index];
            let offset = scope.offset();
            let absolute_index = offset + index;
            if absolute_index < self.memory.len() {
                self.memory[absolute_index] = value;
            }
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
        // Check if this is the global scope (first scope with offset 0)
        if scope.offset() == 0 && !self.scopes.is_empty() && std::ptr::eq(scope, &self.scopes[0]) {
            // Global scope uses global_memory
            let used = scope.used();
            &self.global_memory[0..used.min(self.global_memory.len())]
        } else {
            // Local scopes use memory vector with offset
            let offset = scope.offset();
            let used = scope.used();
            &self.memory[offset..offset + used]
        }
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
    // Total number of local slots pre-allocated for this scope
    length: usize,
    // Current number of slots that have been assigned values
    used: usize,
}

impl Scope {
    pub fn new(locals: usize) -> Self {
        Self {
            offset: 0,      // Will be set when the scope is actually created
            length: locals, // Pre-allocated with exact locals count
            used: 0,        // No slots have been used yet
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

    pub fn used(&self) -> usize {
        self.used
    }

    pub fn increment_used(&mut self) {
        self.used += 1;
    }

    pub fn increment_length(&mut self) {
        self.length += 1;
    }

    pub fn clear(&mut self) {
        self.length = 0;
        self.used = 0;
    }

    pub fn clear_used(&mut self) {
        self.used = 0;
    }
}
