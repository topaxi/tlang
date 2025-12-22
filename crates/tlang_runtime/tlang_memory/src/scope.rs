use std::cell::RefCell;
use std::rc::Rc;

use tlang_hir::hir::{self, HirScope, ScopeIndex};

use crate::resolver::Resolver;
use crate::value::TlangValue;

#[derive(Debug)]
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

        let root_scope = Scope::default(); // Root scope starts at 0 by default
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
        log::debug!(
            "Pushing scope with: {} locals and {} upvars",
            meta.locals(),
            meta.upvars()
        );

        // Local scopes (non-global) start at the current end of memory vector
        // Global scope (index 0) uses separate global_memory
        if self.scopes.is_empty() {
            log::warn!(
                "Pushing global scope - ScopeStack gets initialized with the global/root scope already"
            );
        } else {
            // Reserve capacity for the exact number of locals (avoid reallocations)
            self.memory.reserve(meta.locals());
        }

        let new_scope = Scope::new(self.memory.len(), meta.locals());
        log::debug!(
            "Creating scope with start={}, size={}, current_memory_len={}",
            new_scope.start(),
            new_scope.size(),
            self.memory.len()
        );
        self.scopes.push(new_scope);
    }

    pub fn pop(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            // For local scopes, we truncate memory and close the scope
            if !self.scopes.is_empty() {
                let start = scope.start();

                // Optimization: Only capture memory if the scope is referenced elsewhere
                // (strong_count > 1 means a closure is holding a reference)
                if Rc::strong_count(&scope.inner) > 1 {
                    let end = self.memory.len();
                    let values = self.memory[start..end].to_vec();
                    scope.close(values);
                }

                self.memory.truncate(start);
            }
            // Global scope uses separate global_memory, no truncation needed for memory vector
        }

        log::debug!("Popping scope");
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
        let scope_index = self.scopes.len() - 1;

        if scope_index == 0 {
            // Global scope: clear the global_memory vector
            self.global_memory.clear();
        } else {
            // Local scopes: truncate memory back to this scope's start
            let start = self.current_scope().start();
            self.memory.truncate(start);
        }
    }

    /// # Panics
    ///
    /// Panics if the scope has reached its pre-allocated capacity and is not the root scope.
    pub fn push_value(&mut self, value: TlangValue) {
        let scope_index = self.scopes.len() - 1;

        // Handle global scope (index 0) separately
        if scope_index == 0 {
            // Global scope uses global_memory and can grow freely
            self.global_memory.push(value);
        } else {
            // Local scopes: simply push to the memory vector
            // The vector boundaries are managed by scope start positions
            self.memory.push(value);
        }
    }

    fn get_local(&self, index: usize) -> Option<TlangValue> {
        let scope_index = self.scopes.len() - 1;

        if scope_index == 0 {
            // Global scope uses global_memory
            self.global_memory.get(index).copied()
        } else {
            // Local scopes use memory vector with start position
            let current_scope = self.current_scope();
            let inner = current_scope.inner.borrow();
            match &inner.state {
                ScopeState::Open { start, .. } => {
                    let absolute_index = start + index;
                    self.memory.get(absolute_index).copied()
                }
                ScopeState::Closed { values } => values.get(index).copied(),
            }
        }
    }

    pub fn set_local(&mut self, index: usize, value: TlangValue) {
        let scope_index = self.scopes.len() - 1;

        if scope_index == 0 {
            // Global scope uses global_memory
            // Extend global memory if needed
            if index >= self.global_memory.len() {
                self.global_memory.resize(index + 1, TlangValue::Nil);
            }
            self.global_memory[index] = value;
        } else {
            // Local scopes use memory vector with start position
            // We need to extract the start index first to avoid holding the borrow
            // while mutating self.memory
            let start_index = {
                let current_scope = self.current_scope();
                let inner = current_scope.inner.borrow();
                match inner.state {
                    ScopeState::Open { start, .. } => Some(start),
                    ScopeState::Closed { .. } => None,
                }
            };

            if let Some(start) = start_index {
                let absolute_index = start + index;

                // Extend memory vector if needed to accommodate this slot
                if absolute_index >= self.memory.len() {
                    self.memory.resize(absolute_index + 1, TlangValue::Nil);
                }
                self.memory[absolute_index] = value;
            } else {
                panic!("Cannot set local in closed scope");
            }
        }
    }

    fn get_upvar(&self, relative_scope_index: u16, index: usize) -> Option<TlangValue> {
        let scope_index = self.scope_index(relative_scope_index);

        if scope_index == 0 {
            // Global scope uses global_memory
            self.global_memory.get(index).copied()
        } else {
            // Local scopes use memory vector with start position
            let scope = &self.scopes[scope_index];
            let inner = scope.inner.borrow();
            match &inner.state {
                ScopeState::Open { start, .. } => {
                    let absolute_index = start + index;
                    self.memory.get(absolute_index).copied()
                }
                ScopeState::Closed { values } => values.get(index).copied(),
            }
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
            // Local scopes use memory vector with start position
            let scope = &self.scopes[scope_index];
            let mut inner = scope.inner.borrow_mut();
            match &mut inner.state {
                ScopeState::Open { start, .. } => {
                    let absolute_index = *start + index;
                    if absolute_index < self.memory.len() {
                        self.memory[absolute_index] = value;
                    }
                }
                ScopeState::Closed { values } => {
                    if index < values.len() {
                        values[index] = value;
                    }
                }
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

    /// Get the next variable index for let bindings in the current scope and increment it
    /// # Panics
    /// Panics if there is no current scope
    pub fn allocate_let_binding_index(&mut self) -> usize {
        let current_scope = self.scopes.last_mut().expect("No current scope");
        current_scope.increment_var_index()
    }

    /// Check if the current scope has allocated slots for variables
    pub fn current_scope_has_slots(&self) -> bool {
        self.scopes.last().is_some_and(|scope| scope.size() > 0)
    }

    /// Initialize the variable index counter for function parameters
    /// This should be called after function parameters are pushed to memory
    /// # Panics
    /// Panics if there is no current scope
    pub fn init_var_index_after_params(&mut self, param_count: usize) {
        let current_scope = self.scopes.last_mut().expect("No current scope");
        current_scope.set_next_var_index(param_count);
    }

    pub fn get_scope_locals(&self, scope: &Scope) -> Vec<TlangValue> {
        // Check if this is the global scope (first scope with start 0)
        if scope.start() == 0
            && !self.scopes.is_empty()
            && std::ptr::eq(scope.inner.as_ptr(), self.scopes[0].inner.as_ptr())
        {
            // Global scope uses global_memory - return entire vector
            self.global_memory.clone()
        } else {
            let inner = scope.inner.borrow();
            match &inner.state {
                ScopeState::Open { start, .. } => {
                    // Local scopes: find the range for this scope
                    // Find the end position by looking for the next scope's start or using vector length
                    let end = {
                        // Find this scope's index
                        let scope_index = self
                            .scopes
                            .iter()
                            .position(|s| std::ptr::eq(s.inner.as_ptr(), scope.inner.as_ptr()));

                        if let Some(idx) = scope_index {
                            if idx == self.scopes.len() - 1 {
                                // This is the current (last) scope - end is vector length
                                self.memory.len()
                            } else {
                                // Not the last scope - end is next scope's start
                                self.scopes[idx + 1].start()
                            }
                        } else {
                            // Fallback: assume this scope goes to the end
                            self.memory.len()
                        }
                    };

                    self.memory[*start..end].to_vec()
                }
                ScopeState::Closed { values } => values.clone(),
            }
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

#[derive(Debug, Clone)]
enum ScopeState {
    Open { start: usize, size: usize },
    Closed { values: Vec<TlangValue> },
}

#[derive(Debug)]
struct ScopeInner {
    state: ScopeState,
    next_var_index: usize,
}

#[derive(Debug, Clone)]
pub struct Scope {
    inner: Rc<RefCell<ScopeInner>>,
}

impl Default for Scope {
    fn default() -> Self {
        Self::new(0, 0)
    }
}

impl Scope {
    pub fn new(start: usize, size: usize) -> Self {
        Self {
            inner: Rc::new(RefCell::new(ScopeInner {
                state: ScopeState::Open { start, size },
                next_var_index: 0,
            })),
        }
    }

    pub fn start(&self) -> usize {
        match self.inner.borrow().state {
            ScopeState::Open { start, .. } => start,
            ScopeState::Closed { .. } => 0, // Or panic?
        }
    }

    pub fn size(&self) -> usize {
        match self.inner.borrow().state {
            ScopeState::Open { size, .. } => size,
            ScopeState::Closed { ref values } => values.len(),
        }
    }

    fn close(&self, values: Vec<TlangValue>) {
        self.inner.borrow_mut().state = ScopeState::Closed { values };
    }

    pub fn increment_var_index(&mut self) -> usize {
        let mut inner = self.inner.borrow_mut();
        let index = inner.next_var_index;
        // If we're exceeding the pre-calculated scope size, expand it
        // This handles cases where HIR analysis doesn't perfectly match runtime needs
        let size = match inner.state {
            ScopeState::Open { size, .. } => size,
            ScopeState::Closed { ref values } => values.len(),
        };

        if index >= size {
            log::debug!(
                "Expanding scope size from {} to {} due to additional variable allocation at index {}",
                size,
                index + 1,
                index
            );
            match &mut inner.state {
                ScopeState::Open { size, .. } => *size = index + 1,
                ScopeState::Closed { .. } => panic!("Cannot expand closed scope"),
            }
        }
        inner.next_var_index += 1;
        index
    }

    pub fn set_next_var_index(&mut self, index: usize) {
        self.inner.borrow_mut().next_var_index = index;
    }
}
