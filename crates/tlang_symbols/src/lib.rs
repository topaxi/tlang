//! Symbol types and symbol table implementation.
//!
//! This crate defines the core symbol management types used throughout the tlang compilation
//! pipeline. By separating symbols into their own crate, we achieve better separation of
//! concerns and allow later compilation phases to avoid depending on the entire AST crate.

use log::debug;
#[cfg(feature = "serde")]
use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::Display;
use std::rc::Rc;

pub use tlang_span::{HirId, LineColumn, NodeId, Span};

#[derive(Debug, Default, PartialEq, Copy, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum SymbolType {
    Module,
    #[default]
    Variable,
    Function(u16),
    FunctionSelfRef(u16),
    Parameter,
    Enum,
    EnumVariant(u16),
    Struct,
}

impl SymbolType {
    pub fn arity(self) -> Option<u16> {
        match self {
            SymbolType::Function(arity) | SymbolType::FunctionSelfRef(arity) => Some(arity),
            _ => None,
        }
    }
}

impl Display for SymbolType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SymbolType::Module => write!(f, "module"),
            SymbolType::Variable => write!(f, "variable"),
            SymbolType::Function(_) | SymbolType::FunctionSelfRef(_) => write!(f, "function"),
            SymbolType::Parameter => write!(f, "parameter"),
            SymbolType::Enum => write!(f, "enum"),
            SymbolType::EnumVariant(_) => write!(f, "enum variant"),
            SymbolType::Struct => write!(f, "struct"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub struct SymbolIdTag;

pub type SymbolId = tlang_span::id::Id<SymbolIdTag>;
pub type SymbolIdAllocator = tlang_span::id::IdAllocator<SymbolIdTag>;

// TODO: Make fields private and provide getters/setters if needed. Not done as tests rely too much
//       on constructing `SymbolInfo` directly.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SymbolInfo {
    pub id: SymbolId,
    pub name: Box<str>,
    pub symbol_type: SymbolType,
    pub defined_at: Span,
    pub scope_start: LineColumn,
    pub node_id: Option<NodeId>,
    pub hir_id: Option<HirId>,
    /// Whether the symbol is temporary (e.g., a loop variable), only used for information during
    /// debugging.
    pub temp: bool,
    pub builtin: bool,
    pub used: bool,
    pub declared: bool,
}

impl SymbolInfo {
    pub fn new(
        id: SymbolId,
        name: &str,
        symbol_type: SymbolType,
        defined_at: Span,
        scope_start: LineColumn,
    ) -> Self {
        SymbolInfo {
            id,
            name: name.into(),
            symbol_type,
            defined_at,
            scope_start,
            node_id: None,
            hir_id: None,
            builtin: false,
            temp: false,
            used: false,
            declared: true,
        }
    }

    pub fn new_builtin(id: SymbolId, name: &str, symbol_type: SymbolType) -> Self {
        let mut symbol_info = SymbolInfo::new(
            id,
            name,
            symbol_type,
            Span::default(),
            LineColumn::default(),
        );
        symbol_info.builtin = true;
        symbol_info
    }

    pub fn set_declared(&mut self, declared: bool) -> &mut Self {
        self.declared = declared;
        self
    }

    pub fn with_node_id(mut self, node_id: NodeId) -> Self {
        self.node_id = Some(node_id);
        self
    }

    pub fn with_hir_id(mut self, hir_id: HirId) -> Self {
        self.hir_id = Some(hir_id);
        self
    }

    pub fn as_temp(mut self) -> Self {
        self.temp = true;
        self
    }

    pub fn is_temp(&self) -> bool {
        self.temp
    }

    pub fn is_builtin(&self) -> bool {
        self.builtin
    }

    pub fn is_fn(&self, arity: usize) -> bool {
        matches!(self.symbol_type, SymbolType::Function(a) | SymbolType::FunctionSelfRef(a) if a as usize == arity || a == u16::MAX)
    }

    pub fn is_any_fn(&self) -> bool {
        matches!(
            self.symbol_type,
            SymbolType::Function(_) | SymbolType::FunctionSelfRef(_)
        )
    }

    /// Whether the symbol is the function binding within the function body itself.
    pub fn is_fn_self_binding(&self) -> bool {
        matches!(self.symbol_type, SymbolType::FunctionSelfRef(_))
    }
}

#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SymbolTable {
    // Storage infrastructure for the new pattern
    #[cfg_attr(feature = "serde", serde(skip_serializing))]
    storage: Option<Rc<RefCell<SymbolStorage>>>,
    scope: Option<SymbolScope>,
}

// TODO: Should we keep track of the symbol id within the symbol table?
impl SymbolTable {
    /// Create a new root SymbolTable
    pub fn new() -> Self {
        SymbolTable {
            storage: Some(Rc::new(RefCell::new(SymbolStorage::new()))),
            scope: Some(SymbolScope::new(0, 0)),
        }
    }

    /// Create a child SymbolTable that shares storage with its parent
    pub fn new_child(parent: Rc<RefCell<SymbolTable>>) -> Self {
        let (storage, start) = {
            let parent_borrow = parent.borrow();
            let storage = parent_borrow.storage.clone();
            // Child scope starts where the current storage ends, not where parent scope ends
            let start = storage.as_ref().map_or(0, |s| s.borrow().symbols.len());
            (storage, start)
        };
        
        SymbolTable {
            storage,
            scope: Some(SymbolScope::new(start, 0)),
        }
    }

    /// Get a symbol by ID (direct access)
    pub fn get_by_id(&self, target_id: SymbolId) -> Option<SymbolInfo> {
        if let Some(storage) = &self.storage {
            // Check if the symbol is in our global storage
            if let Some(symbol) = storage.borrow().get(target_id) {
                // Check if we can access it based on scope hierarchy
                return self.can_access_symbol(target_id, symbol);
            }
            return None;
        }

        panic!("SymbolTable not properly initialized with storage");
    }

    /// Check if this symbol table can access a given symbol (respects scope hierarchy)
    fn can_access_symbol(&self, target_id: SymbolId, symbol: &SymbolInfo) -> Option<SymbolInfo> {
        if let Some(ref scope) = self.scope {
            let symbol_index = target_id.as_index();
            
            // In the new architecture, a scope can access:
            // 1. Symbols in its own scope (from scope.start to scope.start + scope.size)
            // 2. Symbols from parent scopes (from 0 to scope.start)
            if symbol_index < scope.start() + scope.size() {
                return Some(symbol.clone());
            }
        }

        None
    }

    /// Debug method to inspect storage (for testing)
    pub fn debug_storage_info(&self) -> String {
        if let Some(storage_ref) = &self.storage {
            let storage = storage_ref.borrow();
            format!("Global storage contains {} symbols", storage.all().len())
        } else {
            "No storage (using old pattern)".to_string()
        }
    }

    /// Debug method to inspect scope (for testing)
    pub fn debug_scope_info(&self) -> String {
        if let Some(ref scope) = self.scope {
            format!("Scope: start={}, size={}", scope.start(), scope.size())
        } else {
            "No scope (using old pattern)".to_string()
        }
    }

    /// Insert a symbol into the storage
    pub fn insert(&mut self, symbol_info: SymbolInfo) -> SymbolId {
        if let (Some(storage), Some(scope)) = (&self.storage, &mut self.scope) {
            let id = storage.borrow_mut().push(symbol_info);
            scope.size += 1; // Expand this scope's size
            debug!("Symbol inserted with ID: {:?}", id);
            id
        } else {
            panic!("SymbolTable not properly initialized with storage");
        }
    }

    pub fn get_slot(&self, predicate: impl Fn(&SymbolInfo) -> bool) -> Option<(usize, usize)> {
        if let (Some(storage), Some(scope)) = (&self.storage, &self.scope) {
            let storage_ref = storage.borrow();
            
            // First, try to find the symbol in the current scope
            let scope_symbols: Vec<_> = storage_ref.scope_symbols(scope)
                .iter()
                .filter(|s| s.declared)
                .filter(|s| self.symbol_gets_slot(s))
                .collect();

            // Filter out duplicate symbols, as fn definitions might have multiple symbols
            // attached to them. The lowering result should properly assign the same HirId to
            // each fn symbol representing the same fn.
            let mut set = HashSet::new();
            if let Some(index) = scope_symbols
                .iter()
                .filter(|s| s.hir_id.is_none() || set.insert(s.hir_id))
                .position(|s| predicate(s))
            {
                return Some((index, 0)); // scope_index = 0 for current scope
            }
            
            // If not found in current scope, search parent scopes
            // We can access symbols from index 0 to scope.start (parent scopes)
            if scope.start() > 0 {
                let parent_symbols: Vec<_> = storage_ref.all()
                    .iter()
                    .take(scope.start()) // All symbols before our scope (parent scopes)
                    .filter(|s| s.declared)
                    .filter(|s| self.symbol_gets_slot(s))
                    .collect();

                let mut set = HashSet::new();
                if let Some(index) = parent_symbols
                    .iter()
                    .filter(|s| s.hir_id.is_none() || set.insert(s.hir_id))
                    .position(|s| predicate(s))
                {
                    // Return with scope_index = 1 to indicate parent scope
                    return Some((index, 1));
                }
            }
        }

        None
    }

    pub fn get_local(&self, predicate: impl Fn(&SymbolInfo) -> bool) -> Option<SymbolInfo> {
        self.get_locals(predicate).into_iter().next()
    }

    pub fn set_declared(&mut self, id: SymbolId, declared: bool) {
        if let Some(storage) = &self.storage {
            if let Some(symbol) = storage.borrow_mut().get_mut(id) {
                symbol.set_declared(declared);
            }
        } else {
            panic!("SymbolTable not properly initialized with storage");
        }
    }

    fn get_locals(&self, predicate: impl Fn(&SymbolInfo) -> bool) -> Vec<SymbolInfo> {
        if let (Some(storage), Some(scope)) = (&self.storage, &self.scope) {
            let storage_ref = storage.borrow();
            let scope_symbols = storage_ref.scope_symbols(scope);
            return scope_symbols
                .iter()
                .filter(|s| s.declared)
                .filter(|s| predicate(s))
                .cloned()
                .collect();
        }

        panic!("SymbolTable not properly initialized with storage");
    }

    pub fn get_by_name(&self, name: &str) -> Vec<SymbolInfo> {
        if let (Some(storage), Some(_scope)) = (&self.storage, &self.scope) {
            let storage_ref = storage.borrow();
            let mut result = Vec::new();
            
            // For symbol lookup, we need to search all symbols that are accessible to this scope
            // In the shared storage model, we need to search through all symbols and check
            // accessibility based on where they were declared
            
            for (storage_index, symbol) in storage_ref.all().iter().enumerate() {
                if !symbol.declared || *symbol.name != *name {
                    continue;
                }
                
                // Check if this symbol is accessible from the current scope
                if self.can_access_symbol_at_index(storage_index, symbol) {
                    result.push(symbol.clone());
                }
            }
            
            return result;
        }

        panic!("SymbolTable not properly initialized with storage");
    }
    
    /// Check if a symbol at a specific storage index is accessible from this scope
    fn can_access_symbol_at_index(&self, storage_index: usize, symbol: &SymbolInfo) -> bool {
        if let Some(ref scope) = self.scope {
            // Special logic for Function vs FunctionSelfRef symbols:
            
            if symbol.is_fn_self_binding() {
                // FunctionSelfRef symbols: only accessible within their own function scope
                // They should never be accessible from the program scope
                
                // First check if this symbol is within our scope range
                if !(storage_index >= scope.start() && storage_index < scope.start() + scope.size()) {
                    return false;
                }
                
                // Additional check: determine if we're in the program scope by checking
                // if our scope contains Function symbols (which are only in program scope)
                if let Some(storage) = &self.storage {
                    let storage_ref = storage.borrow();
                    
                    // Check if our scope contains any Function symbols
                    let contains_function_symbols = (scope.start()..scope.start() + scope.size())
                        .any(|i| {
                            if let Some(sym) = storage_ref.all().get(i) {
                                matches!(sym.symbol_type, SymbolType::Function(_))
                            } else {
                                false
                            }
                        });
                    
                    if contains_function_symbols {
                        // We're in program scope - FunctionSelfRef symbols not accessible
                        return false;
                    }
                }
                
                return true;
            }
            
            // For non-FunctionSelfRef symbols:
            // 1. Symbols from parent scopes (storage_index < scope.start) are accessible
            //    BUT only if they were declared before this scope started
            if storage_index < scope.start() {
                // We need to determine if we're in a function scope or just a statement scope
                // Check if the symbol immediately before our scope start is a function
                let is_function_scope = if scope.start() > 0 {
                    if let Some(storage) = &self.storage {
                        let storage_ref = storage.borrow();
                        if let Some(prev_symbol) = storage_ref.all().get(scope.start() - 1) {
                            matches!(prev_symbol.symbol_type, SymbolType::FunctionSelfRef(_))
                        } else {
                            false
                        }
                    } else {
                        false
                    }
                } else {
                    false
                };
                
                if is_function_scope {
                    // This is a function scope. We need to get the function's declaration span
                    // to compare with the symbol's declaration span.
                    // Variables should not be accessible if they are declared after the function.
                    
                    // Get our function's declaration from storage
                    if let Some(storage) = &self.storage {
                        let storage_ref = storage.borrow();
                        // The function symbol should be at storage_index = scope.start - 1
                        if let Some(function_symbol) = storage_ref.all().get(scope.start() - 1) {
                            // If the symbol we're checking was declared after our function,
                            // it should not be accessible
                            if symbol.defined_at.start > function_symbol.defined_at.start {
                                return false;
                            }
                        }
                    }
                }
                
                return true; // Parent scope symbol that was declared before us (or we're not in a function scope)
            }
            
            // 2. Function symbols are accessible to all scopes (but still subject to declaration order)
            if matches!(symbol.symbol_type, SymbolType::Function(_)) {
                return true;
            }
            
            // 3. EnumVariant symbols are accessible to all scopes (like functions)
            if matches!(symbol.symbol_type, SymbolType::EnumVariant(_)) {
                return true;
            }
            
            // 4. Enum symbols are accessible to all scopes (like functions)
            if matches!(symbol.symbol_type, SymbolType::Enum) {
                return true;
            }
            
            // 5. For other symbols, use the traditional range check
            return storage_index >= scope.start() && storage_index < scope.start() + scope.size();
        }
        false
    }

    pub fn get_by_name_and_arity(&self, name: &str, arity: usize) -> Vec<SymbolInfo> {
        if let (Some(storage), Some(_scope)) = (&self.storage, &self.scope) {
            let storage_ref = storage.borrow();
            let mut result = Vec::new();
            let mut found_self_ref = false;
            
            for (storage_index, symbol) in storage_ref.all().iter().enumerate() {
                if !symbol.declared || *symbol.name != *name {
                    continue;
                }
                
                // Check arity match
                match symbol.symbol_type {
                    SymbolType::Function(a) | SymbolType::FunctionSelfRef(a) => {
                        if a != u16::MAX && a as usize != arity {
                            continue;
                        }
                    }
                    SymbolType::EnumVariant(a) => {
                        if a as usize != arity {
                            continue;
                        }
                    }
                    SymbolType::Struct => {
                        // Struct constructors can be called with any arity
                        // The exact arity checking is done in later semantic analysis
                    }
                    SymbolType::Parameter => {
                        // Parameters can hold function values and should be callable
                        // The arity checking is done at runtime since parameters are dynamically typed
                    }
                    _ => {
                        continue; // Not callable
                    }
                }
                
                // Check if this symbol is accessible from the current scope
                if self.can_access_symbol_at_index(storage_index, symbol) {
                    if symbol.is_fn_self_binding() {
                        found_self_ref = true;
                    }
                    result.push(symbol.clone());
                }
            }
            
            // If we found a FunctionSelfRef symbol, it means there's a self-reference available
            // But we should only prefer it over Function symbols if we're actually in that function's scope
            if found_self_ref {
                // Check if any of the FunctionSelfRef symbols we found are in our current scope
                let self_ref_in_current_scope = result.iter().any(|s| {
                    if s.is_fn_self_binding() {
                        // Check if this FunctionSelfRef symbol is in our current scope range
                        let symbol_index = s.id.as_index();
                        symbol_index >= _scope.start() && symbol_index < _scope.start() + _scope.size()
                    } else {
                        false
                    }
                });
                
                // Only prefer FunctionSelfRef symbols if one of them is in our current scope
                // This means we're doing a recursive call from within the function
                if self_ref_in_current_scope {
                    result.retain(|s| s.is_fn_self_binding());
                }
            }
            
            // Debug function lookup issues
            debug!("=== FUNCTION LOOKUP DEBUG ===");
            debug!("Looking for function '{}' with arity {} in scope start={}, size={}", 
                   name, arity, _scope.start(), _scope.size());
            debug!("Found {} matching functions, found_self_ref={}", result.len(), found_self_ref);
            for s in &result {
                debug!("  Found: {} '{}' arity={:?}", s.symbol_type, s.name, s.symbol_type.arity());
            }
            if result.is_empty() {
                debug!("Available functions in storage:");
                for (i, s) in storage_ref.all().iter().enumerate() {
                    if s.declared && *s.name == *name && s.is_any_fn() {
                        let accessible = self.can_access_symbol_at_index(i, s);
                        debug!("  [{}] {} '{}' arity={:?} accessible={}", 
                               i, s.symbol_type, s.name, s.symbol_type.arity(), accessible);
                    }
                }
            }
            debug!("=== END FUNCTION LOOKUP DEBUG ===");
            
            return result;
        }

        panic!("SymbolTable not properly initialized with storage");
    }

    pub fn get_closest_by_name(&self, name: &str, span: Span) -> Option<SymbolInfo> {
        let closest = self
            .get_by_name(name)
            .iter()
            .rev()
            .filter(|s| s.declared)
            .find(|s| s.scope_start < span.start)
            .cloned();

        if closest.is_some() {
            return closest;
        }

        self.get_by_name(name)
            .iter()
            .rev()
            .filter(|s| s.declared)
            .find(|s| s.defined_at.start < span.start || s.is_any_fn() || s.is_builtin())
            .cloned()
    }

    pub fn has_name(&self, name: &str) -> bool {
        // TODO: This is not efficient, we might want to cache or avoid cloning anything
        // here.
        !self.get_by_name(name).is_empty()
    }

    pub fn has_multi_arity_fn(&self, name: &str, arity: usize) -> bool {
        let symbols = self.get_all_declared_symbols();
        let mut hashset = HashSet::new();
        let fn_symbols: Vec<_> = symbols
            .iter()
            .filter(|s| *s.name == *name && s.is_any_fn())
            .flat_map(|s| s.symbol_type.arity())
            .filter(|a| hashset.insert(*a))
            .collect();

        let has_fn = fn_symbols.len() > 1 && fn_symbols.iter().any(|s| *s as usize == arity);

        debug!(
            "Checking if symbol table has function: {} with arity {}: {:?} -> {}",
            name, arity, fn_symbols, has_fn
        );
        if !has_fn {
            debug!("Available symbols: {:#?}", symbols);
        }

        has_fn
    }

    pub fn shift(&mut self) {
        // TODO: Maybe we shouldn't remove symbols...
        //       This is used to remove the fn self binding from the table when within an
        //       match arm (during lowering).
        
        // In the new storage pattern, we need to mark the first symbol in our scope as "removed"
        // Since we can't actually remove from shared storage, we'll adjust our scope start
        if let Some(scope) = &mut self.scope {
            if scope.size > 0 {
                // Move the scope start forward by 1, effectively "removing" the first symbol
                scope.start += 1;
                scope.size = scope.size.saturating_sub(1);
            }
        }
    }



    pub fn mark_as_used(&mut self, id: SymbolId) {
        if let Some(storage) = &self.storage {
            // Check if we can access this symbol from our scope
            let can_access = {
                if let Some(symbol) = storage.borrow().get(id) {
                    self.can_access_symbol(id, symbol).is_some()
                } else {
                    false
                }
            };

            if can_access {
                // Now mark it as used
                if let Some(symbol_mut) = storage.borrow_mut().get_mut(id) {
                    if symbol_mut.used {
                        return;
                    }

                    if symbol_mut.is_any_fn() {
                        debug!(
                            "Marking {} `{}/{}` with {:?} as used",
                            symbol_mut.symbol_type,
                            symbol_mut.name,
                            symbol_mut.symbol_type.arity().unwrap_or_default(),
                            id
                        );
                    } else {
                        debug!(
                            "Marking {} `{}` with {:?} as used",
                            symbol_mut.symbol_type, symbol_mut.name, id
                        );
                    }

                    symbol_mut.used = true;
                }
            }
        } else {
            panic!("SymbolTable not properly initialized with storage");
        }
    }

    pub fn get_all_declared_local_symbols(&self) -> Vec<SymbolInfo> {
        self.get_locals(|_| true) // get_locals already filters for declared symbols
    }

    pub fn get_all_local_symbols(&self) -> Vec<SymbolInfo> {
        if let (Some(storage), Some(scope)) = (&self.storage, &self.scope) {
            let storage_ref = storage.borrow();
            storage_ref.scope_symbols(scope).to_vec()
        } else {
            panic!("SymbolTable not properly initialized with storage");
        }
    }

    pub fn get_all_local_symbols_mut(&mut self) -> Vec<&mut SymbolInfo> {
        // Note: This method is problematic with the RefCell pattern because we can't
        // return multiple mutable references from a single borrow.
        // However, for compatibility with existing code, we'll implement a workaround
        // by borrowing the storage mutably and extracting pointers.
        // This is safe because we only access the scope's local symbols.
        unimplemented!("get_all_local_symbols_mut needs redesign for new storage pattern - use for_each_local_symbol_mut instead");
    }

    /// Apply a function to each local symbol mutably
    /// This is the preferred way to mutate local symbols in the new storage pattern
    /// Note: "local" here means all symbols accessible from this symbol table's scope,
    /// not just symbols declared in the current scope
    pub fn for_each_local_symbol_mut<F>(&self, mut f: F)
    where
        F: FnMut(&mut SymbolInfo),
    {
        if let (Some(storage), Some(_scope)) = (&self.storage, &self.scope) {
            let mut storage_borrow = storage.borrow_mut();
            // Iterate over all symbols in the storage, not just current scope
            // This matches the behavior expected by AST lowering for HIR ID assignment
            for symbol in storage_borrow.all_mut() {
                f(symbol);
            }
        }
    }

    pub fn get_all_declared_symbols(&self) -> Vec<SymbolInfo> {
        if let (Some(storage), Some(_scope)) = (&self.storage, &self.scope) {
            let storage_ref = storage.borrow();
            let mut result = Vec::new();
            
            // Use the same accessibility logic as in other methods
            for (storage_index, symbol) in storage_ref.all().iter().enumerate() {
                if !symbol.declared {
                    continue;
                }
                
                // Check if this symbol is accessible from the current scope
                if self.can_access_symbol_at_index(storage_index, symbol) {
                    result.push(symbol.clone());
                }
            }
            
            return result;
        }

        panic!("SymbolTable not properly initialized with storage");
    }

    /// Returns the count of local symbols that would get slots allocated.
    /// This uses the same filtering logic as `get_slot`.
    pub fn locals(&self) -> usize {
        self.get_all_declared_local_symbols()
            .iter()
            .filter(|s| self.symbol_gets_slot(s))
            .count()
    }

    /// Helper method to determine if a symbol gets a slot allocated.
    /// This centralizes the filtering logic used in both `get_slot` and `locals`.
    fn symbol_gets_slot(&self, s: &SymbolInfo) -> bool {
        // Builtins are currently not slotted and are looked up by name.
        !s.builtin
            // Enum and struct definitions do not generate a slot
            && !matches!(s.symbol_type, SymbolType::Enum | SymbolType::Struct)
            // And tagged enum variant definitions do not generate a slot
            && !matches!(s.symbol_type, SymbolType::EnumVariant(len) if len > 0)
    }
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

/// Scope definition for symbols within the global storage
#[derive(Debug, Default, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SymbolScope {
    /// Starting position of symbols for this scope in the global symbol vec
    start: usize,
    /// Number of symbols in this scope
    size: usize,
}

impl SymbolScope {
    pub fn new(start: usize, size: usize) -> Self {
        Self { start, size }
    }

    pub fn start(&self) -> usize {
        self.start
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn end(&self) -> usize {
        self.start + self.size
    }

    pub fn contains_index(&self, index: usize) -> bool {
        index >= self.start && index < self.end()
    }
}

/// Global symbol storage that holds all symbols in a continuous vector
#[derive(Debug, Default, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SymbolStorage {
    /// All symbols stored in a continuous vector
    symbols: Vec<SymbolInfo>,
}

impl SymbolStorage {
    pub fn new() -> Self {
        Self {
            symbols: Vec::new(),
        }
    }

    /// Get a symbol by its ID (direct indexing)
    pub fn get(&self, id: SymbolId) -> Option<&SymbolInfo> {
        // Convert 1-based ID to 0-based storage index
        let storage_index = id.as_index();
        self.symbols.get(storage_index)
    }

    /// Get a mutable reference to a symbol by its ID (direct indexing)
    pub fn get_mut(&mut self, id: SymbolId) -> Option<&mut SymbolInfo> {
        // Convert 1-based ID to 0-based storage index
        let storage_index = id.as_index();
        self.symbols.get_mut(storage_index)
    }

    /// Push a new symbol and return its ID
    pub fn push(&mut self, symbol: SymbolInfo) -> SymbolId {
        // In the new storage architecture, we append symbols sequentially
        // and the symbol's ID should correspond to its storage index
        let storage_index = self.symbols.len();
        let expected_id = SymbolId::new(storage_index + 1); // IDs are 1-based
        
        // Verify that the symbol's ID matches what we expect for sequential allocation
        if symbol.id != expected_id {
            panic!("Symbol ID mismatch: expected {:?}, got {:?} for storage index {}", 
                   expected_id, symbol.id, storage_index);
        }
        
        self.symbols.push(symbol);
        expected_id
    }

    /// Get all symbols
    pub fn all(&self) -> &[SymbolInfo] {
        &self.symbols
    }

    /// Get mutable reference to all symbols in storage
    pub fn all_mut(&mut self) -> &mut [SymbolInfo] {
        &mut self.symbols
    }

    /// Get symbols in a specific range
    pub fn range(&self, start: usize, end: usize) -> &[SymbolInfo] {
        &self.symbols[start..end.min(self.symbols.len())]
    }

    /// Get symbols for a specific scope
    pub fn scope_symbols(&self, scope: &SymbolScope) -> &[SymbolInfo] {
        self.range(scope.start(), scope.end())
    }

    /// Get mutable symbols for a specific scope
    pub fn scope_symbols_mut(&mut self, scope: &SymbolScope) -> &mut [SymbolInfo] {
        let start = scope.start();
        let end = scope.end().min(self.symbols.len());
        &mut self.symbols[start..end]
    }
}
