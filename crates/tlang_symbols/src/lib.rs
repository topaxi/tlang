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

#[derive(Debug, Default, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SymbolTable {
    #[cfg_attr(feature = "serde", serde(skip_serializing))]
    parent: Option<Rc<RefCell<SymbolTable>>>,
    symbols: Vec<SymbolInfo>,
    
    // NEW: Storage infrastructure for the updated pattern
    #[cfg_attr(feature = "serde", serde(skip_serializing))]
    storage: Option<Rc<RefCell<SymbolStorage>>>,
    scope: Option<SymbolScope>,
}

// TODO: Should we keep track of the symbol id within the symbol table?
impl SymbolTable {
    pub fn new(parent: Rc<RefCell<SymbolTable>>) -> Self {
        SymbolTable {
            parent: Some(parent),
            ..Default::default()
        }
    }

    /// Create a new SymbolTable using the new storage pattern
    pub fn new_with_storage() -> Self {
        SymbolTable {
            parent: None,
            symbols: Vec::new(),
            storage: Some(Rc::new(RefCell::new(SymbolStorage::new()))),
            scope: Some(SymbolScope::new(0, 0)),
        }
    }

    /// Create a child SymbolTable that shares storage with its parent
    pub fn new_child_with_storage(parent: Rc<RefCell<SymbolTable>>) -> Self {
        let (storage, start) = {
            let parent_borrow = parent.borrow();
            let storage = parent_borrow.storage.clone();
            let start = storage.as_ref().map_or(0, |s| s.borrow().symbols.len());
            (storage, start)
        };
        
        SymbolTable {
            parent: Some(parent),
            symbols: Vec::new(),
            storage,
            scope: Some(SymbolScope::new(start, 0)),
        }
    }

    pub fn parent(&self) -> Option<Rc<RefCell<SymbolTable>>> {
        self.parent.clone()
    }

    pub fn set_parent(&mut self, parent: Rc<RefCell<SymbolTable>>) {
        self.parent = Some(parent);
    }

    /// Get a symbol by ID (direct access) - NEW METHOD for the updated pattern
    /// For now, this traverses the hierarchy until we fully migrate
    pub fn get_by_id(&self, target_id: SymbolId) -> Option<SymbolInfo> {
        // NEW: If using storage pattern, respect scope boundaries and hierarchy
        if let Some(storage) = &self.storage {
            // First, check if the symbol is in our global storage at all
            if let Some(symbol) = storage.borrow().get(target_id) {
                // Now check if we can access it based on scope hierarchy
                return self.can_access_symbol(target_id, symbol);
            }
            return None;
        }

        // OLD: Fall back to traversal for current pattern
        // First check local symbols
        if let Some(symbol) = self.symbols.iter().find(|s| s.id == target_id) {
            return Some(symbol.clone());
        }

        // Then check parent tables
        if let Some(parent) = &self.parent {
            parent.borrow().get_by_id(target_id)
        } else {
            None
        }
    }

    /// Check if this symbol table can access a given symbol (respects scope hierarchy)
    fn can_access_symbol(&self, target_id: SymbolId, symbol: &SymbolInfo) -> Option<SymbolInfo> {
        // Check if symbol is in our own scope
        if let Some(ref scope) = self.scope {
            let symbol_index = target_id.as_index();
            if scope.contains_index(symbol_index) {
                return Some(symbol.clone());
            }
        }

        // Check if symbol is in parent scopes (can access upward)
        if let Some(parent) = &self.parent {
            return parent.borrow().can_access_symbol(target_id, symbol);
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

    /// Insert using the new storage pattern if available, fallback to old pattern
    pub fn insert_with_storage(&mut self, symbol_info: SymbolInfo) -> SymbolId {
        if let (Some(storage), Some(scope)) = (&self.storage, &mut self.scope) {
            // NEW: Use storage pattern
            let id = storage.borrow_mut().push(symbol_info);
            scope.size += 1; // Expand this scope's size
            debug!("Symbol inserted with ID: {:?} using storage pattern", id);
            id
        } else {
            // OLD: Use existing pattern
            let id = symbol_info.id;
            self.insert(symbol_info);
            id
        }
    }

    pub fn get_slot(&self, predicate: impl Fn(&SymbolInfo) -> bool) -> Option<(usize, usize)> {
        let mut table = Some(Rc::new(RefCell::new(self.clone())));
        let mut scope_index = 0;

        while let Some(t) = table {
            let mut set = HashSet::new();

            if let Some(index) = t
                .borrow()
                .symbols
                .iter()
                .filter(|s| t.borrow().symbol_gets_slot(s))
                // Filter out duplicate symbols, as fn definitions might have multiple symbols
                // attached to them. The lowering result should properly assign the same HirId to
                // each fn symbol representing the same fn.
                .filter(|s| s.hir_id.is_none() || set.insert(s.hir_id))
                .position(&predicate)
            {
                return Some((index, scope_index));
            }

            scope_index += 1;
            table = t.borrow().parent();
        }

        None
    }

    pub fn get_local(&self, predicate: impl Fn(&SymbolInfo) -> bool) -> Option<&SymbolInfo> {
        self.symbols.iter().find(|s| predicate(s))
    }

    fn get_local_mut(
        &mut self,
        predicate: impl Fn(&SymbolInfo) -> bool,
    ) -> Option<&mut SymbolInfo> {
        self.symbols.iter_mut().find(|s| predicate(s))
    }

    pub fn set_declared(&mut self, id: SymbolId, declared: bool) {
        if let Some(s) = self.get_local_mut(|s| s.id == id) {
            s.set_declared(declared);
        }
    }

    fn get_locals(&self, predicate: impl Fn(&SymbolInfo) -> bool) -> Vec<&SymbolInfo> {
        self.symbols
            .iter()
            .filter(|s| s.declared)
            .filter(|s| predicate(s))
            .collect()
    }

    fn get_locals_by_name(&self, name: &str) -> Vec<&SymbolInfo> {
        self.get_locals(|s| *s.name == *name)
    }

    pub fn get_by_name(&self, name: &str) -> Vec<SymbolInfo> {
        let locals = self.get_locals_by_name(name);

        if !locals.is_empty() {
            return locals.into_iter().cloned().collect();
        }

        if let Some(parent) = &self.parent {
            parent.borrow().get_by_name(name)
        } else {
            vec![]
        }
    }

    pub fn get_by_name_and_arity(&self, name: &str, arity: usize) -> Vec<SymbolInfo> {
        let locals = self
            .get_locals_by_name(name)
            .into_iter()
            .filter(|s| {
                if let SymbolType::Function(a) | SymbolType::FunctionSelfRef(a) = s.symbol_type {
                    a == u16::MAX || // Builtin n-ary function
                    a as usize == arity
                } else {
                    // Not a function, so we don't care about arity, might be a variable instead.
                    true
                }
            })
            .cloned()
            .collect::<Vec<_>>();

        if !locals.is_empty() {
            return locals;
        }

        if let Some(parent) = &self.parent {
            parent.borrow().get_by_name_and_arity(name, arity)
        } else {
            vec![]
        }
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
        self.symbols.remove(0);
    }

    pub fn insert(&mut self, symbol_info: SymbolInfo) {
        debug!("Inserting symbol: {:?}", symbol_info);

        self.symbols.push(symbol_info);
    }

    pub fn insert_at(&mut self, index: usize, symbol_info: SymbolInfo) {
        debug!("Inserting symbol at index {}: {:?}", index, symbol_info);

        self.symbols.insert(index, symbol_info);
    }

    pub fn insert_after(
        &mut self,
        symbol_info: SymbolInfo,
        predicate: impl Fn(&SymbolInfo) -> bool,
    ) {
        debug!("Inserting symbol after predicate: {:?}", symbol_info);

        if let Some(index) = self.symbols.iter().position(&predicate) {
            self.symbols.insert(index + 1, symbol_info);
        } else {
            self.symbols.push(symbol_info);
        }
    }

    pub fn mark_as_used(&mut self, id: SymbolId) {
        if let Some(symbol_info) = self.get_local_mut(|s| s.id == id) {
            if symbol_info.used {
                return;
            }

            if symbol_info.is_any_fn() {
                debug!(
                    "Marking {} `{}/{}` with {:?} as used",
                    symbol_info.symbol_type,
                    symbol_info.name,
                    symbol_info.symbol_type.arity().unwrap_or_default(),
                    id
                );
            } else {
                debug!(
                    "Marking {} `{}` with {:?} as used",
                    symbol_info.symbol_type, symbol_info.name, id
                );
            }

            symbol_info.used = true;
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().mark_as_used(id);
        }
    }

    pub fn get_all_declared_local_symbols(&self) -> impl Iterator<Item = SymbolInfo> {
        self.symbols.iter().filter(|s| s.declared).cloned()
    }

    pub fn get_all_local_symbols(&self) -> &[SymbolInfo] {
        &self.symbols
    }

    pub fn get_all_local_symbols_mut(&mut self) -> &mut Vec<SymbolInfo> {
        &mut self.symbols
    }

    pub fn get_all_declared_symbols(&self) -> Vec<SymbolInfo> {
        let mut names = self.get_all_declared_local_symbols().collect::<Vec<_>>();
        if let Some(parent) = &self.parent {
            names.extend(parent.borrow().get_all_declared_symbols());
        }
        names
    }

    /// Returns the count of local symbols that would get slots allocated.
    /// This uses the same filtering logic as `get_slot`.
    pub fn locals(&self) -> usize {
        self.get_all_declared_local_symbols()
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
        self.symbols.get(id.as_index())
    }

    /// Get a mutable reference to a symbol by its ID (direct indexing)
    pub fn get_mut(&mut self, id: SymbolId) -> Option<&mut SymbolInfo> {
        self.symbols.get_mut(id.as_index())
    }

    /// Push a new symbol and return its ID
    pub fn push(&mut self, mut symbol: SymbolInfo) -> SymbolId {
        // Ensure the symbol's ID matches its position in the vector
        let id = SymbolId::new(self.symbols.len() + 1); // IDs start from 1
        symbol.id = id;
        self.symbols.push(symbol);
        id
    }

    /// Get all symbols
    pub fn all(&self) -> &[SymbolInfo] {
        &self.symbols
    }

    /// Get symbols in a specific range
    pub fn range(&self, start: usize, end: usize) -> &[SymbolInfo] {
        &self.symbols[start..end.min(self.symbols.len())]
    }

    /// Get symbols for a specific scope
    pub fn scope_symbols(&self, scope: &SymbolScope) -> &[SymbolInfo] {
        self.range(scope.start(), scope.end())
    }
}
