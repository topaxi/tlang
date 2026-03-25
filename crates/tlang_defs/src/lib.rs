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
pub enum DefKind {
    Module,
    #[default]
    Variable,
    Function(u16),
    FunctionSelfRef(u16),
    Parameter,
    Enum,
    EnumVariant(u16),
    Struct,
    Protocol,
    ProtocolMethod(u16),
}

impl DefKind {
    pub fn arity(self) -> Option<u16> {
        match self {
            DefKind::Function(arity)
            | DefKind::FunctionSelfRef(arity)
            | DefKind::ProtocolMethod(arity) => Some(arity),
            _ => None,
        }
    }
}

impl Display for DefKind {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            DefKind::Module => write!(f, "module"),
            DefKind::Variable => write!(f, "variable"),
            DefKind::Function(_) | DefKind::FunctionSelfRef(_) | DefKind::ProtocolMethod(_) => {
                write!(f, "function")
            }
            DefKind::Parameter => write!(f, "parameter"),
            DefKind::Enum => write!(f, "enum"),
            DefKind::EnumVariant(_) => write!(f, "enum variant"),
            DefKind::Struct => write!(f, "struct"),
            DefKind::Protocol => write!(f, "protocol"),
        }
    }
}

#[derive(Debug, Eq, PartialEq, Clone, Copy, Hash)]
pub struct DefIdTag;

pub type DefId = tlang_span::id::Id<DefIdTag>;
pub type DefIdAllocator = tlang_span::id::IdAllocator<DefIdTag>;

// TODO: Make fields private and provide getters/setters if needed. Not done as tests rely too much
//       on constructing `Def` directly.
#[derive(Debug, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Def {
    pub id: DefId,
    pub name: Box<str>,
    pub kind: DefKind,
    pub defined_at: Span,
    pub scope_start: u32,
    pub node_id: Option<NodeId>,
    pub hir_id: Option<HirId>,
    /// Whether the symbol is temporary (e.g., a loop variable), only used for information during
    /// debugging.
    pub temp: bool,
    pub builtin: bool,
    pub used: bool,
    pub declared: bool,
    /// Slot index into the global slots Vec for builtin symbols.
    pub global_slot: Option<usize>,
}

impl Def {
    pub fn new(id: DefId, name: &str, kind: DefKind, defined_at: Span, scope_start: u32) -> Self {
        Def {
            id,
            name: name.into(),
            kind,
            defined_at,
            scope_start,
            node_id: None,
            hir_id: None,
            builtin: false,
            temp: false,
            used: false,
            declared: true,
            global_slot: None,
        }
    }

    pub fn new_builtin(id: DefId, name: &str, kind: DefKind, global_slot: Option<usize>) -> Self {
        let mut symbol_info = Def::new(id, name, kind, Span::default(), 0);
        symbol_info.builtin = true;
        symbol_info.global_slot = global_slot;
        symbol_info
    }

    pub fn global_slot(&self) -> Option<usize> {
        self.global_slot
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
        matches!(self.kind, DefKind::Function(a) | DefKind::FunctionSelfRef(a) | DefKind::ProtocolMethod(a) if a as usize == arity || a == u16::MAX)
    }

    pub fn is_any_fn(&self) -> bool {
        matches!(
            self.kind,
            DefKind::Function(_) | DefKind::FunctionSelfRef(_) | DefKind::ProtocolMethod(_)
        )
    }

    /// Whether the symbol is the function binding within the function body itself.
    pub fn is_fn_self_binding(&self) -> bool {
        matches!(self.kind, DefKind::FunctionSelfRef(_))
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DefScope {
    #[cfg_attr(feature = "serde", serde(skip_serializing))]
    parent: Option<Rc<RefCell<DefScope>>>,
    symbols: Vec<Def>,
    /// When `true`, this scope represents a function boundary (function
    /// declaration, function expression, or impl-block method).  Walking
    /// **out** of a function scope during symbol lookup means crossing a
    /// function boundary, which distinguishes a true closure capture
    /// (`Slot::Upvar`) from an intra-function parent-block reference
    /// (`Slot::BlockVar`).
    is_function_scope: bool,
}

// TODO: Should we keep track of the symbol id within the symbol table?
impl DefScope {
    pub fn new(parent: Rc<RefCell<DefScope>>) -> Self {
        DefScope {
            parent: Some(parent),
            ..Default::default()
        }
    }

    /// Create a new scope that represents a function boundary (function
    /// declaration, function expression, or impl-block method).
    pub fn new_function_scope(parent: Rc<RefCell<DefScope>>) -> Self {
        DefScope {
            parent: Some(parent),
            is_function_scope: true,
            ..Default::default()
        }
    }

    /// Mark (or un-mark) this scope as a function boundary.
    pub fn set_is_function_scope(&mut self, value: bool) {
        self.is_function_scope = value;
    }

    pub fn is_function_scope(&self) -> bool {
        self.is_function_scope
    }

    pub fn parent(&self) -> Option<Rc<RefCell<DefScope>>> {
        self.parent.clone()
    }

    pub fn set_parent(&mut self, parent: Rc<RefCell<DefScope>>) {
        self.parent = Some(parent);
    }

    fn get_slot_index<'a>(
        &'a self,
        symbols: &'a [Def],
        predicate: impl Fn(&Def) -> bool,
    ) -> Option<usize> {
        let mut set = HashSet::new();

        symbols
            .iter()
            .filter(|s| self.symbol_gets_slot(s))
            .filter(|s| s.hir_id.is_none() || set.insert(s.hir_id))
            .position(&predicate)
    }

    /// Look up a symbol and return its slot position.
    ///
    /// Returns `Some((slot_index, scope_distance, crosses_function))` where:
    /// - `slot_index` is the symbol's position among slot-eligible siblings in
    ///   the scope where it was found.
    /// - `scope_distance` is the number of parent-chain hops from the current
    ///   scope to the scope that contains the symbol (0 = local).
    /// - `crosses_function` is `true` when walking from the current scope to
    ///   the defining scope requires leaving at least one function-boundary
    ///   scope, making the access a true closure capture.
    pub fn get_slot(&self, predicate: impl Fn(&Def) -> bool) -> Option<(usize, usize, bool)> {
        if let Some(index) = self.get_slot_index(&self.symbols, &predicate) {
            return Some((index, 0, false));
        }

        let mut scope_index = 1;
        let mut table = self.parent();
        // Track whether we have exited a function-boundary scope.
        // Walking from a child scope to its parent crosses a function
        // boundary when the *child* (i.e. the scope we are leaving) is
        // marked `is_function_scope`.
        let mut crossed_function = self.is_function_scope;

        while let Some(t) = table {
            let t = t.borrow();

            if let Some(index) = self.get_slot_index(&t.symbols, &predicate) {
                return Some((index, scope_index, crossed_function));
            }

            // If the scope we are about to leave is a function scope,
            // any further lookup crosses a function boundary.
            crossed_function = crossed_function || t.is_function_scope;

            scope_index += 1;
            table = t.parent();
        }

        None
    }

    pub fn get_local(&self, predicate: impl Fn(&Def) -> bool) -> Option<&Def> {
        self.symbols.iter().find(|s| predicate(s))
    }

    fn get_local_mut(&mut self, predicate: impl Fn(&Def) -> bool) -> Option<&mut Def> {
        self.symbols.iter_mut().find(|s| predicate(s))
    }

    pub fn set_declared(&mut self, id: DefId, declared: bool) {
        if let Some(s) = self.get_local_mut(|s| s.id == id) {
            s.set_declared(declared);
        }
    }

    fn get_locals(&self, predicate: impl Fn(&Def) -> bool) -> Vec<&Def> {
        self.symbols
            .iter()
            .filter(|s| s.declared)
            .filter(|s| predicate(s))
            .collect()
    }

    fn get_locals_by_name(&self, name: &str) -> Vec<&Def> {
        self.get_locals(|s| *s.name == *name)
    }

    pub fn get_by_name(&self, name: &str) -> Vec<Def> {
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

    pub fn get_by_name_and_arity(&self, name: &str, arity: usize) -> Vec<Def> {
        let locals = self
            .get_locals_by_name(name)
            .into_iter()
            .filter(|s| {
                if let DefKind::Function(a)
                | DefKind::FunctionSelfRef(a)
                | DefKind::ProtocolMethod(a) = s.kind
                {
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

    pub fn get_closest_by_name(&self, name: &str, span: Span) -> Option<Def> {
        let closest = self
            .get_by_name(name)
            .iter()
            .rev()
            .filter(|s| s.declared)
            .find(|s| s.scope_start < span.start_lc.line)
            .cloned();

        if closest.is_some() {
            return closest;
        }

        self.get_by_name(name)
            .iter()
            .rev()
            .filter(|s| s.declared)
            .find(|s| s.defined_at.start_lc < span.start_lc || s.is_any_fn() || s.is_builtin())
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
            .filter_map(|s| s.kind.arity())
            .filter(|a| hashset.insert(*a))
            .collect();

        let has_fn = fn_symbols.len() > 1 && fn_symbols.iter().any(|s| *s as usize == arity);

        debug!(
            "Checking if symbol table has function: {name} with arity {arity}: {fn_symbols:?} -> {has_fn}"
        );
        if !has_fn {
            debug!("Available symbols: {symbols:#?}");
        }

        has_fn
    }

    pub fn shift(&mut self) {
        // TODO: Maybe we shouldn't remove symbols...
        //       This is used to remove the fn self binding from the table when within an
        //       match arm (during lowering).
        self.symbols.remove(0);
    }

    pub fn insert(&mut self, symbol_info: Def) {
        debug!("Inserting symbol: {symbol_info:?}");

        self.symbols.push(symbol_info);
    }

    pub fn insert_at(&mut self, index: usize, symbol_info: Def) {
        debug!("Inserting symbol at index {index}: {symbol_info:?}");

        self.symbols.insert(index, symbol_info);
    }

    pub fn insert_after(&mut self, symbol_info: Def, predicate: impl Fn(&Def) -> bool) {
        debug!("Inserting symbol after predicate: {symbol_info:?}");

        if let Some(index) = self.symbols.iter().position(&predicate) {
            self.symbols.insert(index + 1, symbol_info);
        } else {
            self.symbols.push(symbol_info);
        }
    }

    pub fn mark_as_used(&mut self, id: DefId) {
        if let Some(symbol_info) = self.get_local_mut(|s| s.id == id) {
            if symbol_info.used {
                return;
            }

            if symbol_info.is_any_fn() {
                debug!(
                    "Marking {} `{}/{}` with {:?} as used",
                    symbol_info.kind,
                    symbol_info.name,
                    symbol_info.kind.arity().unwrap_or_default(),
                    id
                );
            } else {
                debug!(
                    "Marking {} `{}` with {:?} as used",
                    symbol_info.kind, symbol_info.name, id
                );
            }

            symbol_info.used = true;
        } else if let Some(parent) = &self.parent {
            parent.borrow_mut().mark_as_used(id);
        }
    }

    pub fn get_all_declared_local_symbols(&self) -> impl Iterator<Item = Def> {
        self.symbols.iter().filter(|s| s.declared).cloned()
    }

    pub fn get_all_local_symbols(&self) -> &[Def] {
        &self.symbols
    }

    pub fn get_all_local_symbols_mut(&mut self) -> &mut Vec<Def> {
        &mut self.symbols
    }

    pub fn get_all_declared_symbols(&self) -> Vec<Def> {
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
    fn symbol_gets_slot(&self, s: &Def) -> bool {
        // Builtins use global slots assigned at interpreter startup, not local scope slots.
        !s.builtin
            // Enum and struct definitions do not generate a slot
            && !matches!(s.kind, DefKind::Enum | DefKind::Struct)
            // Protocol definitions do not generate a slot
            && !matches!(s.kind, DefKind::Protocol)
            // Protocol method implementations do not generate a slot
            && !matches!(s.kind, DefKind::ProtocolMethod(_))
            // And tagged enum variant definitions do not generate a slot
            && !matches!(s.kind, DefKind::EnumVariant(len) if len > 0)
    }
}
