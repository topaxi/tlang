// TODO: Does this module belong in the ast crate?
use log::debug;
#[cfg(feature = "serde")]
use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::Display;
use std::rc::Rc;

use crate::node_id::NodeId;
use crate::span::Span;

#[derive(Debug, Default, PartialEq, Copy, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum SymbolType {
    Module,
    #[default]
    Variable,
    Function(u16),
    Parameter,
    Enum,
    EnumVariant,
    Struct,
}

impl SymbolType {
    pub fn arity(self) -> Option<u16> {
        match self {
            SymbolType::Function(arity) => Some(arity),
            _ => None,
        }
    }
}

impl Display for SymbolType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SymbolType::Module => write!(f, "module"),
            SymbolType::Variable => write!(f, "variable"),
            SymbolType::Function(_) => write!(f, "function"),
            SymbolType::Parameter => write!(f, "parameter"),
            SymbolType::Enum => write!(f, "enum"),
            SymbolType::EnumVariant => write!(f, "enum variant"),
            SymbolType::Struct => write!(f, "struct"),
        }
    }
}

#[derive(Debug, Default, Eq, PartialEq, Clone, Copy, Hash)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SymbolId(usize);

impl SymbolId {
    pub fn new(id: usize) -> Self {
        SymbolId(id)
    }

    pub fn next(self) -> Self {
        SymbolId(self.0 + 1)
    }
}

#[derive(Debug, Default, Clone, Copy)]
pub struct SymbolIdAllocator {
    next_id: SymbolId,
}

impl SymbolIdAllocator {
    pub fn new() -> Self {
        SymbolIdAllocator {
            next_id: SymbolId::new(0),
        }
    }

    pub fn next_id(&mut self) -> SymbolId {
        let id = self.next_id;
        self.next_id = self.next_id.next();
        id
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SymbolInfo {
    pub id: SymbolId,
    pub name: String,
    pub symbol_type: SymbolType,
    pub defined_at: Span,
    pub node_id: NodeId,
    pub used: bool,
}

impl SymbolInfo {
    pub fn new(
        node_id: NodeId,
        id: SymbolId,
        name: &str,
        symbol_type: SymbolType,
        defined_at: Span,
    ) -> Self {
        SymbolInfo {
            id,
            name: name.to_string(),
            symbol_type,
            defined_at,
            node_id,
            ..Default::default()
        }
    }

    pub fn new_builtin(name: &str, symbol_type: SymbolType) -> Self {
        SymbolInfo {
            name: name.to_string(),
            symbol_type,
            ..Default::default()
        }
    }

    pub fn is_fn(&self, arity: usize) -> bool {
        matches!(self.symbol_type, SymbolType::Function(a) if a as usize == arity)
    }

    pub fn is_any_fn(&self) -> bool {
        matches!(self.symbol_type, SymbolType::Function(_))
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct SymbolTable {
    #[cfg_attr(feature = "serde", serde(skip_serializing))]
    parent: Option<Rc<RefCell<SymbolTable>>>,
    symbols: Vec<SymbolInfo>,
}

// TODO: Should we keep track of the symbol id within the symbol table?
impl SymbolTable {
    pub fn new(parent: Rc<RefCell<SymbolTable>>) -> Self {
        SymbolTable {
            parent: Some(parent),
            ..Default::default()
        }
    }

    pub fn parent(&self) -> Option<Rc<RefCell<SymbolTable>>> {
        self.parent.clone()
    }

    fn get_local_mut(&mut self, id: SymbolId) -> Option<&mut SymbolInfo> {
        self.symbols.iter_mut().find(|s| s.id == id)
    }

    pub fn get_local_by_node_id(&self, node_id: NodeId) -> Option<&SymbolInfo> {
        self.symbols.iter().find(|s| s.node_id == node_id)
    }

    fn get_locals_by_name(&self, name: &str) -> Vec<&SymbolInfo> {
        self.symbols.iter().filter(|s| s.name == name).collect()
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

    pub fn has_name(&self, name: &str) -> bool {
        // TODO: This is not efficient, we might want to cache or avoid cloning anything
        // here.
        !self.get_by_name(name).is_empty()
    }

    pub fn has_multi_arity_fn(&self, name: &str, arity: usize) -> bool {
        let symbols = self.get_by_name(name);
        let mut hashset = HashSet::new();
        let fn_symbols: Vec<_> = symbols
            .iter()
            .filter(|s| s.name == name && s.is_any_fn())
            .flat_map(|s| s.symbol_type.arity())
            .filter(|a| hashset.insert(*a))
            .collect();

        let has_fn = fn_symbols.len() > 1 && fn_symbols.iter().any(|s| *s as usize == arity);

        debug!(
            "Checking if symbol table has function: {} with arity {}: {:?}",
            name, arity, fn_symbols
        );

        has_fn
    }

    pub fn insert(&mut self, symbol_info: SymbolInfo) {
        debug!("Inserting symbol: {:?}", symbol_info);

        self.symbols.push(symbol_info);
    }

    pub fn insert_beginning(&mut self, symbol_info: SymbolInfo) {
        debug!("Inserting symbol at beginning: {:?}", symbol_info);

        self.symbols.insert(0, symbol_info);
    }

    pub fn mark_as_used(&mut self, id: SymbolId) {
        if let Some(symbol_info) = self.get_local_mut(id) {
            symbol_info.used = true;
        } else if let Some(ref parent) = self.parent {
            parent.borrow_mut().mark_as_used(id);
        }
    }

    pub fn remove(&mut self, id: SymbolId) -> Option<SymbolInfo> {
        self.symbols
            .iter()
            .position(|s| s.id == id)
            .map(|index| self.symbols.swap_remove(index))
    }

    pub fn get_all_local_symbols(&self) -> &[SymbolInfo] {
        &self.symbols
    }

    pub fn get_all_symbols(&self) -> Vec<SymbolInfo> {
        let mut names = self.get_all_local_symbols().to_vec();
        if let Some(ref parent) = self.parent {
            names.extend(parent.borrow().get_all_symbols());
        }
        names
    }
}
