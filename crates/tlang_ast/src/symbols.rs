// TODO: Does this module belong in the ast crate?
#[cfg(feature = "serde")]
use serde::Serialize;
use std::cell::RefCell;
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
    Function,
    Parameter,
    Enum,
    EnumVariant,
    Struct,
}

impl Display for SymbolType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SymbolType::Module => write!(f, "module"),
            SymbolType::Variable => write!(f, "variable"),
            SymbolType::Function => write!(f, "function"),
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

    fn get_local(&self, id: SymbolId) -> Option<&SymbolInfo> {
        self.symbols.iter().find(|s| s.id == id)
    }

    fn get_local_mut(&mut self, id: SymbolId) -> Option<&mut SymbolInfo> {
        self.symbols.iter_mut().find(|s| s.id == id)
    }

    pub fn get_local_by_node_id(&self, node_id: NodeId) -> Option<&SymbolInfo> {
        self.symbols.iter().find(|s| s.node_id == node_id)
    }

    pub fn get(&self, id: SymbolId) -> Option<SymbolInfo> {
        if let Some(symbol) = self.get_local(id) {
            Some(symbol.clone())
        } else if let Some(ref parent) = self.parent {
            parent.borrow().get(id)
        } else {
            None
        }
    }

    fn get_local_by_name(&self, name: &str) -> Option<&SymbolInfo> {
        self.symbols.iter().find(|s| s.name == name)
    }

    pub fn get_by_name(&self, name: &str) -> Option<SymbolInfo> {
        if let Some(symbol) = self.get_local_by_name(name) {
            Some(symbol.clone())
        } else if let Some(ref parent) = self.parent {
            parent.borrow().get_by_name(name)
        } else {
            None
        }
    }

    pub fn insert(&mut self, symbol_info: SymbolInfo) {
        self.symbols.push(symbol_info);
    }

    pub fn insert_beginning(&mut self, symbol_info: SymbolInfo) {
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
