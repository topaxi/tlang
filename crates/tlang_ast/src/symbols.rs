use std::cell::RefCell;
use std::fmt::Display;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolType {
    Variable,
    Function,
    Parameter,
}

impl Display for SymbolType {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            SymbolType::Variable => write!(f, "variable"),
            SymbolType::Function => write!(f, "function"),
            SymbolType::Parameter => write!(f, "parameter"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub struct SymbolId(usize);

impl SymbolId {
    pub fn new(id: usize) -> Self {
        SymbolId(id)
    }

    pub fn next(&self) -> Self {
        SymbolId(self.0 + 1)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct SymbolInfo {
    pub id: SymbolId,
    pub name: String,
    pub symbol_type: SymbolType,
    pub used: bool,
}

impl Default for SymbolInfo {
    fn default() -> Self {
        SymbolInfo {
            id: SymbolId::new(0),
            name: "".to_string(),
            symbol_type: SymbolType::Variable,
            used: false,
        }
    }
}

impl SymbolInfo {
    pub fn new(id: SymbolId, name: &str, symbol_type: SymbolType) -> Self {
        SymbolInfo {
            id,
            name: name.to_string(),
            symbol_type,
            ..Default::default()
        }
    }
}

#[derive(Debug, Default, PartialEq, Clone)]
pub struct SymbolTable {
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

    fn get_local(&self, id: SymbolId) -> Option<SymbolInfo> {
        self.symbols.iter().find(|s| s.id == id).cloned()
    }

    pub fn get(&self, id: SymbolId) -> Option<SymbolInfo> {
        if let Some(symbol) = self.get_local(id) {
            Some(symbol)
        } else if let Some(ref parent) = self.parent {
            parent.borrow().get(id)
        } else {
            None
        }
    }

    fn get_local_by_name(&self, name: &str) -> Option<SymbolInfo> {
        self.symbols.iter().find(|s| s.name == name).cloned()
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

    pub fn remove(&mut self, id: SymbolId) -> Option<SymbolInfo> {
        self.symbols
            .iter()
            .position(|s| s.id == id)
            .map(|index| self.symbols.swap_remove(index))
    }

    pub fn get_all_local_symbols(&self) -> Vec<SymbolInfo> {
        self.symbols.iter().map(|s| s.clone()).collect()
    }

    pub fn get_all_symbol_names(&self) -> Vec<String> {
        let mut names = self
            .get_all_local_symbols()
            .iter()
            .map(|s| s.name.clone())
            .collect::<Vec<_>>();
        if let Some(ref parent) = self.parent {
            names.extend(parent.borrow().get_all_symbol_names());
        }
        names
    }
}
