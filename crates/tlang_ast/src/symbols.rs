use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolType {
    Variable,
    Function,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SymbolInfo {
    pub name: String,
    pub symbol_type: SymbolType,
}

#[derive(Debug, PartialEq, Clone)]
pub struct SymbolTable {
    pub parent: Option<Rc<RefCell<SymbolTable>>>,
    pub symbols: HashMap<String, SymbolInfo>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        SymbolTable {
            parent: None,
            symbols: HashMap::new(),
        }
    }
}

impl SymbolTable {
    pub fn new(parent: Rc<RefCell<SymbolTable>>) -> Self {
        SymbolTable {
            parent: Some(parent),
            ..Default::default()
        }
    }

    pub fn get(&self, name: &str) -> Option<SymbolInfo> {
        if let Some(symbol) = self.symbols.get(name) {
            Some(symbol.clone())
        } else if let Some(ref parent) = self.parent {
            parent.borrow().get(name)
        } else {
            None
        }
    }

    pub fn insert(&mut self, name: String, symbol_info: SymbolInfo) {
        self.symbols.insert(name, symbol_info);
    }
}
