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
