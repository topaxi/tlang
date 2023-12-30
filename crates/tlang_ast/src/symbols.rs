use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

#[derive(Debug, PartialEq, Clone)]
pub enum SymbolType {
    Variable,
    Function(Box<FunctionInfo>),
}

#[derive(Debug, PartialEq, Clone)]
pub struct FunctionInfo {
    // The parameter names of the function.
    pub params: Vec<String>,

    // Is the function body tail recursive?
    // This is used to determine if we should unwrap the recursion into a while loop.
    pub is_tail_recursive: bool,
}

impl Default for FunctionInfo {
    fn default() -> Self {
        FunctionInfo {
            params: Vec::new(),
            is_tail_recursive: false,
        }
    }
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
    pub fn new(parent: Option<Rc<RefCell<SymbolTable>>>) -> Self {
        SymbolTable {
            parent,
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

    pub fn push(&mut self) -> SymbolTable {
        SymbolTable::new(Some(Rc::new(RefCell::new(self.clone()))))
    }

    pub fn pop(&mut self) -> SymbolTable {
        if let Some(ref parent) = self.parent {
            parent.borrow().clone()
        } else {
            self.clone()
        }
    }
}
