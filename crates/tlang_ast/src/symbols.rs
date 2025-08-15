// TODO: Does this module belong in the ast crate?
use log::debug;
#[cfg(feature = "serde")]
use serde::Serialize;
use std::cell::RefCell;
use std::collections::HashSet;
use std::fmt::Display;
use std::rc::Rc;

use tlang_span::{HirId, LineColumn, NodeId, Span};

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
    EnumVariant,
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
            SymbolType::EnumVariant => write!(f, "enum variant"),
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

    pub fn get_slot(&self, id: SymbolId) -> Option<(usize, usize)> {
        let mut table = Some(Rc::new(RefCell::new(self.clone())));
        let mut scope_index = 0;

        while let Some(t) = table {
            if let Some(index) = t.borrow().symbols.iter().position(|s| s.id == id) {
                return Some((index, scope_index));
            }

            scope_index += 1;
            table = t.borrow().parent.clone();
        }

        None
    }

    pub fn get_local(&self, id: SymbolId) -> Option<&SymbolInfo> {
        self.symbols.iter().find(|s| s.id == id)
    }

    fn get_local_mut(&mut self, id: SymbolId) -> Option<&mut SymbolInfo> {
        self.symbols.iter_mut().find(|s| s.id == id)
    }

    pub fn get_local_by_node_id(&self, node_id: NodeId) -> Option<&SymbolInfo> {
        self.symbols.iter().find(|s| s.node_id == Some(node_id))
    }

    pub fn get_symbol_id_by_hir_id(&self, hir_id: HirId) -> Option<SymbolId> {
        self.get_all_declared_symbols()
            .iter()
            .find(|s| s.hir_id == Some(hir_id))
            .map(|s| s.id)
    }

    pub fn set_declared(&mut self, id: SymbolId, declared: bool) {
        if let Some(s) = self.get_local_mut(id) {
            s.set_declared(declared);
        }
    }

    fn get_locals_by_name(&self, name: &str) -> Vec<&SymbolInfo> {
        self.symbols
            .iter()
            .filter(|s| *s.name == *name)
            .filter(|s| s.declared)
            .collect()
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
        let symbols = self.get_by_name(name);

        symbols
            .iter()
            .rev()
            .filter(|s| s.declared)
            .find(|s| s.scope_start < span.start || s.is_any_fn() || s.is_builtin())
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
        } else if let Some(ref parent) = self.parent {
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
}
