use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use log::debug;
use tlang_ast::symbols::SymbolTable;
use tlang_hir::hir;
use tlang_span::{HirId, HirIdAllocator};

pub struct HirOptContext {
    pub symbols: HashMap<HirId, Rc<RefCell<SymbolTable>>>,
    pub hir_id_allocator: HirIdAllocator,
    pub current_scope: Option<HirId>,
}

impl HirOptContext {
    pub fn current_symbol_table(&self) -> Option<Rc<RefCell<SymbolTable>>> {
        self.current_scope
            .and_then(|scope| self.symbols.get(&scope).cloned())
    }
}

impl From<hir::LowerResultMeta> for HirOptContext {
    fn from(lower_result_meta: hir::LowerResultMeta) -> Self {
        HirOptContext {
            current_scope: None,
            symbols: lower_result_meta.symbol_tables,
            hir_id_allocator: lower_result_meta.hir_id_allocator,
        }
    }
}

pub trait HirPass {
    fn name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool;
}

#[derive(Default)]
pub struct HirOptGroup {
    name: &'static str,
    passes: Vec<Box<dyn HirPass>>,
}

impl HirOptGroup {
    pub fn new(name: &'static str, passes: Vec<Box<dyn HirPass>>) -> Self {
        Self { name, passes }
    }

    pub fn add_pass(&mut self, pass: Box<dyn HirPass>) {
        self.passes.push(pass);
    }
}

impl HirPass for HirOptGroup {
    fn name(&self) -> &'static str {
        self.name
    }

    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        let mut iteration = 0;
        let mut changed = true;

        while changed {
            iteration += 1;
            if iteration > 10 {
                panic!("Too many optimization iterations, likely an infinite loop");
            }

            changed = false;
            for pass in &mut self.passes {
                debug!("Running pass: {}", pass.name());

                changed |= pass.optimize_hir(module, ctx);
            }
        }
        false
    }
}

pub struct HirOptimizer {
    group: HirOptGroup,
}

impl Default for HirOptimizer {
    fn default() -> Self {
        Self::new(vec![
            Box::new(crate::symbol_resolution::SymbolResolution::default()),
            Box::new(crate::constant_folding::ConstantFolding::default()),
            Box::new(crate::slot_allocation::SlotAllocator::default()),
        ])
    }
}

impl HirOptimizer {
    pub fn new(passes: Vec<Box<dyn HirPass>>) -> Self {
        Self {
            group: HirOptGroup::new("root", passes),
        }
    }

    pub fn add_pass(&mut self, pass: Box<dyn HirPass>) {
        self.group.passes.push(pass);
    }

    pub fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.group.optimize_hir(module, ctx)
    }
}
