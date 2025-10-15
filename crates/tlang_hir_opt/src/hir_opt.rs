use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use log::debug;
use tlang_hir::hir;
use tlang_span::{HirId, HirIdAllocator};
use tlang_symbols::SymbolTable;

pub struct HirOptContext {
    pub symbols: HashMap<HirId, Rc<RefCell<SymbolTable>>>,
    pub hir_id_allocator: HirIdAllocator,
    pub current_scope: HirId,
}

impl HirOptContext {
    pub fn current_symbol_table(&self) -> Option<Rc<RefCell<SymbolTable>>> {
        self.symbols.get(&self.current_scope).cloned()
    }
}

impl From<hir::LowerResultMeta> for HirOptContext {
    fn from(lower_result_meta: hir::LowerResultMeta) -> Self {
        HirOptContext {
            current_scope: lower_result_meta.root_symbol_table,
            symbols: lower_result_meta.symbol_tables,
            hir_id_allocator: lower_result_meta.hir_id_allocator,
        }
    }
}

pub trait HirPass {
    fn name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    #[allow(unused_variables)]
    fn init_context(&mut self, ctx: &mut HirOptContext) {}

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

    fn init_context(&mut self, ctx: &mut HirOptContext) {
        for pass in &mut self.passes {
            debug!("Initializing context for pass: {}", pass.name());

            pass.init_context(ctx);
        }
    }

    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        let mut iteration = 0;
        let mut changed = true;

        while changed {
            iteration += 1;
            assert!(iteration <= 10, "Too many optimization iterations, likely an infinite loop");

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
            Box::new(crate::slot_allocation::SlotAllocation::default()),
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

    pub fn optimize_hir(&mut self, module: &mut hir::Module, mut ctx: HirOptContext) -> bool {
        self.group.init_context(&mut ctx);
        self.group.optimize_hir(module, &mut ctx)
    }
}
