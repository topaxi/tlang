use tlang_hir as hir;
use tlang_hir_opt::hir_opt::{HirOptContext, HirOptGroup};
use tlang_hir_opt::{self as hir_opt, HirPass};

use crate::js_anf_transform::JsAnfTransform;

pub struct JsHirOptimizer(HirOptGroup);

impl Default for JsHirOptimizer {
    fn default() -> Self {
        Self::new(vec![
            Box::new(JsAnfTransform::default()),
            Box::new(hir_opt::symbol_resolution::SymbolResolution::default()),
            Box::new(hir_opt::constant_folding::ConstantFolding::default()),
        ])
    }
}

impl JsHirOptimizer {
    pub fn new(passes: Vec<Box<dyn HirPass>>) -> Self {
        Self(HirOptGroup::new("root", passes))
    }

    pub fn add_pass(&mut self, pass: Box<dyn HirPass>) {
        self.0.add_pass(pass);
    }

    pub fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        HirPass::optimize_hir(self, module, ctx)
    }
}

impl HirPass for JsHirOptimizer {
    fn init_context(&mut self, ctx: &mut HirOptContext) {
        self.0.init_context(ctx);
    }

    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.init_context(ctx);
        self.0.optimize_hir(module, ctx)
    }
}
