use tlang_hir::{Visitor, hir, visit};
use tlang_hir_opt::{HirOptContext, HirPass};

#[derive(Debug, Default)]
pub struct HirJsPass;

impl HirJsPass {
    pub fn new() -> Self {
        Self
    }
}

impl HirPass for HirJsPass {
    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.visit_module(module, ctx);
        false
    }
}

impl<'hir> Visitor<'hir> for HirJsPass {
    type Context = HirOptContext;

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, ctx: &mut Self::Context) {
        visit::walk_stmt(self, stmt, ctx);
    }
}
