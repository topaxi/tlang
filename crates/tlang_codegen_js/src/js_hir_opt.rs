use tlang_hir as hir;
use tlang_hir_opt::hir_opt::{HirOptContext, HirOptError, HirOptGroup};
use tlang_hir_opt::{self as hir_opt, HirPass};

use crate::hir_passes::{
    BooleanReturnSimplification, JsAnfReturnOpt, JsAnfTransform, TailCallSelfReferenceValidation,
};

/// The default set of HIR optimization passes for the JavaScript backend.
///
/// Mirrors `DefaultOptimizations` from `tlang_hir_opt`: implements `HirPass`
/// (single pass-through per call) and supports `without()`:
///
/// ```ignore
/// JsHirOptimizer::from(DefaultJsOptimizations::default().without("DeadCodeElimination"))
/// ```
pub struct DefaultJsOptimizations {
    passes: Vec<Box<dyn HirPass>>,
}

impl Default for DefaultJsOptimizations {
    fn default() -> Self {
        Self {
            passes: vec![
                Box::new(hir_opt::tail_call_validation::TailPositionAnalysis::default()),
                // SymbolResolution must run before ANF so that callee paths have
                // their `res.hir_id()` set — the ANF pass uses HirId identity to
                // detect self-referencing tail calls.
                Box::new(hir_opt::symbol_resolution::SymbolResolution::default()),
                // Warn about non-self-referencing `rec` calls that the JS backend
                // cannot compile into loops. Must run after SymbolResolution.
                Box::new(TailCallSelfReferenceValidation::default()),
                Box::new(JsAnfTransform::default()),
                Box::new(JsAnfReturnOpt::default()),
                Box::new(BooleanReturnSimplification::default()),
                Box::new(hir_opt::constant_folding::ConstantFolding::default()),
                Box::new(hir_opt::dead_code_elimination::DeadCodeElimination::default()),
            ],
        }
    }
}

impl DefaultJsOptimizations {
    /// Remove all passes with the given name.
    ///
    /// Matches against each pass's [`HirPass::name()`] at runtime.
    pub fn without(mut self, name: &str) -> Self {
        self.passes.retain(|p| p.name() != name);
        self
    }
}

impl HirPass for DefaultJsOptimizations {
    fn name(&self) -> &'static str {
        "DefaultJsOptimizations"
    }

    fn init_context(&mut self, ctx: &mut HirOptContext) {
        for pass in &mut self.passes {
            pass.init_context(ctx);
        }
    }

    fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        let mut changed = false;
        for pass in &mut self.passes {
            changed |= pass.optimize_hir(module, ctx)?;
        }
        Ok(changed)
    }
}

pub struct JsHirOptimizer(HirOptGroup);

impl Default for JsHirOptimizer {
    fn default() -> Self {
        Self::from(DefaultJsOptimizations::default())
    }
}

impl From<DefaultJsOptimizations> for JsHirOptimizer {
    fn from(defaults: DefaultJsOptimizations) -> Self {
        Self::new(vec![Box::new(defaults)])
    }
}

impl JsHirOptimizer {
    pub fn new(passes: Vec<Box<dyn HirPass>>) -> Self {
        Self(HirOptGroup::new("root", passes))
    }

    pub fn pre_typecheck() -> Self {
        Self::new(vec![
            Box::new(hir_opt::tail_call_validation::TailPositionAnalysis::default()),
            Box::new(hir_opt::symbol_resolution::SymbolResolution::default()),
            Box::new(TailCallSelfReferenceValidation::default()),
        ])
    }

    pub fn post_typecheck() -> Self {
        Self::new(vec![
            Box::new(JsAnfTransform::default()),
            Box::new(JsAnfReturnOpt::default()),
            Box::new(BooleanReturnSimplification::default()),
            Box::new(hir_opt::constant_folding::ConstantFolding::default()),
            Box::new(hir_opt::dead_code_elimination::DeadCodeElimination::default()),
        ])
    }

    pub fn add_pass(&mut self, pass: Box<dyn HirPass>) {
        self.0.add_pass(pass);
    }

    pub fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        HirPass::optimize_hir(self, module, ctx)
    }
}

impl HirPass for JsHirOptimizer {
    fn init_context(&mut self, ctx: &mut HirOptContext) {
        self.0.init_context(ctx);
    }

    fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        self.init_context(ctx);
        self.0.optimize_hir(module, ctx)
    }
}
