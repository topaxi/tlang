use std::cell::RefCell;
use std::collections::HashMap;
use std::rc::Rc;

use log::debug;
use tlang_defs::DefScope;
use tlang_diagnostics::Diagnostic;
use tlang_hir as hir;
use tlang_span::{HirId, HirIdAllocator};

pub const MAX_ITERATIONS: u32 = 10;

#[derive(Debug)]
pub enum HirOptError {
    ConvergenceFailure { pass_name: String, iteration: u32 },
}

impl std::fmt::Display for HirOptError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            HirOptError::ConvergenceFailure {
                pass_name,
                iteration,
            } => write!(
                f,
                "HIR optimizer pass '{}' did not converge after {} iterations",
                pass_name, iteration
            ),
        }
    }
}

impl std::error::Error for HirOptError {}

#[derive(Debug)]
pub struct HirOptContext {
    pub symbols: HashMap<HirId, Rc<RefCell<DefScope>>>,
    pub hir_id_allocator: HirIdAllocator,
    pub current_scope: HirId,
    pub diagnostics: Vec<Diagnostic>,
}

impl HirOptContext {
    pub fn current_symbol_table(&self) -> Option<Rc<RefCell<DefScope>>> {
        self.symbols.get(&self.current_scope).cloned()
    }
}

impl From<hir::LowerResultMeta> for HirOptContext {
    fn from(lower_result_meta: hir::LowerResultMeta) -> Self {
        HirOptContext {
            current_scope: lower_result_meta.root_symbol_table,
            symbols: lower_result_meta.symbol_tables,
            hir_id_allocator: lower_result_meta.hir_id_allocator,
            diagnostics: Vec::new(),
        }
    }
}

pub trait HirPass {
    fn name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    #[allow(unused_variables)]
    fn init_context(&mut self, ctx: &mut HirOptContext) {}

    fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError>;
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

    fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        let mut iteration = 0u32;
        let mut changed = true;
        let mut any_changed = false;

        while changed {
            iteration += 1;
            if iteration > MAX_ITERATIONS {
                return Err(HirOptError::ConvergenceFailure {
                    pass_name: self.name().to_string(),
                    iteration: MAX_ITERATIONS,
                });
            }

            changed = false;
            for pass in &mut self.passes {
                debug!("Running pass: {}", pass.name());

                let pass_changed = pass.optimize_hir(module, ctx)?;
                changed |= pass_changed;
                any_changed |= pass_changed;
            }
        }
        Ok(any_changed)
    }
}

pub struct HirOptimizer(HirOptGroup);

impl Default for HirOptimizer {
    fn default() -> Self {
        Self::new(vec![
            Box::new(crate::tail_call_validation::TailPositionAnalysis),
            Box::new(crate::symbol_resolution::SymbolResolution::default()),
            Box::new(crate::constant_folding::ConstantFolding::default()),
            Box::new(crate::dead_code_elimination::DeadCodeElimination::default()),
            Box::new(crate::slot_allocation::SlotAllocation::default()),
            Box::new(crate::free_variable_analysis::FreeVariableAnalysis),
        ])
    }
}

impl HirOptimizer {
    pub fn new(passes: Vec<Box<dyn HirPass>>) -> Self {
        Self(HirOptGroup::new("root", passes))
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

impl HirPass for HirOptimizer {
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

#[cfg(test)]
mod tests {
    use super::*;

    struct AlwaysChangedPass;

    impl HirPass for AlwaysChangedPass {
        fn name(&self) -> &'static str {
            "AlwaysChangedPass"
        }

        fn optimize_hir(
            &mut self,
            _module: &mut hir::Module,
            _ctx: &mut HirOptContext,
        ) -> Result<bool, HirOptError> {
            Ok(true) // always reports a change, causing infinite convergence
        }
    }

    fn make_ctx() -> HirOptContext {
        HirOptContext {
            symbols: HashMap::new(),
            hir_id_allocator: HirIdAllocator::default(),
            current_scope: HirId::new(1),
            diagnostics: Vec::new(),
        }
    }

    #[test]
    fn convergence_failure_returns_error() {
        let mut group = HirOptGroup::new("test_group", vec![Box::new(AlwaysChangedPass)]);
        let mut module = hir::Module::default();
        let mut ctx = make_ctx();
        let result = HirPass::optimize_hir(&mut group, &mut module, &mut ctx);
        match result {
            Err(HirOptError::ConvergenceFailure {
                pass_name,
                iteration,
            }) => {
                assert_eq!(pass_name, "test_group");
                assert_eq!(iteration, MAX_ITERATIONS);
            }
            Ok(_) => panic!("expected ConvergenceFailure, got Ok"),
        }
    }

    #[test]
    fn convergence_failure_display() {
        let err = HirOptError::ConvergenceFailure {
            pass_name: "my_pass".to_string(),
            iteration: 10,
        };
        assert_eq!(
            err.to_string(),
            "HIR optimizer pass 'my_pass' did not converge after 10 iterations"
        );
    }
}
