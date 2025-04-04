use tlang_hir::hir::Module;

use crate::{ConstantFolder, ConstantPropagator};

pub trait HirPass {
    fn optimize_module(&mut self, module: &mut Module) -> bool;
}

pub struct HirOptimizer {
    passes: Vec<Box<dyn HirPass>>,
}

impl Default for HirOptimizer {
    fn default() -> Self {
        let mut optimizer = Self::new();
        optimizer.add_pass(Box::new(ConstantFolder::default()));
        optimizer.add_pass(Box::new(ConstantPropagator::default()));
        // TODO: Removing unused variable declarations messes up the slots from the lowering
        //       process. We should probably do this after lowering and dead code elimination.
        //optimizer.add_pass(Box::new(DeadCodeEliminator::default()));
        optimizer
    }
}

impl HirOptimizer {
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    pub fn add_pass(&mut self, pass: Box<dyn HirPass>) {
        self.passes.push(pass);
    }

    pub fn optimize_module(&mut self, module: &mut Module) {
        let mut iteration = 0;
        let mut changed = true;
        while changed {
            iteration += 1;
            changed = false;
            for pass in self.passes.iter_mut() {
                if pass.optimize_module(module) {
                    changed = true;
                    break;
                }
            }
            if iteration > 10 {
                panic!("Too many optimization iterations, likely an infinite loop");
            }
        }
    }
}
