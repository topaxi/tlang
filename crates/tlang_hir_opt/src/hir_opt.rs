use tlang_hir::hir::Module;

pub trait HirPass {
    fn optimize_module(&mut self, module: &mut Module);
}

pub struct HirOptimizer {
    passes: Vec<Box<dyn HirPass>>,
}

impl HirOptimizer {
    pub fn new() -> Self {
        Self { passes: Vec::new() }
    }

    pub fn add_pass(&mut self, pass: Box<dyn HirPass>) {
        self.passes.push(pass);
    }

    pub fn optimize_module(&mut self, module: &mut Module) {
        for pass in &mut self.passes {
            pass.optimize_module(module);
        }
    }
} 