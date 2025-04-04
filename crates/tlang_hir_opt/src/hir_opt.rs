use tlang_hir::hir::Module;

pub trait HirPass {
    fn optimize_module(&mut self, module: &mut Module) -> bool;
}

#[derive(Default)]
pub struct HirOptGroup {
    passes: Vec<Box<dyn HirPass>>,
}

impl HirOptGroup {
    pub fn new(passes: Vec<Box<dyn HirPass>>) -> Self {
        Self { passes }
    }

    pub fn add_pass(&mut self, pass: Box<dyn HirPass>) {
        self.passes.push(pass);
    }
}

impl HirPass for HirOptGroup {
    fn optimize_module(&mut self, module: &mut Module) -> bool {
        let mut iteration = 0;
        let mut changed = true;

        while changed {
            iteration += 1;
            if iteration > 10 {
                panic!("Too many optimization iterations, likely an infinite loop");
            }

            changed = false;
            for pass in self.passes.iter_mut() {
                changed |= pass.optimize_module(module)
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
        Self::new(HirOptGroup::new(vec![
            Box::new(crate::ConstantFolder::default()),
            Box::new(crate::ConstantPropagator::default()),
        ]))
    }
}

impl HirOptimizer {
    pub fn new(group: HirOptGroup) -> Self {
        Self { group }
    }

    pub fn add_pass(&mut self, pass: Box<dyn HirPass>) {
        self.group.passes.push(pass);
    }

    pub fn optimize_module(&mut self, module: &mut Module) -> bool {
        self.group.optimize_module(module)
    }
}
