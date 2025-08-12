use tlang_hir::hir::LowerResult;

pub trait HirPass {
    fn optimize_hir(&mut self, hir: &mut LowerResult) -> bool;
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
    fn optimize_hir(&mut self, module: &mut LowerResult) -> bool {
        let mut iteration = 0;
        let mut changed = true;

        while changed {
            iteration += 1;
            if iteration > 10 {
                panic!("Too many optimization iterations, likely an infinite loop");
            }

            changed = false;
            for pass in &mut self.passes {
                changed |= pass.optimize_hir(module);
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
            Box::new(crate::ConstantFolder::default()),
            Box::new(crate::ConstantPropagator::default()),
            Box::new(crate::symbol_resolution::SymbolResolution::default()),
        ])
    }
}

impl HirOptimizer {
    pub fn new(passes: Vec<Box<dyn HirPass>>) -> Self {
        Self {
            group: HirOptGroup::new(passes),
        }
    }

    pub fn add_pass(&mut self, pass: Box<dyn HirPass>) {
        self.group.passes.push(pass);
    }

    pub fn optimize_hir(&mut self, hir: &mut LowerResult) -> bool {
        self.group.optimize_hir(hir)
    }
}
