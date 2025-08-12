use log::debug;
use tlang_hir::hir::LowerResult;

pub trait HirPass {
    fn name(&self) -> &'static str {
        std::any::type_name::<Self>()
    }

    fn optimize_hir(&mut self, hir: &mut LowerResult) -> bool;
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
                debug!("Running pass: {}", pass.name());

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
            Box::new(crate::constant_folding::ConstantFolding::default()),
            Box::new(crate::symbol_resolution::SymbolResolution::default()),
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

    pub fn optimize_hir(&mut self, hir: &mut LowerResult) -> bool {
        self.group.optimize_hir(hir)
    }
}
