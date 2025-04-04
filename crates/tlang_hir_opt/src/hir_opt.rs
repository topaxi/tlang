use tlang_hir::hir::Module;

pub trait HirPass {
    fn optimize_module(&mut self, module: &mut Module) -> bool;
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
        let mut iteration = 0;
        let mut changed = true;
        while changed {
            iteration += 1;
            println!("Optimization iteration {}", iteration);
            changed = false;
            for (i, pass) in self.passes.iter_mut().enumerate() {
                println!("Running pass {}", i);
                if pass.optimize_module(module) {
                    println!("Pass {} made changes", i);
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