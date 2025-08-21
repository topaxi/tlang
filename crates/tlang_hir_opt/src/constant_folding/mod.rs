use tlang_hir::hir;

use crate::HirPass;
use crate::hir_opt::{HirOptContext, HirOptGroup};

mod constant_folder;
mod constant_propagation;

pub use constant_folder::ConstantFolder;
pub use constant_propagation::ConstantPropagator;

pub struct ConstantFolding(HirOptGroup);

impl ConstantFolding {
    pub fn new() -> Self {
        Self(HirOptGroup::new(
            std::any::type_name::<Self>(),
            vec![
                Box::new(ConstantFolder::default()),
                Box::new(ConstantPropagator::default()),
            ],
        ))
    }
}

impl Default for ConstantFolding {
    fn default() -> Self {
        Self::new()
    }
}

impl HirPass for ConstantFolding {
    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.0.optimize_hir(module, ctx)
    }
}
