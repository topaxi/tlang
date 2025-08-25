use tlang_hir::hir;

use crate::HirPass;
use crate::hir_opt::{HirOptContext, HirOptGroup};

mod scope_data_updater;
mod slot_allocator;

pub use scope_data_updater::ScopeDataUpdater;
pub use slot_allocator::SlotAllocator;

pub struct SlotAllocation(HirOptGroup);

impl SlotAllocation {
    pub fn new() -> Self {
        Self(HirOptGroup::new(
            std::any::type_name::<Self>(),
            vec![
                Box::new(SlotAllocator::default()),
                Box::new(ScopeDataUpdater),
            ],
        ))
    }
}

impl Default for SlotAllocation {
    fn default() -> Self {
        Self::new()
    }
}

impl HirPass for SlotAllocation {
    fn init_context(&mut self, ctx: &mut HirOptContext) {
        self.0.init_context(ctx);
    }

    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.0.optimize_hir(module, ctx)
    }
}
