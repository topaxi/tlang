use tlang_hir::hir;

use crate::HirPass;
use crate::hir_opt::{HirOptContext, HirOptGroup};

use self::identifier_resolver::IdentifierResolver;

mod identifier_resolver;

pub struct SymbolResolution(HirOptGroup);

impl SymbolResolution {
    pub fn new() -> Self {
        Self(HirOptGroup::new(
            std::any::type_name::<Self>(),
            vec![Box::new(IdentifierResolver::default())],
        ))
    }
}

impl Default for SymbolResolution {
    fn default() -> Self {
        Self::new()
    }
}

impl HirPass for SymbolResolution {
    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.0.optimize_hir(module, ctx)
    }
}
