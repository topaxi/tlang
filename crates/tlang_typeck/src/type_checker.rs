use tlang_hir as hir;
use tlang_hir_opt::hir_opt::{HirOptContext, HirOptError, HirPass};

use crate::{TypeError, TypeTable};

/// The main type-checking pass that runs on the HIR after lowering and
/// before optimization.
///
/// This is a skeleton implementation for Phase 0; subsequent phases will
/// add inference, checking, and diagnostics.
#[derive(Debug, Default)]
pub struct TypeChecker {
    /// Accumulated type errors from the current pass.
    pub errors: Vec<TypeError>,
    /// Side-table mapping HIR nodes to their resolved types.
    pub type_table: TypeTable,
}

impl TypeChecker {
    pub fn new() -> Self {
        Self::default()
    }
}

impl HirPass for TypeChecker {
    fn name(&self) -> &'static str {
        "TypeChecker"
    }

    fn optimize_hir(
        &mut self,
        _module: &mut hir::Module,
        ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        // Convert any accumulated type errors into diagnostics and consume
        // them so repeated optimizer iterations do not emit duplicates.
        for error in self.errors.drain(..) {
            ctx.diagnostics.push((&error).into());
        }

        // The type checker does not transform the HIR (no changes reported).
        Ok(false)
    }
}
