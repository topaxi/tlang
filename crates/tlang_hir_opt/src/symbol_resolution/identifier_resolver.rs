use log::{debug, warn};
use tlang_hir::{Visitor, hir};
use tlang_span::HirId;

use crate::HirPass;
use crate::hir_opt::HirOptContext;

#[derive(Debug, Default)]
pub struct IdentifierResolver {
    scopes: Vec<HirId>,
}

impl<'hir> Visitor<'hir> for IdentifierResolver {
    type Context = HirOptContext;

    fn enter_scope(&mut self, hir_id: hir::HirId, ctx: &mut Self::Context) {
        if ctx.symbols.contains_key(&hir_id) {
            ctx.current_scope = Some(hir_id);
            self.scopes.push(hir_id);
        }
    }

    fn leave_scope(&mut self, hir_id: hir::HirId, ctx: &mut Self::Context) {
        if ctx.symbols.contains_key(&hir_id) {
            debug_assert!(self.scopes.pop() == Some(hir_id), "Mismatched scope exit");

            ctx.current_scope = self.scopes.last().copied();
        }
    }

    fn visit_path(&mut self, path: &'hir mut hir::Path, ctx: &mut Self::Context) {
        let symbol_table = ctx
            .current_symbol_table()
            .unwrap_or_else(|| {
                panic!(
                    "No symbol table for current scope of {:?} while resolving {}.\nAvailable scopes: {:#?}",
                    ctx.current_scope,
                    path,
                    ctx.symbols.keys()
                )
            });

        let symbols = symbol_table
            .borrow()
            .get_by_name(&path.to_string())
            .iter()
            .filter_map(|s| s.hir_id)
            .collect::<Vec<_>>();

        if let Some(hir_id) = symbols.last() {
            debug!("Resolved path '{}' to {:?}", path, hir_id);

            path.res.set_hir_id(*hir_id);
        } else {
            // TODO: Builtin symbols do not have a HirId, we should handle/resolve these somehow.
            warn!(
                "No symbols found for path '{}'. Available symbols: {:#?}",
                path,
                symbol_table.borrow()
            );
        }
    }
}

impl HirPass for IdentifierResolver {
    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.visit_module(module, ctx);

        false
    }
}
