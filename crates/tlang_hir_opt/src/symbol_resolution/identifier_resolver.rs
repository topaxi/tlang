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
            debug!("Entering scope for: {:?}", hir_id);
            ctx.current_scope = Some(hir_id);
            self.scopes.push(hir_id);
        } else {
            debug!("Scope skipped for: {:?}", hir_id);
        }
    }

    fn leave_scope(&mut self, hir_id: hir::HirId, ctx: &mut Self::Context) {
        if ctx.symbols.contains_key(&hir_id) {
            debug!("Leaving scope for: {:?}", hir_id);
            debug_assert!(self.scopes.pop() == Some(hir_id), "Mismatched scope exit");

            ctx.current_scope = self.scopes.last().copied();
        }
    }

    fn visit_path(&mut self, path: &'hir mut hir::Path, ctx: &mut Self::Context) {
        // TODO: Only visit paths which are not in declaration position (eg. function names).
        if path.res.hir_id().is_some() {
            debug!(
                "Path '{}' on line {} already resolved to {:?}",
                path, path.span.start, path.res
            );

            return;
        }

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

        let symbol_info = symbol_table
            .borrow()
            // TODO: Also match function names with explicit arity in their name
            .get_closest_by_name(&path.to_string(), path.span);

        if let Some(symbol_info) = symbol_info
            && let Some(hir_id) = symbol_info.hir_id
        {
            debug!(
                "Resolved path '{}' on line {} to {:?}",
                path, path.span.start, hir_id
            );

            path.res.set_hir_id(hir_id);
            path.res.set_binding_kind(symbol_info.symbol_type.into());
        } else {
            // TODO: Builtin symbols do not have a HirId, we should handle/resolve these somehow.
            warn!(
                "No symbols found for path '{}' on line {}. Available symbols: {:#?}",
                path,
                path.span.start,
                symbol_table.borrow().get_all_declared_symbols()
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
