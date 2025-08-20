use log::{debug, warn};
use tlang_hir::visit::walk_expr;
use tlang_hir::{Visitor, hir};
use tlang_span::HirId;

use crate::HirPass;
use crate::hir_opt::HirOptContext;

#[derive(Debug, Default)]
pub struct SlotAllocator {
    scopes: Vec<HirId>,
}

impl<'hir> Visitor<'hir> for SlotAllocator {
    type Context = HirOptContext;

    fn enter_scope(&mut self, hir_id: hir::HirId, ctx: &mut Self::Context) {
        if ctx.symbols.contains_key(&hir_id) {
            debug!("Entering scope for: {:?}", hir_id);

            ctx.current_scope = hir_id;
            self.scopes.push(hir_id);
        }
    }

    fn leave_scope(&mut self, hir_id: hir::HirId, ctx: &mut Self::Context) {
        if ctx.symbols.contains_key(&hir_id) {
            debug!("Leaving scope for: {:?}", hir_id);

            let left_scope = self.scopes.pop();
            debug_assert!(left_scope == Some(hir_id), "Mismatched scope exit");

            ctx.current_scope = self.scopes.last().copied().unwrap();
        }
    }

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut Self::Context) {
        if let hir::ExprKind::Dict(pairs) = &mut expr.kind {
            for (key, value) in pairs.iter_mut() {
                if !key.is_path() {
                    self.visit_expr(key, ctx);
                }

                self.visit_expr(value, ctx);
            }
        } else {
            walk_expr(self, expr, ctx);
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

        if path.res.hir_id().is_none() {
            warn!(
                "Unable to assign slot for path '{}' on line {}, as it has not been resolved",
                path, path.span.start,
            );

            return;
        }

        let slot = symbol_table
            .borrow()
            .get_slot(|s| s.hir_id == path.res.hir_id());

        if let Some(slot) = slot {
            debug!("Assigning path '{}' to slot {:?}", path, slot,);

            path.res.set_slot(slot.into());
        } else {
            // TODO: Builtin symbols do not have a HirId, we should handle/resolve these somehow.
            warn!(
                "No symbols found for path '{}' (res.hir_id = {:?}) on line {}.\nCurrent scope: {:?}\nAvailable symbols: {:#?}",
                path,
                path.res.hir_id(),
                path.span.start,
                ctx.current_scope,
                symbol_table.borrow().get_all_declared_symbols()
            );
        }
    }
}

impl HirPass for SlotAllocator {
    fn init_context(&mut self, ctx: &mut HirOptContext) {
        self.scopes.push(ctx.current_scope);
    }

    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.visit_module(module, ctx);

        false
    }
}
