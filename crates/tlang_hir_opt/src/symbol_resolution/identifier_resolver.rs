use log::{debug, warn};
use tlang_hir::visit::walk_expr;
use tlang_hir::{Visitor, hir};
use tlang_span::HirId;

use crate::HirPass;
use crate::hir_opt::HirOptContext;

#[derive(Debug, Default)]
pub struct IdentifierResolver {
    scopes: Vec<HirId>,
}

impl IdentifierResolver {
    fn resolve_path(&mut self, path: &mut hir::Path, ctx: &mut HirOptContext) {
        debug!("Resolving path: '{}' on line {}", path, path.span.start);

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

        let path_string = path.to_string();
        let symbol_info = symbol_table
            .borrow()
            // TODO: Also match function names with explicit arity in their name
            .get_closest_by_name(&path_string, path.span)
            .or_else(|| {
                if path_string.contains('/') {
                    let (name, arity) = path_string
                        .rsplit_once('/')
                        .expect("Path should contain a '/' for arity");
                    let arity = arity
                        .parse::<usize>()
                        .expect("Arity should be a valid usize");

                    symbol_table
                        .borrow()
                        .get_by_name_and_arity(name, arity)
                        .first()
                        .cloned()
                } else {
                    None
                }
            });

        if let Some(symbol_info) = symbol_info {
            path.res.set_binding_kind(symbol_info.symbol_type.into());

            if let Some(hir_id) = symbol_info.hir_id {
                debug!(
                    "Resolved path '{}' on line {} to {:?}",
                    path, path.span.start, hir_id
                );

                path.res.set_hir_id(hir_id);
            } else if symbol_info.is_builtin() {
                if let Some(slot) = symbol_info.global_slot() {
                    debug!(
                        "Path '{}' on line {} is a builtin symbol with global slot {}.",
                        path, path.span.start, slot
                    );
                    path.res.set_slot(hir::Slot::Global(slot));
                } else {
                    debug!(
                        "Path '{}' on line {} is a builtin symbol with no slot (module/type).",
                        path, path.span.start
                    );
                }
            } else {
                debug!(
                    "Symbol '{}' on line {} has no HirId, skipping resolution.",
                    path, path.span.start
                );
            }
        } else {
            warn!(
                "No symbols found for path '{}' on line {}. Available symbols: {:#?}",
                path,
                path.span.start,
                symbol_table.borrow().get_all_declared_symbols()
            );
        }
    }
}

impl<'hir> Visitor<'hir> for IdentifierResolver {
    type Context = HirOptContext;

    fn enter_scope(&mut self, hir_id: HirId, ctx: &mut Self::Context) {
        if ctx.symbols.contains_key(&hir_id) {
            debug!("Entering scope for: {hir_id:?}");
            ctx.current_scope = hir_id;
            self.scopes.push(hir_id);
        } else {
            debug!(
                "Scope skipped for: {:?}, current scope: {:?}",
                hir_id, ctx.current_scope
            );
        }
    }

    fn leave_scope(&mut self, hir_id: HirId, ctx: &mut Self::Context) {
        if ctx.symbols.contains_key(&hir_id) {
            let left_scope = self.scopes.pop();

            debug!("Leaving scope for: {hir_id:?}");
            debug_assert!(left_scope == Some(hir_id), "Mismatched scope exit");

            ctx.current_scope = self.scopes.last().copied().unwrap();
        }
    }

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut Self::Context) {
        match &mut expr.kind {
            hir::ExprKind::Dict(pairs) => {
                for (key, value) in pairs.iter_mut() {
                    if !key.is_path() {
                        self.visit_expr(key, ctx);
                    }
                    self.visit_expr(value, ctx);
                }
            }
            _ => walk_expr(self, expr, ctx),
        }
    }

    fn visit_path(&mut self, path: &'hir mut hir::Path, ctx: &mut Self::Context) {
        self.resolve_path(path, ctx);
    }
}

impl HirPass for IdentifierResolver {
    fn init_context(&mut self, ctx: &mut HirOptContext) {
        self.enter_scope(ctx.current_scope, ctx);
    }

    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.visit_module(module, ctx);

        false
    }
}
