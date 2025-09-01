use log::debug;
use tlang_hir::hir::HirScope;
use tlang_hir::{Visitor, hir};

use crate::HirPass;
use crate::hir_opt::HirOptContext;

/// A HIR pass that updates scope data (locals count) for HIR nodes after slot allocation.
/// This pass should run after SlotAllocator to ensure accurate locals count for memory pre-allocation.
#[derive(Debug, Default)]
pub struct ScopeDataUpdater;

impl<'hir> Visitor<'hir> for ScopeDataUpdater {
    type Context = HirOptContext;

    fn visit_block(&mut self, block: &'hir mut hir::Block, ctx: &mut Self::Context) {
        // First walk the block normally
        tlang_hir::visit::walk_block(self, block, ctx);

        // Update the block's scope data with locals count
        if let Some(symbol_table) = ctx.symbols.get(&block.hir_id) {
            let locals_count = symbol_table.borrow().locals();
            debug!(
                "Setting block {:?} locals count to {}",
                block.hir_id, locals_count
            );
            block.set_locals(locals_count);
        }
    }

    fn visit_module(&mut self, module: &'hir mut hir::Module, ctx: &mut Self::Context) {
        // First walk the module normally
        tlang_hir::visit::walk_module(self, module, ctx);

        // Update the module's scope data with locals count
        if let Some(symbol_table) = ctx.symbols.get(&module.hir_id) {
            let locals_count = symbol_table.borrow().locals();
            debug!(
                "Setting module {:?} locals count to {}",
                module.hir_id, locals_count
            );
            module.set_locals(locals_count);
        } else {
            // Fallback: use current scope if module HIR ID doesn't have symbol table
            if let Some(symbol_table) = ctx.symbols.get(&ctx.current_scope) {
                let locals_count = symbol_table.borrow().locals();
                debug!(
                    "Using current scope {:?} for module {:?}, locals count: {}",
                    ctx.current_scope, module.hir_id, locals_count
                );
                module.set_locals(locals_count);
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, ctx: &mut Self::Context) {
        // Handle function declarations specially to update their scope data
        match &mut stmt.kind {
            hir::StmtKind::FunctionDeclaration(decl) => {
                let function_hir_id = decl.hir_id;

                // First process the statement normally
                tlang_hir::visit::walk_stmt(self, stmt, ctx);

                // Update the function's scope data with locals count
                if let Some(symbol_table) = ctx.symbols.get(&function_hir_id) {
                    let locals_count = symbol_table.borrow().locals();
                    debug!(
                        "Setting function {:?} locals count to {}",
                        function_hir_id, locals_count
                    );
                    if let hir::StmtKind::FunctionDeclaration(decl) = &mut stmt.kind {
                        decl.set_locals(locals_count);
                    }
                }
            }
            _ => {
                // For other statements, use the default walking
                tlang_hir::visit::walk_stmt(self, stmt, ctx);
            }
        }
    }

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut Self::Context) {
        // Handle function expressions (lambda functions) specially to update their scope data
        match &mut expr.kind {
            hir::ExprKind::FunctionExpression(decl) => {
                let function_hir_id = decl.hir_id;

                // First process the expression normally
                tlang_hir::visit::walk_expr(self, expr, ctx);

                // Update the function expression's scope data with locals count
                if let Some(symbol_table) = ctx.symbols.get(&function_hir_id) {
                    let locals_count = symbol_table.borrow().locals();
                    debug!(
                        "Setting lambda function {:?} locals count to {}",
                        function_hir_id, locals_count
                    );
                    if let hir::ExprKind::FunctionExpression(decl) = &mut expr.kind {
                        decl.set_locals(locals_count);
                    }
                }
            }
            _ => {
                // For other expressions, use the default walking
                tlang_hir::visit::walk_expr(self, expr, ctx);
            }
        }
    }
}

impl HirPass for ScopeDataUpdater {
    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.visit_module(module, ctx);
        false // This pass doesn't change the HIR structure, just updates metadata
    }
}
