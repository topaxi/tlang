use std::collections::HashMap;

use tlang_ast::token::Literal;
use tlang_hir::{
    hir::{Expr, ExprKind, HirId, Module, PatKind, Stmt, StmtKind},
    visit::{self, Visitor},
};

use crate::hir_opt::HirPass;

pub struct ConstantPropagator {
    constants: HashMap<HirId, Literal>,
}

impl ConstantPropagator {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
        }
    }
}

impl<'hir> Visitor<'hir> for ConstantPropagator {
    fn visit_stmt(&mut self, stmt: &'hir mut Stmt) {
        match &mut stmt.kind {
            StmtKind::Let(pat, expr, _) => {
                // Visit the expression first
                self.visit_expr(expr);

                // If the expression is a literal, store it for propagation
                if let ExprKind::Literal(lit) = &expr.kind {
                    if let PatKind::Identifier(hir_id, _) = pat.kind {
                        self.constants.insert(hir_id, *lit.clone());
                    }
                }
            }
            _ => {
                // For other statements, just visit them
                visit::walk_stmt(self, stmt);
            }
        }
    }

    fn visit_expr(&mut self, expr: &'hir mut Expr) {
        // Visit child expressions first
        visit::walk_expr(self, expr);

        // Try to propagate constants
        if let ExprKind::Path(path) = &expr.kind {
            if let Some(hir_id) = path.res.hir_id() {
                if let Some(lit) = self.constants.get(&hir_id).cloned() {
                    expr.kind = ExprKind::Literal(Box::new(lit));
                }
            }
        }
    }
}

impl HirPass for ConstantPropagator {
    fn optimize_module(&mut self, module: &mut Module) {
        self.visit_module(module);
    }
} 