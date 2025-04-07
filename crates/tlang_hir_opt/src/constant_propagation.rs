use std::collections::HashMap;

use tlang_ast::token::Literal;
use tlang_hir::{
    hir::{Expr, ExprKind, HirId, Module, Pat, PatKind, Stmt, StmtKind},
    visit::{self, Visitor},
};

use crate::hir_opt::HirPass;

pub struct ConstantPropagator {
    constants: HashMap<HirId, Literal>,
    changed: bool,
}

impl Default for ConstantPropagator {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstantPropagator {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
            changed: false,
        }
    }
}

impl<'hir> Visitor<'hir> for ConstantPropagator {
    fn visit_stmt(&mut self, stmt: &'hir mut Stmt) {
        match &mut stmt.kind {
            StmtKind::Let(
                box Pat {
                    kind: PatKind::Identifier(hir_id, _),
                    ..
                },
                expr,
                ..,
            ) => {
                self.visit_expr(expr);

                if let ExprKind::Literal(lit) = &expr.kind {
                    self.constants.insert(*hir_id, *lit.clone());
                }
            }
            _ => visit::walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'hir mut Expr) {
        visit::walk_expr(self, expr);

        if let ExprKind::Path(path) = &expr.kind {
            if let Some(lit) = path
                .res
                .hir_id()
                .and_then(|hir_id| self.constants.get(&hir_id))
                .cloned()
            {
                expr.kind = ExprKind::Literal(Box::new(lit));
                self.changed = true;
            }
        }
    }
}

impl HirPass for ConstantPropagator {
    fn optimize_module(&mut self, module: &mut Module) -> bool {
        self.changed = false;
        self.visit_module(module);
        self.changed
    }
}
