use std::collections::HashMap;
use tlang_ast::token::Literal;
use tlang_hir::hir::{BinaryOpKind, Expr, ExprKind, HirId, Module, PatKind, Res, Stmt, StmtKind};
use tlang_hir::visit::{Visitor, walk_expr, walk_stmt};

pub struct ConstantFolder {
    // Map from HirId to its constant value
    constants: HashMap<HirId, Literal>,
}

impl ConstantFolder {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
        }
    }

    pub fn optimize_module(&mut self, module: &mut Module) {
        self.visit_module(module);
    }

    fn try_eval_binary_op(&self, op: BinaryOpKind, lhs: &Literal, rhs: &Literal) -> Option<Literal> {
        match (op, lhs, rhs) {
            (BinaryOpKind::Add, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::UnsignedInteger(l + r))
            }
            _ => None,
        }
    }

    fn try_eval_expr(&self, expr: &Expr) -> Option<Literal> {
        match &expr.kind {
            ExprKind::Binary(op, lhs, rhs) => {
                let lhs_val = self.try_eval_expr(lhs)?;
                let rhs_val = self.try_eval_expr(rhs)?;
                self.try_eval_binary_op(*op, &lhs_val, &rhs_val)
            }
            ExprKind::Literal(lit) => Some(lit.clone()),
            ExprKind::Path(path) => {
                let hir_id = path.hir_id?;
                self.constants.get(&hir_id).cloned()
            }
            _ => None,
        }
    }
}

impl<'hir> Visitor<'hir> for ConstantFolder {
    fn visit_stmt(&mut self, stmt: &'hir mut Stmt) {
        match &mut stmt.kind {
            StmtKind::Let(pat, expr, _) => {
                // Visit the expression first to fold any constants
                self.visit_expr(expr);

                // Try to evaluate the expression
                if let Some(lit) = self.try_eval_expr(expr) {
                    // If we can evaluate it, store the constant
                    if let Some(hir_id) = pat.hir_id {
                        self.constants.insert(hir_id, lit.clone());
                    }
                    // Replace the expression with the literal
                    expr.kind = ExprKind::Literal(lit);
                }
            }
            _ => {
                // For other statements, just visit them
                walk_stmt(self, stmt);
            }
        }
    }

    fn visit_expr(&mut self, expr: &'hir mut Expr) {
        // Visit child expressions first (bottom-up)
        walk_expr(self, expr);

        // Try to evaluate the expression
        if let Some(lit) = self.try_eval_expr(expr) {
            expr.kind = ExprKind::Literal(lit);
        }
    }
}


