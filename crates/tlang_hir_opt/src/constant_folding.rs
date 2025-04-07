use std::collections::HashMap;
use tlang_ast::token::Literal;
use tlang_hir::{
    hir::{BinaryOpKind, Expr, ExprKind, HirId, Module, PatKind, Stmt, StmtKind},
    visit::{self, Visitor},
};

use crate::hir_opt::HirPass;

pub struct ConstantFolder {
    // Map from HirId to its constant value
    constants: HashMap<HirId, Literal>,
    // Track which expressions we've already folded
    folded_exprs: HashMap<HirId, Literal>,
    changed: bool,
}

impl Default for ConstantFolder {
    fn default() -> Self {
        Self::new()
    }
}

impl ConstantFolder {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
            folded_exprs: HashMap::new(),
            changed: false,
        }
    }

    pub fn optimize_module(&mut self, module: &mut Module) -> bool {
        self.changed = false;
        self.visit_module(module);
        self.changed
    }

    fn try_eval_binary_op(
        &self,
        op: BinaryOpKind,
        lhs: &Literal,
        rhs: &Literal,
    ) -> Option<Literal> {
        match (op, lhs, rhs) {
            (BinaryOpKind::Add, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::UnsignedInteger(l + r))
            }
            (BinaryOpKind::Sub, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::UnsignedInteger(l.saturating_sub(*r)))
            }
            (BinaryOpKind::Mul, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::UnsignedInteger(l * r))
            }
            _ => None,
        }
    }

    fn try_eval_expr(&self, expr: &Expr) -> Option<Literal> {
        // If we've already folded this expression, return None to avoid re-folding
        if let Some(lit) = self.folded_exprs.get(&expr.hir_id) {
            return Some(lit.clone());
        }

        match &expr.kind {
            ExprKind::Binary(op, lhs, rhs) => {
                let lhs_val = self.try_eval_expr(lhs)?;
                let rhs_val = self.try_eval_expr(rhs)?;
                self.try_eval_binary_op(*op, &lhs_val, &rhs_val)
            }
            ExprKind::Literal(lit) => Some(*lit.clone()),
            _ => None,
        }
    }
}

impl<'hir> Visitor<'hir> for ConstantFolder {
    fn visit_stmt(&mut self, stmt: &'hir mut Stmt) {
        match &mut stmt.kind {
            StmtKind::Let(pat, expr, _) => {
                self.visit_expr(expr);

                if let Some(lit) = self.try_eval_expr(expr) {
                    if let PatKind::Identifier(hir_id, _) = pat.kind {
                        // Only mark as changed if we haven't seen this constant before
                        if self.constants.get(&hir_id) != Some(&lit) {
                            self.constants.insert(hir_id, lit.clone());
                            self.changed = true;
                        }

                        expr.kind = ExprKind::Literal(Box::new(lit.clone()));
                        self.folded_exprs.insert(expr.hir_id, lit);
                    }
                }
            }
            _ => visit::walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'hir mut Expr) {
        visit::walk_expr(self, expr);

        if let Some(lit) = self.try_eval_expr(expr) {
            if self.folded_exprs.get(&expr.hir_id) != Some(&lit) {
                expr.kind = ExprKind::Literal(Box::new(lit.clone()));
                self.folded_exprs.insert(expr.hir_id, lit);
                self.changed = true;
            }
        }
    }
}

impl HirPass for ConstantFolder {
    fn optimize_module(&mut self, module: &mut Module) -> bool {
        self.changed = false;
        self.visit_module(module);
        self.changed
    }
}
