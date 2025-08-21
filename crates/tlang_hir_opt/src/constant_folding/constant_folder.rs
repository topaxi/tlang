use std::collections::HashMap;
use tlang_ast::{node::UnaryOp, token::Literal};
use tlang_hir::{
    hir::{self, BinaryOpKind, Expr, ExprKind},
    visit::{self, Visitor},
};
use tlang_span::HirId;

use crate::hir_opt::{HirOptContext, HirPass};

pub struct ConstantFolder {
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
            folded_exprs: HashMap::new(),
            changed: false,
        }
    }

    fn try_eval_binary_op(
        &self,
        op: BinaryOpKind,
        lhs: &Literal,
        rhs: &Literal,
    ) -> Option<Literal> {
        match (op, lhs, rhs) {
            // Unsigned integers
            (BinaryOpKind::Add, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::UnsignedInteger(l + r))
            }
            (BinaryOpKind::Sub, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::UnsignedInteger(l.saturating_sub(*r)))
            }
            (BinaryOpKind::Mul, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::UnsignedInteger(l * r))
            }

            // Booleans
            (BinaryOpKind::And, Literal::Boolean(lhs), Literal::Boolean(rhs)) => {
                Some(Literal::Boolean(*lhs && *rhs))
            }
            (BinaryOpKind::Or, Literal::Boolean(lhs), Literal::Boolean(rhs)) => {
                Some(Literal::Boolean(*lhs || *rhs))
            }
            (BinaryOpKind::NotEq, Literal::Boolean(lhs), Literal::Boolean(rhs)) => {
                Some(Literal::Boolean(lhs != rhs))
            }

            // Everything else we don't support yet
            _ => None,
        }
    }

    fn try_eval_unary_op(&self, op: UnaryOp, val: &Literal) -> Option<Literal> {
        match (op, val) {
            (UnaryOp::Not, Literal::Boolean(v)) => Some(Literal::Boolean(!v)),
            _ => None,
        }
    }

    fn try_eval_expr(&self, expr: &Expr) -> Option<Literal> {
        if let Some(lit) = self.folded_exprs.get(&expr.hir_id) {
            return Some(lit.clone());
        }

        match &expr.kind {
            ExprKind::Unary(op, expr) => self.try_eval_unary_op(*op, &self.try_eval_expr(expr)?),
            ExprKind::Binary(op, lhs, rhs) => {
                let lhs_val = self.try_eval_expr(lhs)?;
                let rhs_val = self.try_eval_expr(rhs)?;

                self.try_eval_binary_op(*op, &lhs_val, &rhs_val)
            }
            ExprKind::Literal(box lit) => Some(lit.clone()),
            _ => None,
        }
    }
}

impl<'hir> Visitor<'hir> for ConstantFolder {
    fn visit_expr(&mut self, expr: &'hir mut Expr, ctx: &mut Self::Context) {
        visit::walk_expr(self, expr, ctx);

        if let Some(lit) = self.try_eval_expr(expr)
            && self.folded_exprs.get(&expr.hir_id) != Some(&lit)
        {
            expr.kind = ExprKind::Literal(Box::new(lit.clone()));
            self.folded_exprs.insert(expr.hir_id, lit);
            self.changed = true;
        }
    }
}

impl HirPass for ConstantFolder {
    fn optimize_hir(&mut self, module: &mut hir::Module, _ctx: &mut HirOptContext) -> bool {
        self.changed = false;
        self.visit_module(module, &mut ());
        self.changed
    }
}
