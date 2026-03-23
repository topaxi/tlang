use std::collections::HashMap;
use tlang_ast::{node::UnaryOp, token::Literal};
use tlang_hir::{
    self as hir, BinaryOpKind, Expr, ExprKind,
    visit::{self, Visitor},
};
use tlang_span::HirId;

use crate::hir_opt::{HirOptContext, HirOptError, HirPass};

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

    #[allow(clippy::too_many_lines)]
    fn try_eval_binary_op(
        &self,
        op: BinaryOpKind,
        lhs: &Literal,
        rhs: &Literal,
    ) -> Option<Literal> {
        match (op, lhs, rhs) {
            // Unsigned integer arithmetic
            (BinaryOpKind::Add, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::UnsignedInteger(l + r))
            }
            (BinaryOpKind::Sub, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::UnsignedInteger(l.saturating_sub(*r)))
            }
            (BinaryOpKind::Mul, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::UnsignedInteger(l * r))
            }
            (BinaryOpKind::Div, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                if *r == 0 {
                    None
                } else {
                    Some(Literal::UnsignedInteger(l / r))
                }
            }
            (BinaryOpKind::Mod, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                if *r == 0 {
                    None
                } else {
                    Some(Literal::UnsignedInteger(l % r))
                }
            }
            (BinaryOpKind::Exp, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::UnsignedInteger(l.pow(*r as u32)))
            }

            // Unsigned integer comparisons
            (BinaryOpKind::Eq, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::Boolean(l == r))
            }
            (BinaryOpKind::NotEq, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::Boolean(l != r))
            }
            (BinaryOpKind::Less, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::Boolean(l < r))
            }
            (BinaryOpKind::LessEq, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::Boolean(l <= r))
            }
            (BinaryOpKind::Greater, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::Boolean(l > r))
            }
            (BinaryOpKind::GreaterEq, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::Boolean(l >= r))
            }

            // Unsigned integer bitwise
            (
                BinaryOpKind::BitwiseAnd,
                Literal::UnsignedInteger(l),
                Literal::UnsignedInteger(r),
            ) => Some(Literal::UnsignedInteger(l & r)),
            (BinaryOpKind::BitwiseOr, Literal::UnsignedInteger(l), Literal::UnsignedInteger(r)) => {
                Some(Literal::UnsignedInteger(l | r))
            }
            (
                BinaryOpKind::BitwiseXor,
                Literal::UnsignedInteger(l),
                Literal::UnsignedInteger(r),
            ) => Some(Literal::UnsignedInteger(l ^ r)),

            // Signed integer arithmetic
            (BinaryOpKind::Add, Literal::Integer(l), Literal::Integer(r)) => {
                l.checked_add(*r).map(Literal::Integer)
            }
            (BinaryOpKind::Sub, Literal::Integer(l), Literal::Integer(r)) => {
                l.checked_sub(*r).map(Literal::Integer)
            }
            (BinaryOpKind::Mul, Literal::Integer(l), Literal::Integer(r)) => {
                l.checked_mul(*r).map(Literal::Integer)
            }
            (BinaryOpKind::Div, Literal::Integer(l), Literal::Integer(r)) => {
                if *r == 0 {
                    None
                } else {
                    l.checked_div(*r).map(Literal::Integer)
                }
            }
            (BinaryOpKind::Mod, Literal::Integer(l), Literal::Integer(r)) => {
                if *r == 0 {
                    None
                } else {
                    l.checked_rem(*r).map(Literal::Integer)
                }
            }
            (BinaryOpKind::Exp, Literal::Integer(l), Literal::Integer(r)) if *r >= 0 => {
                l.checked_pow(*r as u32).map(Literal::Integer)
            }

            // Signed integer comparisons
            (BinaryOpKind::Eq, Literal::Integer(l), Literal::Integer(r)) => {
                Some(Literal::Boolean(l == r))
            }
            (BinaryOpKind::NotEq, Literal::Integer(l), Literal::Integer(r)) => {
                Some(Literal::Boolean(l != r))
            }
            (BinaryOpKind::Less, Literal::Integer(l), Literal::Integer(r)) => {
                Some(Literal::Boolean(l < r))
            }
            (BinaryOpKind::LessEq, Literal::Integer(l), Literal::Integer(r)) => {
                Some(Literal::Boolean(l <= r))
            }
            (BinaryOpKind::Greater, Literal::Integer(l), Literal::Integer(r)) => {
                Some(Literal::Boolean(l > r))
            }
            (BinaryOpKind::GreaterEq, Literal::Integer(l), Literal::Integer(r)) => {
                Some(Literal::Boolean(l >= r))
            }

            // Signed integer bitwise
            (BinaryOpKind::BitwiseAnd, Literal::Integer(l), Literal::Integer(r)) => {
                Some(Literal::Integer(l & r))
            }
            (BinaryOpKind::BitwiseOr, Literal::Integer(l), Literal::Integer(r)) => {
                Some(Literal::Integer(l | r))
            }
            (BinaryOpKind::BitwiseXor, Literal::Integer(l), Literal::Integer(r)) => {
                Some(Literal::Integer(l ^ r))
            }

            // Float arithmetic
            (BinaryOpKind::Add, Literal::Float(l), Literal::Float(r)) => {
                let result = l + r;
                if result.is_finite() {
                    Some(Literal::Float(result))
                } else {
                    None
                }
            }
            (BinaryOpKind::Sub, Literal::Float(l), Literal::Float(r)) => {
                let result = l - r;
                if result.is_finite() {
                    Some(Literal::Float(result))
                } else {
                    None
                }
            }
            (BinaryOpKind::Mul, Literal::Float(l), Literal::Float(r)) => {
                let result = l * r;
                if result.is_finite() {
                    Some(Literal::Float(result))
                } else {
                    None
                }
            }
            (BinaryOpKind::Div, Literal::Float(l), Literal::Float(r)) => {
                let result = l / r;
                if result.is_finite() {
                    Some(Literal::Float(result))
                } else {
                    None
                }
            }
            (BinaryOpKind::Mod, Literal::Float(l), Literal::Float(r)) => {
                let result = l % r;
                if result.is_finite() {
                    Some(Literal::Float(result))
                } else {
                    None
                }
            }

            // Float comparisons
            (BinaryOpKind::Eq, Literal::Float(l), Literal::Float(r)) => {
                Some(Literal::Boolean(l == r))
            }
            (BinaryOpKind::NotEq, Literal::Float(l), Literal::Float(r)) => {
                Some(Literal::Boolean(l != r))
            }
            (BinaryOpKind::Less, Literal::Float(l), Literal::Float(r)) => {
                Some(Literal::Boolean(l < r))
            }
            (BinaryOpKind::LessEq, Literal::Float(l), Literal::Float(r)) => {
                Some(Literal::Boolean(l <= r))
            }
            (BinaryOpKind::Greater, Literal::Float(l), Literal::Float(r)) => {
                Some(Literal::Boolean(l > r))
            }
            (BinaryOpKind::GreaterEq, Literal::Float(l), Literal::Float(r)) => {
                Some(Literal::Boolean(l >= r))
            }

            // Booleans
            (BinaryOpKind::And, Literal::Boolean(lhs), Literal::Boolean(rhs)) => {
                Some(Literal::Boolean(*lhs && *rhs))
            }
            (BinaryOpKind::Or, Literal::Boolean(lhs), Literal::Boolean(rhs)) => {
                Some(Literal::Boolean(*lhs || *rhs))
            }
            (BinaryOpKind::Eq, Literal::Boolean(lhs), Literal::Boolean(rhs)) => {
                Some(Literal::Boolean(lhs == rhs))
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
            (UnaryOp::Minus, Literal::UnsignedInteger(v)) => Some(Literal::Integer(-(*v as i64))),
            (UnaryOp::Minus, Literal::Integer(v)) => v.checked_neg().map(Literal::Integer),
            (UnaryOp::Minus, Literal::Float(v)) => Some(Literal::Float(-v)),
            _ => None,
        }
    }

    fn try_eval_expr(&self, expr: &Expr) -> Option<Literal> {
        if let Some(lit) = self.folded_exprs.get(&expr.hir_id) {
            return Some(*lit);
        }

        match &expr.kind {
            ExprKind::Unary(op, expr) => self.try_eval_unary_op(*op, &self.try_eval_expr(expr)?),
            ExprKind::Binary(op, lhs, rhs) => {
                let lhs_val = self.try_eval_expr(lhs)?;
                let rhs_val = self.try_eval_expr(rhs)?;

                self.try_eval_binary_op(*op, &lhs_val, &rhs_val)
            }
            ExprKind::Literal(box lit) => Some(*lit),
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
            expr.kind = ExprKind::Literal(Box::new(lit));
            self.folded_exprs.insert(expr.hir_id, lit);
            self.changed = true;
        }
    }
}

impl HirPass for ConstantFolder {
    fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        _ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        self.changed = false;
        self.visit_module(module, &mut ());
        Ok(self.changed)
    }
}
