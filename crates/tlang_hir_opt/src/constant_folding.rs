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
}

impl ConstantFolder {
    pub fn new() -> Self {
        Self {
            constants: HashMap::new(),
        }
    }

    pub fn optimize_module(&mut self, module: &mut Module) {
        println!("Starting module optimization");
        self.visit_module(module);
    }

    fn try_eval_binary_op(&self, op: BinaryOpKind, lhs: &Literal, rhs: &Literal) -> Option<Literal> {
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
        println!("Visiting statement: {:?}", stmt.kind);
        match &mut stmt.kind {
            StmtKind::Let(pat, expr, _) => {
                println!("Found let statement");
                // First visit the expression to fold any constants in it
                self.visit_expr(expr);

                // If the expression is now a constant, store it for future use
                if let Some(lit) = self.try_eval_expr(expr) {
                    println!("  Evaluated expression to: {:?}", lit);
                    if let PatKind::Identifier(hir_id, _) = pat.kind {
                        println!("  Storing constant for hir_id: {:?}", hir_id);
                        self.constants.insert(hir_id, lit.clone());
                        // Replace the expression with the constant
                        expr.kind = ExprKind::Literal(Box::new(lit));
                    }
                }
            }
            StmtKind::Expr(expr) => {
                println!("Found expression statement");
                self.visit_expr(expr);
            }
            _ => visit::walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'hir mut Expr) {
        // Visit child expressions first (bottom-up)
        visit::walk_expr(self, expr);

        // Try to evaluate the expression
        if let Some(lit) = self.try_eval_expr(expr) {
            expr.kind = ExprKind::Literal(Box::new(lit));
        }
    }
}

impl HirPass for ConstantFolder {
    fn optimize_module(&mut self, module: &mut Module) {
        self.visit_module(module);
    }
}


