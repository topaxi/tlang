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
        println!("Starting module optimization");
        self.visit_module(module);
    }

    fn try_eval_binary_op(&self, op: BinaryOpKind, lhs: &Expr, rhs: &Expr) -> Option<Literal> {
        println!("Evaluating binary op: {:?}", op);
        let lhs_lit = self.try_eval_expr(lhs)?;
        let rhs_lit = self.try_eval_expr(rhs)?;
        println!("  lhs_lit: {:?}, rhs_lit: {:?}", lhs_lit, rhs_lit);

        let result = match (op, lhs_lit, rhs_lit) {
            (BinaryOpKind::Add, Literal::UnsignedInteger(a), Literal::UnsignedInteger(b)) => {
                Some(Literal::UnsignedInteger(a + b))
            }
            (BinaryOpKind::Sub, Literal::UnsignedInteger(a), Literal::UnsignedInteger(b)) => {
                Some(Literal::UnsignedInteger(a.saturating_sub(b)))
            }
            (BinaryOpKind::Mul, Literal::UnsignedInteger(a), Literal::UnsignedInteger(b)) => {
                Some(Literal::UnsignedInteger(a * b))
            }
            (BinaryOpKind::Div, Literal::UnsignedInteger(a), Literal::UnsignedInteger(b)) if b != 0 => {
                Some(Literal::UnsignedInteger(a / b))
            }
            _ => None,
        };
        println!("  result: {:?}", result);
        result
    }

    fn try_eval_expr(&self, expr: &Expr) -> Option<Literal> {
        println!("Evaluating expr: {:?}", expr.kind);
        let result = match &expr.kind {
            ExprKind::Literal(lit) => {
                println!("  Found literal: {:?}", lit);
                Some(*lit.clone())
            }
            ExprKind::Path(path) => {
                println!("  Found path: {:?}", path);
                if let Res::Local(hir_id, _) = path.res {
                    let const_val = self.constants.get(&hir_id).cloned();
                    println!("    Resolved to constant: {:?}", const_val);
                    const_val
                } else {
                    None
                }
            }
            ExprKind::Binary(op, lhs, rhs) => {
                println!("  Found binary op: {:?}", op);
                let lhs_val = self.try_eval_expr(lhs)?;
                let rhs_val = self.try_eval_expr(rhs)?;
                println!("    lhs_val: {:?}, rhs_val: {:?}", lhs_val, rhs_val);
                match (op, lhs_val, rhs_val) {
                    (BinaryOpKind::Add, Literal::UnsignedInteger(a), Literal::UnsignedInteger(b)) => {
                        Some(Literal::UnsignedInteger(a + b))
                    }
                    (BinaryOpKind::Sub, Literal::UnsignedInteger(a), Literal::UnsignedInteger(b)) => {
                        Some(Literal::UnsignedInteger(a.saturating_sub(b)))
                    }
                    (BinaryOpKind::Mul, Literal::UnsignedInteger(a), Literal::UnsignedInteger(b)) => {
                        Some(Literal::UnsignedInteger(a * b))
                    }
                    (BinaryOpKind::Div, Literal::UnsignedInteger(a), Literal::UnsignedInteger(b)) if b != 0 => {
                        Some(Literal::UnsignedInteger(a / b))
                    }
                    _ => None,
                }
            }
            _ => None,
        };
        println!("  result: {:?}", result);
        result
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
            _ => walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'hir mut Expr) {
        println!("Visiting expression: {:?}", expr.kind);
        match &mut expr.kind {
            ExprKind::Binary(op, lhs, rhs) => {
                println!("Found binary expression: {:?}", op);
                // First visit both sides to fold any constants in them
                self.visit_expr(lhs);
                self.visit_expr(rhs);

                // Try to evaluate the binary operation
                if let Some(lit) = self.try_eval_binary_op(*op, lhs, rhs) {
                    println!("  Folded to constant: {:?}", lit);
                    expr.kind = ExprKind::Literal(Box::new(lit));
                }
            }
            ExprKind::Path(path) => {
                println!("Found path expression: {:?}", path);
                if let Res::Local(hir_id, _) = path.res {
                    if let Some(lit) = self.constants.get(&hir_id).cloned() {
                        println!("  Replaced with constant: {:?}", lit);
                        expr.kind = ExprKind::Literal(Box::new(lit));
                    }
                }
            }
            ExprKind::Call(call_expr) => {
                // Visit the callee and arguments
                self.visit_expr(&mut call_expr.callee);
                for arg in &mut call_expr.arguments {
                    self.visit_expr(arg);
                }
            }
            _ => walk_expr(self, expr),
        }
    }
}


