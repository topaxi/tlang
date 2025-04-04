use std::collections::{HashMap, HashSet};
use tlang_ast::token::Literal;
use tlang_hir::{
    hir::{BinaryOpKind, Expr, ExprKind, HirId, Module, Pat, PatKind, Stmt, StmtKind},
    visit::{self, Visitor},
};

use crate::hir_opt::HirPass;

#[derive(Default)]
pub struct AssignmentCollector {
    reassigned_variables: HashSet<HirId>,
}

impl AssignmentCollector {
    pub fn collect(module: &mut Module) -> HashSet<HirId> {
        let mut collector = Self::default();
        collector.visit_module(module);
        collector.reassigned_variables
    }
}

impl<'hir> Visitor<'hir> for AssignmentCollector {
    fn visit_expr(&mut self, expr: &'hir mut Expr) {
        if let ExprKind::Binary(BinaryOpKind::Assign, lhs, _rhs) = &mut expr.kind {
            if let ExprKind::Path(lhs_path) = &lhs.kind {
                if let Some(resolved_hir_id) = lhs_path.res.hir_id() {
                    self.reassigned_variables.insert(resolved_hir_id);
                }
            }
        }

        visit::walk_expr(self, expr);
    }
}

pub struct ConstantPropagator {
    constants: HashMap<HirId, Literal>,
    reassigned_variables: HashSet<HirId>,
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
            reassigned_variables: HashSet::new(),
            changed: false,
        }
    }
}

// Implement the Visitor trait for the propagator.
impl<'hir> Visitor<'hir> for ConstantPropagator {
    fn visit_stmt(&mut self, stmt: &'hir mut Stmt) {
        match &mut stmt.kind {
            // Handle 'let' statements.
            StmtKind::Let(
                box Pat {
                    kind: PatKind::Identifier(hir_id, _ident),
                    ..
                },
                expr,
                ..,
            ) => {
                self.visit_expr(expr);

                if !self.reassigned_variables.contains(hir_id) {
                    match &expr.kind {
                        ExprKind::Literal(lit) => {
                            self.constants.insert(*hir_id, *lit.clone());
                        }
                        _ => { /* Not a literal, do nothing */ }
                    }
                }
                return;
            }
            _ => visit::walk_stmt(self, stmt),
        }
    }

    fn visit_expr(&mut self, expr: &'hir mut Expr) {
        if let ExprKind::Path(path) = &expr.kind {
            if let Some(resolved_hir_id) = path.res.hir_id() {
                if let Some(lit) = self.constants.get(&resolved_hir_id) {
                    expr.kind = ExprKind::Literal(Box::new(lit.clone()));
                    self.changed = true;
                    return;
                }
            }
            return;
        }

        visit::walk_expr(self, expr);
    }
}

impl HirPass for ConstantPropagator {
    fn optimize_module(&mut self, module: &mut Module) -> bool {
        self.constants.clear();
        self.reassigned_variables = AssignmentCollector::collect(module);

        self.changed = false;
        self.visit_module(module);
        self.changed
    }
}
