use std::collections::HashSet;
use tlang_hir::{
    hir::{Expr, ExprKind, HirId, Module, PatKind, Stmt, StmtKind},
    visit::Visitor,
};

use crate::hir_opt::HirPass;

trait UsageVisitor {
    fn visit_module(&mut self, module: &Module);
    fn visit_stmt(&mut self, stmt: &Stmt);
    fn visit_expr(&mut self, expr: &Expr);
}

impl<T: UsageVisitor> UsageVisitor for &mut T {
    fn visit_module(&mut self, module: &Module) {
        (**self).visit_module(module)
    }
    fn visit_stmt(&mut self, stmt: &Stmt) {
        (**self).visit_stmt(stmt)
    }
    fn visit_expr(&mut self, expr: &Expr) {
        (**self).visit_expr(expr)
    }
}

pub struct DeadCodeEliminator {
    used_vars: HashSet<HirId>,
    changed: bool,
    preserve_root: bool,
}

impl Default for DeadCodeEliminator {
    fn default() -> Self {
        Self::new()
    }
}

impl DeadCodeEliminator {
    pub fn new() -> Self {
        Self {
            used_vars: HashSet::new(),
            changed: false,
            preserve_root: false,
        }
    }

    pub fn with_preserve_root(mut self, preserve: bool) -> Self {
        self.preserve_root = preserve;
        self
    }
}

struct UsedVarsCollector {
    used_vars: HashSet<HirId>,
}

impl UsageVisitor for UsedVarsCollector {
    fn visit_module(&mut self, module: &Module) {
        for stmt in &module.block.stmts {
            self.visit_stmt(stmt);
        }
    }

    fn visit_stmt(&mut self, stmt: &Stmt) {
        match &stmt.kind {
            StmtKind::Let(_, expr, _) => {
                self.visit_expr(expr);
            }
            StmtKind::Expr(expr) => {
                self.visit_expr(expr);
            }
            _ => {}
        }
    }

    fn visit_expr(&mut self, expr: &Expr) {
        if let ExprKind::Path(path) = &expr.kind {
            if let Some(hir_id) = path.res.hir_id() {
                self.used_vars.insert(hir_id);
            }
        }

        match &expr.kind {
            ExprKind::Binary(_, lhs, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExprKind::Call(call) => {
                self.visit_expr(&call.callee);
                for arg in &call.arguments {
                    self.visit_expr(arg);
                }
            }
            _ => {}
        }
    }
}

impl<'hir> Visitor<'hir> for DeadCodeEliminator {
    fn visit_module(&mut self, module: &'hir mut Module) {
        if self.preserve_root {
            // If preserving root, just visit children without eliminating
            for stmt in &mut module.block.stmts {
                self.visit_stmt(stmt);
            }
        } else {
            // Otherwise proceed with dead code elimination
            module.block.stmts.retain_mut(|stmt| {
                match &stmt.kind {
                    StmtKind::Let(pat, expr, _) => {
                        // Keep let bindings for used variables
                        if let PatKind::Identifier(hir_id, _) = pat.kind {
                            if !self.used_vars.contains(&hir_id) {
                                // Keep the statement only if it has side effects
                                match &expr.kind {
                                    ExprKind::Call(_) => {
                                        // Convert to expression statement
                                        stmt.kind = StmtKind::Expr(expr.clone());
                                        self.changed = true;
                                        true
                                    }
                                    _ => {
                                        // Remove the statement
                                        self.changed = true;
                                        false
                                    }
                                }
                            } else {
                                true
                            }
                        } else {
                            true
                        }
                    }
                    _ => true,
                }
            });
        }
    }
}

impl HirPass for DeadCodeEliminator {
    fn optimize_module(&mut self, module: &mut Module) -> bool {
        self.changed = false;
        self.used_vars.clear();

        // Collect used variables
        let mut collector = UsedVarsCollector {
            used_vars: HashSet::new(),
        };
        collector.visit_module(module);
        self.used_vars = collector.used_vars;

        // Apply dead code elimination
        self.visit_module(module);
        self.changed
    }
}
