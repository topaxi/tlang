use std::collections::HashSet;
use tlang_hir::{
    hir::{Block, Expr, ExprKind, HirId, Module, PatKind, Stmt, StmtKind},
    visit::{Visitor, walk_expr},
};

use crate::hir_opt::HirPass;

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

impl UsedVarsCollector {
    fn new() -> Self {
        Self {
            used_vars: HashSet::new(),
        }
    }

    fn collect(module: &mut Module) -> HashSet<HirId> {
        let mut collector = Self::new();
        collector.visit_module(module);
        println!("\nCollected used variables: {:?}", collector.used_vars);
        collector.used_vars
    }

    fn visit_block(&mut self, block: &Block) {
        for stmt in &block.stmts {
            self.visit_stmt(stmt);
        }
        if let Some(expr) = &block.expr {
            self.visit_expr(expr);
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
        // First collect any path references
        if let ExprKind::Path(path) = &expr.kind {
            if let Some(hir_id) = path.res.hir_id() {
                println!("Found used variable: {:?} from path: {}", hir_id, path.segments[0].ident);
                self.used_vars.insert(hir_id);
            }
        }

        // Then visit all child expressions
        match &expr.kind {
            ExprKind::Block(block) => {
                self.visit_block(block);
            }
            ExprKind::Call(call) => {
                self.visit_expr(&call.callee);
                for arg in &call.arguments {
                    self.visit_expr(arg);
                }
            }
            ExprKind::Binary(_, lhs, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            ExprKind::IfElse(cond, then_block, else_clauses) => {
                self.visit_expr(cond);
                self.visit_block(then_block);
                for else_clause in else_clauses {
                    if let Some(cond) = &else_clause.condition {
                        self.visit_expr(cond);
                    }
                    self.visit_block(&else_clause.consequence);
                }
            }
            _ => {}
        }
    }

    fn visit_module(&mut self, module: &Module) {
        self.visit_block(&module.block);
    }
}

impl<'hir> Visitor<'hir> for DeadCodeEliminator {
    fn visit_module(&mut self, module: &'hir mut Module) {
        if !self.preserve_root {
            self.visit_block(&mut module.block);
        }
    }

    fn visit_block(&mut self, block: &'hir mut Block) {
        // Process block and remove unused statements
        let mut i = 0;
        while i < block.stmts.len() {
            match &block.stmts[i].kind {
                StmtKind::Let(pat, expr, _) => {
                    // Keep let bindings for used variables
                    if let PatKind::Identifier(hir_id, _) = pat.kind {
                        if !self.used_vars.contains(&hir_id) {
                            println!("Found unused variable: {:?}", hir_id);
                            // Keep the statement only if it has side effects
                            match &expr.kind {
                                ExprKind::Call(_) => {
                                    println!("  Converting to expression statement due to side effects");
                                    // Convert to expression statement
                                    block.stmts[i].kind = StmtKind::Expr(expr.clone());
                                    self.changed = true;
                                    i += 1;
                                }
                                _ => {
                                    println!("  Removing unused variable");
                                    // Remove the statement
                                    block.stmts.remove(i);
                                    self.changed = true;
                                }
                            }
                        } else {
                            println!("Keeping used variable: {:?}", hir_id);
                            i += 1;
                        }
                    } else {
                        i += 1;
                    }
                }
                _ => {
                    i += 1;
                }
            }
        }

        // Visit children after eliminating dead code
        for stmt in &mut block.stmts {
            self.visit_stmt(stmt);
        }
        if let Some(expr) = &mut block.expr {
            self.visit_expr(expr);
        }
    }
}

impl HirPass for DeadCodeEliminator {
    fn optimize_module(&mut self, module: &mut Module) -> bool {
        // First collect all used variables
        self.used_vars = UsedVarsCollector::collect(module);
        self.changed = false;

        // Then eliminate dead code
        self.visit_module(module);

        self.changed
    }
}
