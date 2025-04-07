use log::{debug, trace};
use std::collections::HashSet;
use tlang_hir::{
    hir::{Block, ExprKind, HirId, Module, Pat, PatKind, StmtKind},
    visit::{self, Visitor},
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

pub struct UsedVarsCollector {
    pub used_vars: HashSet<HirId>,
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
        debug!("Collected used variables: {:?}", collector.used_vars);
        collector.used_vars
    }
}

impl<'hir> Visitor<'hir> for UsedVarsCollector {
    fn visit_pat(&mut self, pat: &'hir mut Pat) {
        trace!("Visiting pattern: {:?}", pat.kind);

        match &pat.kind {
            PatKind::Identifier(hir_id, _) => {
                trace!("Found identifier pattern with HirId: {:?}", hir_id);

                self.used_vars.insert(*hir_id);
            }
            _ => visit::walk_pat(self, pat),
        }
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
                            debug!("Found unused variable: {:?}", hir_id);
                            // Keep the statement only if it has side effects
                            match &expr.kind {
                                ExprKind::Call(_) => {
                                    debug!(
                                        "  Converting to expression statement due to side effects"
                                    );
                                    // Convert to expression statement
                                    block.stmts[i].kind = StmtKind::Expr(expr.clone());
                                    self.changed = true;
                                    i += 1;
                                }
                                _ => {
                                    debug!("  Removing unused variable");
                                    // Remove the statement
                                    block.stmts.remove(i);
                                    self.changed = true;
                                }
                            }
                        } else {
                            debug!("Keeping used variable: {:?}", hir_id);
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
        self.used_vars = UsedVarsCollector::collect(module);
        self.changed = false;
        self.visit_module(module);
        self.changed
    }
}
