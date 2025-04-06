use log::{debug, trace};
use std::collections::{HashMap, HashSet};
use tlang_hir::{
    hir::{Block, Expr, ExprKind, HirId, Module, Pat, PatKind, Path, Res, StmtKind},
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
    fn visit_expr(&mut self, expr: &'hir mut Expr) {
        trace!("Visiting expression: {:?}", expr.kind);
        match &mut expr.kind {
            ExprKind::Match(scrutinee, arms) => {
                trace!("Processing match expression with {} arms", arms.len());
                self.visit_expr(scrutinee);
                for (i, arm) in arms.iter_mut().enumerate() {
                    trace!("Processing match arm {}", i);
                    self.visit_pat(&mut arm.pat);
                    if let Some(guard) = &mut arm.guard {
                        trace!("Processing guard expression");
                        self.visit_expr(guard);
                    }
                    self.visit_block(&mut arm.block);
                }
            }
            _ => {
                visit::walk_expr(self, expr);
            }
        }
    }

    fn visit_pat(&mut self, pat: &'hir mut Pat) {
        trace!("Visiting pattern: {:?}", pat.kind);
        match &pat.kind {
            PatKind::Identifier(hir_id, _) => {
                trace!("Found identifier pattern with HirId: {:?}", hir_id);
                self.used_vars.insert(*hir_id);
            }
            _ => {
                visit::walk_pat(self, pat);
            }
        }
    }
}

#[derive(Debug)]
struct ScopeInfo {
    // Maps HirId to its new slot index in this scope
    local_slots: HashMap<HirId, usize>,
    // Maps parent scope HirId to its upvar slot index in this scope
    upvar_slots: HashMap<HirId, usize>,
    // Current slot index for local variables
    next_local_slot: usize,
}

impl ScopeInfo {
    fn new() -> Self {
        Self {
            local_slots: HashMap::new(),
            upvar_slots: HashMap::new(),
            next_local_slot: 0,
        }
    }

    fn next_local(&mut self) -> usize {
        let slot = self.next_local_slot;
        self.next_local_slot += 1;
        slot
    }
}

struct SlotUpdater {
    // Stack of scope information
    scopes: Vec<ScopeInfo>,
    // Set of used variables
    used_vars: HashSet<HirId>,
}

impl SlotUpdater {
    fn new(used_vars: HashSet<HirId>) -> Self {
        Self {
            scopes: vec![ScopeInfo::new()],
            used_vars,
        }
    }

    fn enter_scope(&mut self) {
        self.scopes.push(ScopeInfo::new());
    }

    fn exit_scope(&mut self) {
        self.scopes.pop();
    }

    fn current_scope(&mut self) -> &mut ScopeInfo {
        self.scopes.last_mut().unwrap()
    }

    fn update_path(&mut self, path: &mut Box<Path>) {
        if let Some(hir_id) = path.res.hir_id() {
            // Only update paths for used variables
            if !self.used_vars.contains(&hir_id) {
                return;
            }

            // Try to find the variable in the current scope first
            let mut found = false;
            for (depth, scope) in self.scopes.iter().rev().enumerate() {
                if let Some(&slot) = scope.local_slots.get(&hir_id) {
                    if depth == 0 {
                        path.res = Res::Local(hir_id, slot);
                    } else {
                        // Variable found in an outer scope, it's an upvar
                        path.res = Res::Upvar(depth - 1, slot);
                    }
                    found = true;
                    break;
                }
                if let Some(&slot) = scope.upvar_slots.get(&hir_id) {
                    // For upvars from parent scopes, we need to add the current depth
                    path.res = Res::Upvar(depth, slot);
                    found = true;
                    break;
                }
            }

            // If not found in any scope but it's a used local variable,
            // assign it a new slot in the current scope
            if !found && matches!(path.res, Res::Local(..)) {
                let slot = self.current_scope().next_local();
                self.current_scope().local_slots.insert(hir_id, slot);
                path.res = Res::Local(hir_id, slot);
            }
        }
    }
}

impl<'hir> Visitor<'hir> for SlotUpdater {
    fn visit_expr(&mut self, expr: &'hir mut Expr) {
        if let ExprKind::Path(path) = &mut expr.kind {
            self.update_path(path);
        }

        // Use the default implementation to visit all child expressions
        visit::walk_expr(self, expr);
    }

    fn visit_block(&mut self, block: &'hir mut Block) {
        self.enter_scope();

        // First pass: collect new slot indices for variables in this scope
        for stmt in &mut block.stmts {
            if let StmtKind::Let(pat, _, _) = &mut stmt.kind {
                if let PatKind::Identifier(hir_id, _) = pat.kind {
                    // Only assign slots to used variables
                    if self.used_vars.contains(&hir_id) {
                        let slot = self.current_scope().next_local();
                        self.current_scope().local_slots.insert(hir_id, slot);
                    }
                }
            }
        }

        // Second pass: update path resolutions
        visit::walk_block(self, block);

        self.exit_scope();
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
        // First collect all used variables
        self.used_vars = UsedVarsCollector::collect(module);
        self.changed = false;

        // Then eliminate dead code
        self.visit_module(module);

        // Finally update slot indices
        if self.changed {
            SlotUpdater::new(self.used_vars.clone()).visit_module(module);
        }

        self.changed
    }
}
