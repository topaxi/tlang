use std::collections::HashMap;
use log::debug;
use tlang_hir::hir::{self, Block, Expr, ExprKind, HirId, Module, Path, Res, Stmt, StmtKind, Pat, PatKind};
use tlang_hir::visit::{self, Visitor};

use crate::HirPass;

#[derive(Debug, Clone)]
struct Binding {
    name: String,
    res: Res,
}

impl Binding {
    fn new_local(name: String, hir_id: HirId, index: usize) -> Self {
        Self {
            name,
            res: Res::Local(hir_id, index),
        }
    }

    fn new_upvar(name: String, scope: usize, index: usize) -> Self {
        Self {
            name,
            res: Res::Upvar(scope, index),
        }
    }

    fn name(&self) -> &str {
        &self.name
    }

    fn res(&self) -> Res {
        self.res
    }
}

#[derive(Default)]
struct Scope {
    locals: HashMap<String, Binding>,
    upvars: HashMap<String, Binding>,
    next_local_slot: usize,
}

impl Scope {
    fn new() -> Self {
        Self::default()
    }

    fn def_local(&mut self, name: &str, hir_id: HirId) -> Binding {
        let binding = Binding::new_local(name.to_string(), hir_id, self.next_local_slot);
        self.next_local_slot += 1;
        self.locals.insert(name.to_string(), binding.clone());
        binding
    }

    fn def_upvar(&mut self, name: &str, scope: usize, index: usize) -> Binding {
        let binding = Binding::new_upvar(name.to_string(), scope, index);
        self.upvars.insert(name.to_string(), binding.clone());
        binding
    }

    fn lookup(&self, name: &str) -> Option<&Binding> {
        self.locals.get(name).or_else(|| self.upvars.get(name))
    }
}

pub struct PathResolver {
    scopes: Vec<Scope>,
    changed: bool,
}

impl PathResolver {
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::new()],
            changed: false,
        }
    }

    fn scope(&mut self) -> &mut Scope {
        self.scopes.last_mut().unwrap()
    }

    fn lookup(&mut self, name: &str) -> Option<Binding> {
        let (scope_index, binding) = self
            .scopes
            .iter()
            .enumerate()
            .rev()
            .find_map(|(i, scope)| scope.lookup(name).map(|binding| (i, binding.clone())))?;

        if scope_index < self.scopes.len() - 1 {
            let relative_scope_index = self.scopes.len() - 1 - scope_index;
            let slot_index = binding.res().slot_index().unwrap();

            return Some(self.scopes.last_mut().unwrap().def_upvar(
                binding.name(),
                relative_scope_index,
                slot_index,
            ));
        }

        Some(binding)
    }

    fn with_new_scope<F, R>(&mut self, f: F) -> R
    where
        F: FnOnce(&mut Self) -> R,
        R: hir::HirScope,
    {
        debug!("Entering new scope");
        self.scopes.push(Scope::new());
        let mut result = f(self);
        result.set_locals(self.scope().next_local_slot);
        result.set_upvars(self.scope().upvars.len());
        debug!("Leaving scope");
        self.scopes.pop();
        result
    }

    fn update_path(&mut self, path: &mut Path) {
        if let Some(binding) = self.lookup(&path.join("::")) {
            let old_res = path.res;
            path.res = binding.res();
            if old_res != path.res {
                self.changed = true;
            }
        }
    }
}

impl<'hir> Visitor<'hir> for PathResolver {
    fn visit_module(&mut self, module: &'hir mut Module) {
        // Visit the module block directly
        self.visit_block(&mut module.block);
    }

    fn visit_block(&mut self, block: &'hir mut Block) {
        // First collect let bindings in the current scope
        for stmt in &mut block.stmts {
            if let StmtKind::Let(pat, _, _) = &mut stmt.kind {
                if let PatKind::Identifier(hir_id, ref ident) = pat.kind {
                    self.scope().def_local(ident.as_str(), hir_id);
                }
            }
        }

        // Then visit all statements in a new scope
        self.with_new_scope(|this| {
            // Visit all statements
            for stmt in &mut block.stmts {
                this.visit_stmt(stmt);
            }
            if let Some(expr) = &mut block.expr {
                this.visit_expr(expr);
            }

            block.clone()
        });
    }

    fn visit_expr(&mut self, expr: &'hir mut Expr) {
        match &mut expr.kind {
            ExprKind::Path(path) => {
                self.update_path(path);
            }
            _ => visit::walk_expr(self, expr),
        }
    }
}

impl HirPass for PathResolver {
    fn optimize_module(&mut self, module: &mut Module) -> bool {
        self.changed = false;
        self.visit_module(module);
        self.changed
    }
} 