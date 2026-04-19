use log::debug;
use std::collections::{HashMap, HashSet};
use tlang_ast::token::Literal;
use tlang_hir::{
    self as hir, BinaryOpKind, Expr, ExprKind, Module, Pat, PatKind, Stmt, StmtKind, TyKind,
    visit::{self, Visitor},
};
use tlang_span::HirId;

use crate::hir_opt::{HirOptContext, HirOptError, HirPass};

#[derive(Default)]
pub struct AssignmentCollector {
    reassigned_variables: HashSet<HirId>,
}

impl AssignmentCollector {
    pub fn collect(module: &mut Module) -> HashSet<HirId> {
        let mut collector = Self::default();
        collector.visit_module(module, &mut ());
        collector.reassigned_variables
    }
}

impl<'hir> Visitor<'hir> for AssignmentCollector {
    fn visit_expr(&mut self, expr: &'hir mut Expr, ctx: &mut Self::Context) {
        if let ExprKind::Binary(BinaryOpKind::Assign, lhs, _rhs) = &mut expr.kind
            && let ExprKind::Path(lhs_path) = &lhs.kind
            && let Some(resolved_hir_id) = lhs_path.res.hir_id()
        {
            self.reassigned_variables.insert(resolved_hir_id);
        }

        visit::walk_expr(self, expr, ctx);
    }
}

pub struct ConstantPropagator {
    /// Maps each binding HirId to its literal value and the declared type
    /// annotation.  The type is `Unknown` when the binding has no explicit
    /// annotation (e.g. `let x = 5`).  Carrying the declared type ensures
    /// that propagating an explicitly-typed binding (`let x: i64 = 5`) does
    /// not silently coerce the inlined literal to an unrelated target type
    /// (e.g. `isize`).
    constants: HashMap<HirId, (Literal, TyKind)>,
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

impl<'hir> Visitor<'hir> for ConstantPropagator {
    fn visit_stmt(&mut self, stmt: &'hir mut Stmt, ctx: &mut Self::Context) {
        match &mut stmt.kind {
            StmtKind::Let(
                box Pat {
                    kind: PatKind::Identifier(hir_id, _ident),
                    ..
                },
                expr,
                ty,
            ) => {
                self.visit_expr(expr, ctx);

                if !self.reassigned_variables.contains(hir_id)
                    && let ExprKind::Literal(lit) = &expr.kind
                {
                    self.constants
                        .insert(*hir_id, (*lit.clone(), ty.kind.clone()));
                }
            }
            // Const declarations are always propagatable since they can never be reassigned.
            StmtKind::Const(
                _,
                box Pat {
                    kind: PatKind::Identifier(hir_id, _ident),
                    ..
                },
                expr,
                ty,
            ) => {
                self.visit_expr(expr, ctx);

                if let ExprKind::Literal(lit) = &expr.kind {
                    self.constants
                        .insert(*hir_id, (*lit.clone(), ty.kind.clone()));
                }
            }
            // Collect const items from struct/enum/protocol declarations so that
            // qualified references (e.g. `Config::DEFAULT_TIMEOUT`) get inlined.
            StmtKind::StructDeclaration(decl) => {
                for const_item in &mut decl.consts {
                    self.visit_expr(&mut const_item.value, ctx);
                    if let ExprKind::Literal(lit) = &const_item.value.kind {
                        self.constants.insert(
                            const_item.hir_id,
                            (*lit.clone(), const_item.ty.kind.clone()),
                        );
                    }
                }
            }
            StmtKind::EnumDeclaration(decl) => {
                for const_item in &mut decl.consts {
                    self.visit_expr(&mut const_item.value, ctx);
                    if let ExprKind::Literal(lit) = &const_item.value.kind {
                        self.constants.insert(
                            const_item.hir_id,
                            (*lit.clone(), const_item.ty.kind.clone()),
                        );
                    }
                }
                visit::walk_stmt(self, stmt, ctx);
            }
            StmtKind::ProtocolDeclaration(decl) => {
                for const_item in &mut decl.consts {
                    self.visit_expr(&mut const_item.value, ctx);
                    if let ExprKind::Literal(lit) = &const_item.value.kind {
                        self.constants.insert(
                            const_item.hir_id,
                            (*lit.clone(), const_item.ty.kind.clone()),
                        );
                    }
                }
                visit::walk_stmt(self, stmt, ctx);
            }
            _ => visit::walk_stmt(self, stmt, ctx),
        }
    }

    fn visit_expr(&mut self, expr: &'hir mut Expr, ctx: &mut Self::Context) {
        if let ExprKind::Path(path) = &expr.kind {
            if let Some(resolved_hir_id) = path.res.hir_id()
                && let Some((lit, declared_ty)) = self.constants.get(&resolved_hir_id)
            {
                debug!("Constant propagating: {resolved_hir_id:?} -> {lit:?}");
                expr.kind = ExprKind::Literal(Box::new(*lit));
                // Preserve the declared type of the binding so that
                // downstream type-checking can enforce it.  When the binding
                // had no annotation the declared type is `Unknown` and the
                // type checker will apply contextual inference as usual.
                expr.ty.kind = declared_ty.clone();
                self.changed = true;
                return;
            }
            return;
        }

        visit::walk_expr(self, expr, ctx);
    }
}

impl HirPass for ConstantPropagator {
    fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        _ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        self.constants.clear();
        self.reassigned_variables = AssignmentCollector::collect(module);

        self.changed = false;
        self.visit_module(module, &mut ());
        Ok(self.changed)
    }
}
