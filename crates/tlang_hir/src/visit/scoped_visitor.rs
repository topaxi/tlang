use crate::hir;

pub trait ScopedVisitor<'hir>: Sized {
    type ScopeHandle: Copy;

    fn push_scope(&mut self, hir_id: hir::HirId) -> Option<Self::ScopeHandle>;
    fn pop_scope(&mut self, scope_handle: Self::ScopeHandle);

    fn visit_module(&mut self, module: &'hir mut hir::Module, parent_scope: Self::ScopeHandle) {
        walk_module(self, module, parent_scope);
    }

    fn visit_block(&mut self, block: &'hir mut hir::Block, parent_scope: Self::ScopeHandle) {
        walk_block(self, block, parent_scope);
    }

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, parent_scope: Self::ScopeHandle) {
        walk_stmt(self, stmt, parent_scope);
    }

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, parent_scope: Self::ScopeHandle) {
        walk_expr(self, expr, parent_scope);
    }

    fn visit_pat(&mut self, pat: &'hir mut hir::Pat, parent_scope: Self::ScopeHandle) {
        walk_pat(self, pat, parent_scope);
    }

    fn visit_ident(
        &mut self,
        _ident: &'hir mut tlang_ast::node::Ident,
        _parent_scope: Self::ScopeHandle,
    ) {
    }

    fn visit_path(&mut self, _path: &'hir mut hir::Path, _parent_scope: Self::ScopeHandle) {}
    fn visit_ty(&mut self, _ty: &'hir mut hir::Ty, _parent_scope: Self::ScopeHandle) {}

    fn visit_literal(
        &mut self,
        _literal: &'hir mut tlang_ast::token::Literal,
        _parent_scope: Self::ScopeHandle,
    ) {
    }
}

/// # Panics
pub fn walk_module<'hir, V: ScopedVisitor<'hir>>(
    visitor: &mut V,
    module: &'hir mut hir::Module,
    _parent_scope: V::ScopeHandle,
) {
    let module_scope = visitor
        .push_scope(module.hir_id)
        .expect("Module should have a symbol table");
    visitor.visit_block(&mut module.block, module_scope);
    visitor.pop_scope(module_scope);
}

pub fn walk_block<'hir, V: ScopedVisitor<'hir>>(
    visitor: &mut V,
    block: &'hir mut hir::Block,
    parent_scope: V::ScopeHandle,
) {
    // TODO: We made a mess where modules have blocks without scopes, and potentially in other
    //       places like loops and function bodies etc.
    let block_scope = visitor
        .push_scope(block.hir_id)
        .unwrap_or_else(|| parent_scope);

    for statement in &mut block.stmts {
        visitor.visit_stmt(statement, block_scope);
    }

    if let Some(expr) = &mut block.expr {
        visitor.visit_expr(expr, block_scope);
    }

    visitor.pop_scope(block_scope);
}

pub fn walk_stmt<'hir, V: ScopedVisitor<'hir>>(
    visitor: &mut V,
    stmt: &'hir mut hir::Stmt,
    parent_scope: V::ScopeHandle,
) {
    match &mut stmt.kind {
        hir::StmtKind::Let(pat, expr, ty) => {
            visitor.visit_expr(expr, parent_scope);
            visitor.visit_pat(pat, parent_scope);
            visitor.visit_ty(ty, parent_scope);
        }
        hir::StmtKind::Expr(expr) => visitor.visit_expr(expr, parent_scope),
        hir::StmtKind::Return(expr) => {
            if let Some(expr) = expr.as_mut() {
                visitor.visit_expr(expr, parent_scope);
            }
        }
        hir::StmtKind::FunctionDeclaration(decl) => {
            visitor.visit_expr(&mut decl.name, parent_scope);

            for param in &mut decl.parameters {
                visitor.visit_ident(&mut param.name, parent_scope);
                visitor.visit_ty(&mut param.type_annotation, parent_scope);
            }

            visitor.visit_block(&mut decl.body, parent_scope);
        }
        hir::StmtKind::StructDeclaration(decl) => {
            for field in &mut decl.fields {
                visitor.visit_ident(&mut field.name, parent_scope);
                visitor.visit_ty(&mut field.ty, parent_scope);
            }
        }
        hir::StmtKind::EnumDeclaration(decl) => {
            for variant in &mut decl.variants {
                visitor.visit_ident(&mut variant.name, parent_scope);

                for field in &mut variant.parameters {
                    visitor.visit_ident(&mut field.name, parent_scope);
                    visitor.visit_ty(&mut field.ty, parent_scope);
                }
            }
        }
        hir::StmtKind::DynFunctionDeclaration(_decl) => {}
    }
}

pub fn walk_expr<'hir, V: ScopedVisitor<'hir>>(
    visitor: &mut V,
    expr: &'hir mut hir::Expr,
    parent_scope: V::ScopeHandle,
) {
    match &mut expr.kind {
        hir::ExprKind::Path(path) => visitor.visit_path(path, parent_scope),
        hir::ExprKind::Unary(_, expr) => visitor.visit_expr(expr, parent_scope),
        hir::ExprKind::Let(pat, expr) => {
            visitor.visit_expr(expr, parent_scope);
            visitor.visit_pat(pat, parent_scope);
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            visitor.visit_expr(lhs, parent_scope);
            visitor.visit_expr(rhs, parent_scope);
        }
        hir::ExprKind::IfElse(condition, consequence, else_branches) => {
            visitor.visit_expr(condition, parent_scope);
            visitor.visit_block(consequence, parent_scope);

            for hir::ElseClause {
                condition,
                consequence,
            } in else_branches
            {
                if let Some(expr) = condition {
                    visitor.visit_expr(expr, parent_scope);
                }

                visitor.visit_block(consequence, parent_scope);
            }
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            visitor.visit_block(block, parent_scope);
        }
        hir::ExprKind::Break(Some(expr)) => {
            visitor.visit_expr(expr, parent_scope);
        }
        hir::ExprKind::Break(_) => {}
        hir::ExprKind::FunctionExpression(decl) => {
            visitor.visit_expr(&mut decl.name, parent_scope);

            for param in &mut decl.parameters {
                visitor.visit_ident(&mut param.name, parent_scope);
                visitor.visit_ty(&mut param.type_annotation, parent_scope);
            }

            visitor.visit_block(&mut decl.body, parent_scope);
        }
        hir::ExprKind::Call(call_expr) | hir::ExprKind::TailCall(call_expr) => {
            visitor.visit_expr(&mut call_expr.callee, parent_scope);

            for arg in &mut call_expr.arguments {
                visitor.visit_expr(arg, parent_scope);
            }
        }
        hir::ExprKind::FieldAccess(base, ident) => {
            visitor.visit_expr(base, parent_scope);
            visitor.visit_ident(ident, parent_scope);
        }
        hir::ExprKind::IndexAccess(base, expr) => {
            visitor.visit_expr(expr, parent_scope);
            visitor.visit_expr(base, parent_scope);
        }
        hir::ExprKind::Match(expr, arms) => {
            visitor.visit_expr(expr, parent_scope);

            for arm in arms {
                visitor.visit_pat(&mut arm.pat, parent_scope);

                if let Some(guard) = &mut arm.guard {
                    visitor.visit_expr(guard, parent_scope);
                }

                visitor.visit_block(&mut arm.block, parent_scope);
            }
        }
        hir::ExprKind::Dict(pairs) => {
            for (key, value) in pairs {
                visitor.visit_expr(key, parent_scope);
                visitor.visit_expr(value, parent_scope);
            }
        }
        hir::ExprKind::List(exprs) => {
            for value in exprs {
                visitor.visit_expr(value, parent_scope);
            }
        }
        hir::ExprKind::Literal(literal) => visitor.visit_literal(literal, parent_scope),
        hir::ExprKind::Cast(expr, ty) => {
            visitor.visit_expr(expr, parent_scope);
            visitor.visit_ty(ty, parent_scope);
        }
        hir::ExprKind::Wildcard => {}
        hir::ExprKind::Continue => {}
        hir::ExprKind::Range(..) => todo!(),
    }
}

pub fn walk_pat<'hir, V: ScopedVisitor<'hir>>(
    visitor: &mut V,
    pat: &'hir mut hir::Pat,
    parent_scope: V::ScopeHandle,
) {
    match &mut pat.kind {
        hir::PatKind::Identifier(_, ident) => visitor.visit_ident(ident, parent_scope),
        hir::PatKind::List(pats) => {
            for pat in pats {
                visitor.visit_pat(pat, parent_scope);
            }
        }
        hir::PatKind::Rest(pat) => {
            visitor.visit_pat(pat, parent_scope);
        }
        hir::PatKind::Enum(path, fields) => {
            visitor.visit_path(path, parent_scope);

            for (ident, pat) in fields {
                visitor.visit_ident(ident, parent_scope);
                visitor.visit_pat(pat, parent_scope);
            }
        }
        hir::PatKind::Literal(literal) => visitor.visit_literal(literal, parent_scope),
        hir::PatKind::Wildcard => {}
    }
}
