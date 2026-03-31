//! Mutable AST visitor trait and walk functions.
//!
//! Unlike the immutable [`Visitor`](crate::Visitor), this visitor receives
//! mutable references to AST nodes, allowing passes to annotate or transform
//! the AST in place.
use crate::node;

pub trait VisitorMut: Sized {
    fn visit_module(&mut self, module: &mut node::Module) {
        walk_module(self, module);
    }

    fn visit_stmt(&mut self, stmt: &mut node::Stmt) {
        walk_stmt(self, stmt);
    }

    fn visit_fn_decls(&mut self, decls: &mut [node::FunctionDeclaration]) {
        walk_fn_decls(self, decls);
    }

    fn visit_fn_decl(&mut self, decl: &mut node::FunctionDeclaration) {
        walk_fn_decl(self, decl);
    }

    fn visit_fn_param(&mut self, param: &mut node::FunctionParameter) {
        walk_fn_param(self, param);
    }

    fn visit_expr(&mut self, expr: &mut node::Expr) {
        walk_expr(self, expr);
    }

    fn visit_impl_block(&mut self, impl_block: &mut node::ImplBlock) {
        walk_impl_block(self, impl_block);
    }
}

pub fn walk_module(visitor: &mut impl VisitorMut, module: &mut node::Module) {
    for stmt in &mut module.statements {
        visitor.visit_stmt(stmt);
    }
}

pub fn walk_stmt(visitor: &mut impl VisitorMut, stmt: &mut node::Stmt) {
    match &mut stmt.kind {
        node::StmtKind::FunctionDeclaration(decl) => visitor.visit_fn_decl(decl),
        node::StmtKind::FunctionDeclarations(decls) => visitor.visit_fn_decls(decls),
        node::StmtKind::Expr(expr) => visitor.visit_expr(expr),
        node::StmtKind::Return(Some(expr)) => visitor.visit_expr(expr),
        node::StmtKind::Let(let_decl) => visitor.visit_expr(&mut let_decl.expression),
        node::StmtKind::Const(const_decl) => visitor.visit_expr(&mut const_decl.expression),
        node::StmtKind::ImplBlock(impl_block) => {
            visitor.visit_impl_block(impl_block);
        }
        _ => {}
    }
}

pub fn walk_impl_block(visitor: &mut impl VisitorMut, impl_block: &mut node::ImplBlock) {
    for decl in &mut impl_block.methods {
        visitor.visit_fn_decl(decl);
    }
}

pub fn walk_fn_decls(visitor: &mut impl VisitorMut, decls: &mut [node::FunctionDeclaration]) {
    for decl in decls.iter_mut() {
        visitor.visit_fn_decl(decl);
    }
}

pub fn walk_fn_decl(visitor: &mut impl VisitorMut, decl: &mut node::FunctionDeclaration) {
    for param in &mut decl.parameters {
        visitor.visit_fn_param(param);
    }

    if let Some(ref mut guard) = decl.guard {
        visitor.visit_expr(guard);
    }

    for stmt in &mut decl.body.statements {
        visitor.visit_stmt(stmt);
    }

    if let Some(ref mut expr) = decl.body.expression {
        visitor.visit_expr(expr);
    }
}

pub fn walk_fn_param(_visitor: &mut impl VisitorMut, _param: &mut node::FunctionParameter) {}

pub fn walk_expr(visitor: &mut impl VisitorMut, expr: &mut node::Expr) {
    match &mut expr.kind {
        node::ExprKind::FunctionExpression(decl) => visitor.visit_fn_decl(decl),
        node::ExprKind::Block(block) | node::ExprKind::Loop(block) => {
            for stmt in &mut block.statements {
                visitor.visit_stmt(stmt);
            }
            if let Some(ref mut expr) = block.expression {
                visitor.visit_expr(expr);
            }
        }
        _ => {}
    }
}
