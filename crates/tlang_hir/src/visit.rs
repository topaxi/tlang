use crate::hir;

pub trait Visitor<'hir>: Sized {
    fn visit_module(&mut self, module: &'hir hir::Module) {
        walk_module(self, module);
    }

    fn visit_block(&mut self, block: &'hir hir::Block) {
        walk_block(self, block);
    }

    fn visit_stmt(&mut self, stmt: &'hir hir::Stmt) {
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'hir hir::Expr) {
        walk_expr(self, expr);
    }

    fn visit_ident(&mut self, _ident: &'hir tlang_ast::node::Ident) {}
    fn visit_path(&mut self, _path: &'hir hir::Path) {}
    fn visit_ty(&mut self, _ty: &'hir hir::Ty) {}
    fn visit_literal(&mut self, _literal: &'hir tlang_ast::token::Literal) {}
}

pub fn walk_module<'hir, V: Visitor<'hir>>(visitor: &mut V, module: &'hir hir::Module) {
    walk_block(visitor, &module.block);
}

pub fn walk_block<'hir, V: Visitor<'hir>>(visitor: &mut V, block: &'hir hir::Block) {
    for statement in &block.stmts {
        visitor.visit_stmt(statement);
    }

    if let Some(expr) = &block.expr {
        visitor.visit_expr(expr);
    }
}

pub fn walk_stmt<'hir, V: Visitor<'hir>>(visitor: &mut V, stmt: &'hir hir::Stmt) {
    match &stmt.kind {
        hir::StmtKind::Expr(expr) => visitor.visit_expr(expr),
        hir::StmtKind::FunctionDeclaration(decl) => {
            visitor.visit_expr(&decl.name);

            for param in &decl.parameters {
                visitor.visit_ident(&param.name);
                visitor.visit_ty(&param.type_annotation);
            }

            visitor.visit_block(&decl.body);
        }
        hir::StmtKind::StructDeclaration(decl) => {
            for field in &decl.fields {
                visitor.visit_ident(&field.name);
                visitor.visit_ty(&field.ty);
            }
        }
        hir::StmtKind::EnumDeclaration(decl) => {
            for variant in &decl.variants {
                visitor.visit_ident(&variant.name);

                for field in &variant.parameters {
                    visitor.visit_ident(&field.name);
                    visitor.visit_ty(&field.ty);
                }
            }
        }
        hir::StmtKind::None => {}
        _ => todo!("{:?}", stmt),
    }
}

pub fn walk_expr<'hir, V: Visitor<'hir>>(visitor: &mut V, expr: &'hir hir::Expr) {
    match &expr.kind {
        hir::ExprKind::Path(path) => visitor.visit_path(path),
        hir::ExprKind::Binary(_, lhs, rhs) => {
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs);
        }
        hir::ExprKind::Call(call_expr) | hir::ExprKind::TailCall(call_expr) => {
            visitor.visit_expr(&call_expr.callee);

            for arg in &call_expr.arguments {
                visitor.visit_expr(arg);
            }
        }
        hir::ExprKind::FieldAccess(base, ident) => {
            visitor.visit_expr(base);
            visitor.visit_ident(ident);
        }
        hir::ExprKind::Literal(literal) => visitor.visit_literal(literal),
        hir::ExprKind::Wildcard => {}
        _ => todo!("{:?}", expr),
    }
}
