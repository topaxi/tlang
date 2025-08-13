use crate::hir;

pub trait Visitor<'hir>: Sized {
    fn visit_module(&mut self, module: &'hir mut hir::Module) {
        walk_module(self, module);
    }

    fn visit_block(&mut self, block: &'hir mut hir::Block) {
        walk_block(self, block);
    }

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt) {
        walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr) {
        walk_expr(self, expr);
    }

    fn visit_pat(&mut self, pat: &'hir mut hir::Pat) {
        walk_pat(self, pat);
    }

    fn visit_ident(&mut self, _ident: &'hir mut tlang_ast::node::Ident) {}
    fn visit_path(&mut self, _path: &'hir mut hir::Path) {}
    fn visit_ty(&mut self, _ty: &'hir mut hir::Ty) {}
    fn visit_literal(&mut self, _literal: &'hir mut tlang_ast::token::Literal) {}
}

pub fn walk_module<'hir, V: Visitor<'hir>>(visitor: &mut V, module: &'hir mut hir::Module) {
    walk_block(visitor, &mut module.block);
}

pub fn walk_block<'hir, V: Visitor<'hir>>(visitor: &mut V, block: &'hir mut hir::Block) {
    for statement in &mut block.stmts {
        visitor.visit_stmt(statement);
    }

    if let Some(expr) = &mut block.expr {
        visitor.visit_expr(expr);
    }
}

pub fn walk_stmt<'hir, V: Visitor<'hir>>(visitor: &mut V, stmt: &'hir mut hir::Stmt) {
    match &mut stmt.kind {
        hir::StmtKind::Let(pat, expr, ty) => {
            visitor.visit_expr(expr);
            visitor.visit_pat(pat);
            visitor.visit_ty(ty);
        }
        hir::StmtKind::Expr(expr) => visitor.visit_expr(expr),
        hir::StmtKind::Return(expr) => {
            if let Some(expr) = expr.as_mut() {
                visitor.visit_expr(expr);
            }
        }
        hir::StmtKind::FunctionDeclaration(decl) => {
            visitor.visit_expr(&mut decl.name);

            for param in &mut decl.parameters {
                visitor.visit_ident(&mut param.name);
                visitor.visit_ty(&mut param.type_annotation);
            }

            visitor.visit_block(&mut decl.body);
        }
        hir::StmtKind::StructDeclaration(decl) => {
            for field in &mut decl.fields {
                visitor.visit_ident(&mut field.name);
                visitor.visit_ty(&mut field.ty);
            }
        }
        hir::StmtKind::EnumDeclaration(decl) => {
            for variant in &mut decl.variants {
                visitor.visit_ident(&mut variant.name);

                for field in &mut variant.parameters {
                    visitor.visit_ident(&mut field.name);
                    visitor.visit_ty(&mut field.ty);
                }
            }
        }
        hir::StmtKind::DynFunctionDeclaration(_decl) => {}
    }
}

pub fn walk_expr<'hir, V: Visitor<'hir>>(visitor: &mut V, expr: &'hir mut hir::Expr) {
    match &mut expr.kind {
        hir::ExprKind::Path(path) => visitor.visit_path(path),
        hir::ExprKind::Unary(_, expr) => visitor.visit_expr(expr),
        hir::ExprKind::Let(pat, expr) => {
            visitor.visit_expr(expr);
            visitor.visit_pat(pat);
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            visitor.visit_expr(lhs);
            visitor.visit_expr(rhs);
        }
        hir::ExprKind::IfElse(condition, consequence, else_branches) => {
            visitor.visit_expr(condition);
            visitor.visit_block(consequence);

            for hir::ElseClause {
                condition,
                consequence,
            } in else_branches
            {
                if let Some(expr) = condition {
                    visitor.visit_expr(expr);
                }

                visitor.visit_block(consequence);
            }
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => visitor.visit_block(block),
        hir::ExprKind::Break(Some(expr)) => {
            visitor.visit_expr(expr);
        }
        hir::ExprKind::Break(_) => {}
        hir::ExprKind::FunctionExpression(decl) => {
            visitor.visit_expr(&mut decl.name);

            for param in &mut decl.parameters {
                visitor.visit_ident(&mut param.name);
                visitor.visit_ty(&mut param.type_annotation);
            }

            visitor.visit_block(&mut decl.body);
        }
        hir::ExprKind::Call(call_expr) | hir::ExprKind::TailCall(call_expr) => {
            visitor.visit_expr(&mut call_expr.callee);

            for arg in &mut call_expr.arguments {
                visitor.visit_expr(arg);
            }
        }
        hir::ExprKind::FieldAccess(base, ident) => {
            visitor.visit_expr(base);
            visitor.visit_ident(ident);
        }
        hir::ExprKind::IndexAccess(base, expr) => {
            visitor.visit_expr(expr);
            visitor.visit_expr(base);
        }
        hir::ExprKind::Match(expr, arms) => {
            visitor.visit_expr(expr);

            for arm in arms {
                visitor.visit_pat(&mut arm.pat);

                if let Some(guard) = &mut arm.guard {
                    visitor.visit_expr(guard);
                }

                visitor.visit_block(&mut arm.block);
            }
        }
        hir::ExprKind::Dict(pairs) => {
            for (key, value) in pairs {
                visitor.visit_expr(key);
                visitor.visit_expr(value);
            }
        }
        hir::ExprKind::List(exprs) => {
            for value in exprs {
                visitor.visit_expr(value);
            }
        }
        hir::ExprKind::Literal(literal) => visitor.visit_literal(literal),
        hir::ExprKind::Cast(expr, ty) => {
            visitor.visit_expr(expr);
            visitor.visit_ty(ty);
        }
        hir::ExprKind::Wildcard => {}
        hir::ExprKind::Continue => {}
        hir::ExprKind::Range(..) => todo!(),
    }
}

pub fn walk_pat<'hir, V: Visitor<'hir>>(visitor: &mut V, pat: &'hir mut hir::Pat) {
    match &mut pat.kind {
        hir::PatKind::Identifier(_, ident) => visitor.visit_ident(ident),
        hir::PatKind::List(pats) => {
            for pat in pats {
                visitor.visit_pat(pat);
            }
        }
        hir::PatKind::Rest(pat) => {
            visitor.visit_pat(pat);
        }
        hir::PatKind::Enum(path, fields) => {
            visitor.visit_path(path);

            for (ident, pat) in fields {
                visitor.visit_ident(ident);
                visitor.visit_pat(pat);
            }
        }
        hir::PatKind::Literal(literal) => visitor.visit_literal(literal),
        hir::PatKind::Wildcard => {}
    }
}
