use tlang_span::HirId;

use crate::hir;

pub trait Visitor<'hir>: Sized {
    // The default type here requires #![feature(associated_type_defaults)]
    type Context = ();

    #[allow(unused_variables)]
    fn enter_scope(&mut self, hir_id: HirId, ctx: &mut Self::Context) {}
    #[allow(unused_variables)]
    fn leave_scope(&mut self, hir_id: HirId, ctx: &mut Self::Context) {}

    fn visit_module(&mut self, module: &'hir mut hir::Module, ctx: &mut Self::Context) {
        walk_module(self, module, ctx);
    }

    fn visit_block(&mut self, block: &'hir mut hir::Block, ctx: &mut Self::Context) {
        walk_block(self, block, ctx);
    }

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, ctx: &mut Self::Context) {
        walk_stmt(self, stmt, ctx);
    }

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut Self::Context) {
        walk_expr(self, expr, ctx);
    }

    fn visit_pat(&mut self, pat: &'hir mut hir::Pat, ctx: &mut Self::Context) {
        walk_pat(self, pat, ctx);
    }

    #[allow(unused_variables)]
    fn visit_ident(&mut self, ident: &'hir mut tlang_ast::node::Ident, ctx: &mut Self::Context) {}

    #[allow(unused_variables)]
    fn visit_path(&mut self, path: &'hir mut hir::Path, ctx: &mut Self::Context) {}

    #[allow(unused_variables)]
    fn visit_ty(&mut self, ty: &'hir mut hir::Ty, ctx: &mut Self::Context) {}

    #[allow(unused_variables)]
    fn visit_literal(
        &mut self,
        literal: &'hir mut tlang_ast::token::Literal,
        ctx: &mut Self::Context,
    ) {
    }
}

pub fn walk_module<'hir, V: Visitor<'hir>>(
    visitor: &mut V,
    module: &'hir mut hir::Module,
    ctx: &mut V::Context,
) {
    visitor.enter_scope(module.hir_id, ctx);
    visitor.visit_block(&mut module.block, ctx);
    visitor.leave_scope(module.hir_id, ctx);
}

pub fn walk_block<'hir, V: Visitor<'hir>>(
    visitor: &mut V,
    block: &'hir mut hir::Block,
    ctx: &mut V::Context,
) {
    visitor.enter_scope(block.hir_id, ctx);

    for statement in &mut block.stmts {
        visitor.visit_stmt(statement, ctx);
    }

    if let Some(expr) = &mut block.expr {
        visitor.visit_expr(expr, ctx);
    }

    visitor.leave_scope(block.hir_id, ctx);
}

pub fn walk_stmt<'hir, V: Visitor<'hir>>(
    visitor: &mut V,
    stmt: &'hir mut hir::Stmt,
    ctx: &mut V::Context,
) {
    match &mut stmt.kind {
        hir::StmtKind::Let(pat, expr, ty) => {
            visitor.visit_expr(expr, ctx);
            visitor.visit_pat(pat, ctx);
            visitor.visit_ty(ty, ctx);
        }
        hir::StmtKind::Const(_, pat, expr, ty) => {
            visitor.visit_expr(expr, ctx);
            visitor.visit_pat(pat, ctx);
            visitor.visit_ty(ty, ctx);
        }
        hir::StmtKind::Expr(expr) => visitor.visit_expr(expr, ctx),
        hir::StmtKind::Return(expr) => {
            if let Some(expr) = expr.as_mut() {
                visitor.visit_expr(expr, ctx);
            }
        }
        hir::StmtKind::FunctionDeclaration(decl) => {
            visitor.visit_expr(&mut decl.name, ctx);
            visitor.enter_scope(decl.hir_id, ctx);

            for param in &mut decl.parameters {
                visitor.visit_ident(&mut param.name, ctx);
                visitor.visit_ty(&mut param.type_annotation, ctx);
            }

            visitor.visit_block(&mut decl.body, ctx);
            visitor.leave_scope(decl.hir_id, ctx);
        }
        hir::StmtKind::StructDeclaration(decl) => {
            for field in &mut decl.fields {
                visitor.visit_ident(&mut field.name, ctx);
                visitor.visit_ty(&mut field.ty, ctx);
            }
            for const_item in &mut decl.consts {
                visitor.visit_ty(&mut const_item.ty, ctx);
                visitor.visit_expr(&mut const_item.value, ctx);
            }
        }
        hir::StmtKind::EnumDeclaration(decl) => {
            for variant in &mut decl.variants {
                visitor.visit_ident(&mut variant.name, ctx);

                for field in &mut variant.parameters {
                    visitor.visit_ident(&mut field.name, ctx);
                    visitor.visit_ty(&mut field.ty, ctx);
                }
            }
            for const_item in &mut decl.consts {
                visitor.visit_ty(&mut const_item.ty, ctx);
                visitor.visit_expr(&mut const_item.value, ctx);
            }
        }
        hir::StmtKind::DynFunctionDeclaration(decl) => {
            visitor.visit_expr(&mut decl.name, ctx);
        }
        hir::StmtKind::ProtocolDeclaration(decl) => {
            visitor.visit_ident(&mut decl.name, ctx);
            for constraint in &mut decl.constraints {
                visitor.visit_path(constraint, ctx);
            }
            for method in &mut decl.methods {
                visitor.visit_ident(&mut method.name, ctx);
                // Visit default method bodies with their own scope so that
                // `SymbolResolution` (and other HIR passes) can resolve
                // identifiers like `self` and other parameters inside them.
                if let Some(body) = &mut method.body {
                    visitor.enter_scope(method.hir_id, ctx);
                    for param in &mut method.parameters {
                        visitor.visit_ident(&mut param.name, ctx);
                        visitor.visit_ty(&mut param.type_annotation, ctx);
                    }
                    visitor.visit_block(body, ctx);
                    visitor.leave_scope(method.hir_id, ctx);
                }
            }
            for const_item in &mut decl.consts {
                visitor.visit_ty(&mut const_item.ty, ctx);
                visitor.visit_expr(&mut const_item.value, ctx);
            }
        }
        hir::StmtKind::ImplBlock(impl_block) => {
            visitor.visit_path(&mut impl_block.protocol_name, ctx);
            visitor.visit_path(&mut impl_block.target_type, ctx);
            if let Some(wc) = &mut impl_block.where_clause {
                for pred in &mut wc.predicates {
                    for bound in &mut pred.bounds {
                        visitor.visit_ty(bound, ctx);
                    }
                }
            }
            for assoc_ty in &mut impl_block.associated_types {
                visitor.visit_ty(&mut assoc_ty.ty, ctx);
            }
            for decl in &mut impl_block.methods {
                // Skip visiting method name — it's in declaration position and
                // not resolvable as a standalone path (only Protocol::method is).
                visitor.enter_scope(decl.hir_id, ctx);
                for param in &mut decl.parameters {
                    visitor.visit_ident(&mut param.name, ctx);
                    visitor.visit_ty(&mut param.type_annotation, ctx);
                }
                visitor.visit_block(&mut decl.body, ctx);
                visitor.leave_scope(decl.hir_id, ctx);
            }
        }
    }
}

pub fn walk_expr<'hir, V: Visitor<'hir>>(
    visitor: &mut V,
    expr: &'hir mut hir::Expr,
    ctx: &mut V::Context,
) {
    match &mut expr.kind {
        hir::ExprKind::Path(path) => visitor.visit_path(path, ctx),
        hir::ExprKind::Unary(_, expr) => visitor.visit_expr(expr, ctx),
        hir::ExprKind::Let(pat, expr) => {
            visitor.visit_expr(expr, ctx);
            visitor.visit_pat(pat, ctx);
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            visitor.visit_expr(lhs, ctx);
            visitor.visit_expr(rhs, ctx);
        }
        hir::ExprKind::IfElse(condition, consequence, else_branches) => {
            visitor.visit_expr(condition, ctx);
            visitor.visit_block(consequence, ctx);

            for hir::ElseClause {
                condition,
                consequence,
            } in else_branches
            {
                if let Some(expr) = condition {
                    visitor.visit_expr(expr, ctx);
                }

                visitor.visit_block(consequence, ctx);
            }
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            visitor.visit_block(block, ctx);
        }
        hir::ExprKind::Break(Some(expr)) => {
            visitor.visit_expr(expr, ctx);
        }
        hir::ExprKind::Break(_) => {}
        hir::ExprKind::FunctionExpression(decl) => {
            visitor.enter_scope(decl.hir_id, ctx);
            visitor.visit_expr(&mut decl.name, ctx);

            for param in &mut decl.parameters {
                visitor.visit_ident(&mut param.name, ctx);
                visitor.visit_ty(&mut param.type_annotation, ctx);
            }

            visitor.visit_block(&mut decl.body, ctx);
            visitor.leave_scope(decl.hir_id, ctx);
        }
        hir::ExprKind::Call(call_expr) | hir::ExprKind::TailCall(call_expr) => {
            for arg in &mut call_expr.arguments {
                visitor.visit_expr(arg, ctx);
            }

            visitor.visit_expr(&mut call_expr.callee, ctx);
        }
        hir::ExprKind::FieldAccess(base, ident) => {
            visitor.visit_expr(base, ctx);
            visitor.visit_ident(ident, ctx);
        }
        hir::ExprKind::IndexAccess(base, expr) => {
            visitor.visit_expr(expr, ctx);
            visitor.visit_expr(base, ctx);
        }
        hir::ExprKind::Match(expr, arms) => {
            visitor.visit_expr(expr, ctx);
            for arm in arms {
                walk_match_arm(visitor, arm, ctx);
            }
        }
        hir::ExprKind::Implements(expr, path) => {
            visitor.visit_expr(expr, ctx);
            visitor.visit_path(path, ctx);
        }
        hir::ExprKind::Dict(pairs) => {
            for (key, value) in pairs {
                visitor.visit_expr(key, ctx);
                visitor.visit_expr(value, ctx);
            }
        }
        hir::ExprKind::List(exprs) => {
            for value in exprs {
                visitor.visit_expr(value, ctx);
            }
        }
        hir::ExprKind::Literal(literal) => visitor.visit_literal(literal, ctx),
        hir::ExprKind::Cast(expr, ty) => {
            visitor.visit_expr(expr, ctx);
            visitor.visit_ty(ty, ctx);
        }
        hir::ExprKind::TryCast(expr, ty) => {
            visitor.visit_expr(expr, ctx);
            visitor.visit_ty(ty, ctx);
        }
        hir::ExprKind::TaggedString { tag, exprs, .. } => {
            visitor.visit_expr(tag, ctx);
            for expr in exprs {
                visitor.visit_expr(expr, ctx);
            }
        }
        hir::ExprKind::Wildcard => {}
        hir::ExprKind::Continue => {}
        hir::ExprKind::Range(..) => todo!(),
    }
}

fn walk_match_arm<'hir, V: Visitor<'hir>>(
    visitor: &mut V,
    arm: &'hir mut hir::MatchArm,
    ctx: &mut V::Context,
) {
    let block_hir_id = arm.block.hir_id;
    let has_pat_scope = arm.hir_id != block_hir_id;
    if has_pat_scope {
        visitor.enter_scope(arm.hir_id, ctx);
    }
    visitor.enter_scope(block_hir_id, ctx);
    visitor.visit_pat(&mut arm.pat, ctx);
    if let Some(guard) = &mut arm.guard {
        visitor.visit_expr(guard, ctx);
    }
    visitor.visit_block(&mut arm.block, ctx);
    visitor.leave_scope(block_hir_id, ctx);
    if has_pat_scope {
        visitor.leave_scope(arm.hir_id, ctx);
    }
}

pub fn walk_pat<'hir, V: Visitor<'hir>>(
    visitor: &mut V,
    pat: &'hir mut hir::Pat,
    ctx: &mut V::Context,
) {
    match &mut pat.kind {
        hir::PatKind::Identifier(_, ident) => visitor.visit_ident(ident, ctx),
        hir::PatKind::List(pats) => {
            for pat in pats {
                visitor.visit_pat(pat, ctx);
            }
        }
        hir::PatKind::Rest(pat) => {
            visitor.visit_pat(pat, ctx);
        }
        hir::PatKind::Enum(path, fields) => {
            visitor.visit_path(path, ctx);

            for (ident, pat) in fields {
                visitor.visit_ident(ident, ctx);
                visitor.visit_pat(pat, ctx);
            }
        }
        hir::PatKind::Literal(literal) => visitor.visit_literal(literal, ctx),
        hir::PatKind::Wildcard => {}
    }
}
