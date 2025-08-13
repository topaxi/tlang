use std::cell::RefCell;
use std::rc::Rc;

use tlang_ast::symbols::SymbolTable;

use crate::hir;

pub trait ScopedVisitor<'hir>: Sized {
    fn get_symbol_table(&mut self, hir_id: hir::HirId) -> Option<Rc<RefCell<SymbolTable>>>;

    fn visit_module(&mut self, module: &'hir mut hir::Module, scope: &Rc<RefCell<SymbolTable>>) {
        walk_module(self, module, scope);
    }

    fn visit_block(&mut self, block: &'hir mut hir::Block, scope: &Rc<RefCell<SymbolTable>>) {
        walk_block(self, block, scope);
    }

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, scope: &Rc<RefCell<SymbolTable>>) {
        walk_stmt(self, stmt, scope);
    }

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, scope: &Rc<RefCell<SymbolTable>>) {
        walk_expr(self, expr, scope);
    }

    fn visit_pat(&mut self, pat: &'hir mut hir::Pat, scope: &Rc<RefCell<SymbolTable>>) {
        walk_pat(self, pat, scope);
    }

    fn visit_ident(
        &mut self,
        _ident: &'hir mut tlang_ast::node::Ident,
        _scope: &Rc<RefCell<SymbolTable>>,
    ) {
    }
    fn visit_path(&mut self, _path: &'hir mut hir::Path, _scope: &Rc<RefCell<SymbolTable>>) {}
    fn visit_ty(&mut self, _ty: &'hir mut hir::Ty, _scope: &Rc<RefCell<SymbolTable>>) {}
    fn visit_literal(
        &mut self,
        _literal: &'hir mut tlang_ast::token::Literal,
        _scope: &Rc<RefCell<SymbolTable>>,
    ) {
    }
}

pub fn walk_module<'hir, V: ScopedVisitor<'hir>>(
    visitor: &mut V,
    module: &'hir mut hir::Module,
    _scope: &Rc<RefCell<SymbolTable>>,
) {
    let symbol_table = visitor
        .get_symbol_table(module.hir_id)
        .expect("Module should have a symbol table");

    visitor.visit_block(&mut module.block, &symbol_table);
}

pub fn walk_block<'hir, V: ScopedVisitor<'hir>>(
    visitor: &mut V,
    block: &'hir mut hir::Block,
    scope: &Rc<RefCell<SymbolTable>>,
) {
    // TODO: We made a mess where modules have blocks without scopes, and potentially in other
    //       places like loops and function bodies etc.
    let scope = visitor
        .get_symbol_table(block.hir_id)
        .unwrap_or_else(|| scope.clone());

    for statement in &mut block.stmts {
        visitor.visit_stmt(statement, &scope);
    }

    if let Some(expr) = &mut block.expr {
        visitor.visit_expr(expr, &scope);
    }
}

pub fn walk_stmt<'hir, V: ScopedVisitor<'hir>>(
    visitor: &mut V,
    stmt: &'hir mut hir::Stmt,
    scope: &Rc<RefCell<SymbolTable>>,
) {
    match &mut stmt.kind {
        hir::StmtKind::Let(pat, expr, ty) => {
            visitor.visit_expr(expr, scope);
            visitor.visit_pat(pat, scope);
            visitor.visit_ty(ty, scope);
        }
        hir::StmtKind::Expr(expr) => visitor.visit_expr(expr, scope),
        hir::StmtKind::Return(expr) => {
            if let Some(expr) = expr.as_mut() {
                visitor.visit_expr(expr, scope);
            }
        }
        hir::StmtKind::FunctionDeclaration(decl) => {
            visitor.visit_expr(&mut decl.name, scope);

            for param in &mut decl.parameters {
                visitor.visit_ident(&mut param.name, scope);
                visitor.visit_ty(&mut param.type_annotation, scope);
            }

            visitor.visit_block(&mut decl.body, scope);
        }
        hir::StmtKind::StructDeclaration(decl) => {
            for field in &mut decl.fields {
                visitor.visit_ident(&mut field.name, scope);
                visitor.visit_ty(&mut field.ty, scope);
            }
        }
        hir::StmtKind::EnumDeclaration(decl) => {
            for variant in &mut decl.variants {
                visitor.visit_ident(&mut variant.name, scope);

                for field in &mut variant.parameters {
                    visitor.visit_ident(&mut field.name, scope);
                    visitor.visit_ty(&mut field.ty, scope);
                }
            }
        }
        hir::StmtKind::DynFunctionDeclaration(_decl) => {}
    }
}

pub fn walk_expr<'hir, V: ScopedVisitor<'hir>>(
    visitor: &mut V,
    expr: &'hir mut hir::Expr,
    scope: &Rc<RefCell<SymbolTable>>,
) {
    match &mut expr.kind {
        hir::ExprKind::Path(path) => visitor.visit_path(path, scope),
        hir::ExprKind::Unary(_, expr) => visitor.visit_expr(expr, scope),
        hir::ExprKind::Let(pat, expr) => {
            visitor.visit_expr(expr, scope);
            visitor.visit_pat(pat, scope);
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            visitor.visit_expr(lhs, scope);
            visitor.visit_expr(rhs, scope);
        }
        hir::ExprKind::IfElse(condition, consequence, else_branches) => {
            visitor.visit_expr(condition, scope);
            visitor.visit_block(consequence, scope);

            for hir::ElseClause {
                condition,
                consequence,
            } in else_branches
            {
                if let Some(expr) = condition {
                    visitor.visit_expr(expr, scope);
                }

                visitor.visit_block(consequence, scope);
            }
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            visitor.visit_block(block, scope);
        }
        hir::ExprKind::Break(Some(expr)) => {
            visitor.visit_expr(expr, scope);
        }
        hir::ExprKind::Break(_) => {}
        hir::ExprKind::FunctionExpression(decl) => {
            visitor.visit_expr(&mut decl.name, scope);

            for param in &mut decl.parameters {
                visitor.visit_ident(&mut param.name, scope);
                visitor.visit_ty(&mut param.type_annotation, scope);
            }

            visitor.visit_block(&mut decl.body, scope);
        }
        hir::ExprKind::Call(call_expr) | hir::ExprKind::TailCall(call_expr) => {
            visitor.visit_expr(&mut call_expr.callee, scope);

            for arg in &mut call_expr.arguments {
                visitor.visit_expr(arg, scope);
            }
        }
        hir::ExprKind::FieldAccess(base, ident) => {
            visitor.visit_expr(base, scope);
            visitor.visit_ident(ident, scope);
        }
        hir::ExprKind::IndexAccess(base, expr) => {
            visitor.visit_expr(expr, scope);
            visitor.visit_expr(base, scope);
        }
        hir::ExprKind::Match(expr, arms) => {
            visitor.visit_expr(expr, scope);

            for arm in arms {
                visitor.visit_pat(&mut arm.pat, scope);

                if let Some(guard) = &mut arm.guard {
                    visitor.visit_expr(guard, scope);
                }

                visitor.visit_block(&mut arm.block, scope);
            }
        }
        hir::ExprKind::Dict(pairs) => {
            for (key, value) in pairs {
                visitor.visit_expr(key, scope);
                visitor.visit_expr(value, scope);
            }
        }
        hir::ExprKind::List(exprs) => {
            for value in exprs {
                visitor.visit_expr(value, scope);
            }
        }
        hir::ExprKind::Literal(literal) => visitor.visit_literal(literal, scope),
        hir::ExprKind::Cast(expr, ty) => {
            visitor.visit_expr(expr, scope);
            visitor.visit_ty(ty, scope);
        }
        hir::ExprKind::Wildcard => {}
        hir::ExprKind::Continue => {}
        hir::ExprKind::Range(..) => todo!(),
    }
}

pub fn walk_pat<'hir, V: ScopedVisitor<'hir>>(
    visitor: &mut V,
    pat: &'hir mut hir::Pat,
    scope: &Rc<RefCell<SymbolTable>>,
) {
    match &mut pat.kind {
        hir::PatKind::Identifier(_, ident) => visitor.visit_ident(ident, scope),
        hir::PatKind::List(pats) => {
            for pat in pats {
                visitor.visit_pat(pat, scope);
            }
        }
        hir::PatKind::Rest(pat) => {
            visitor.visit_pat(pat, scope);
        }
        hir::PatKind::Enum(path, fields) => {
            visitor.visit_path(path, scope);

            for (ident, pat) in fields {
                visitor.visit_ident(ident, scope);
                visitor.visit_pat(pat, scope);
            }
        }
        hir::PatKind::Literal(literal) => visitor.visit_literal(literal, scope),
        hir::PatKind::Wildcard => {}
    }
}
