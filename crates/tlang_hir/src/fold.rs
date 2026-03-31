use crate::hir;

/// A HIR fold/transform trait. Unlike `Visitor` (which walks mutably in-place),
/// `Folder` takes ownership and returns a (possibly different) node. The default
/// `fold_block` implementation owns the `Vec<Stmt>` so implementors can freely
/// inject new statements via `pending_stmts`.
pub trait Folder: Sized {
    fn fold_module(&mut self, module: hir::Module) -> hir::Module {
        fold_module(self, module)
    }

    fn fold_block(&mut self, block: hir::Block) -> hir::Block {
        fold_block(self, block)
    }

    fn fold_stmt(&mut self, stmt: hir::Stmt) -> SmallVec<hir::Stmt> {
        fold_stmt(self, stmt)
    }

    fn fold_expr(&mut self, expr: hir::Expr) -> hir::Expr {
        fold_expr(self, expr)
    }

    fn fold_pat(&mut self, pat: hir::Pat) -> hir::Pat {
        fold_pat(self, pat)
    }

    fn fold_ty(&mut self, ty: hir::Ty) -> hir::Ty {
        ty
    }
}

/// A small-vec-like return type that avoids heap allocation for the common
/// single-statement case while supporting 1:N statement expansion.
pub enum SmallVec<T> {
    One(T),
    Many(Vec<T>),
}

impl<T> SmallVec<T> {
    pub fn one(item: T) -> Self {
        SmallVec::One(item)
    }

    pub fn many(items: Vec<T>) -> Self {
        SmallVec::Many(items)
    }
}

impl<T> IntoIterator for SmallVec<T> {
    type Item = T;
    type IntoIter = SmallVecIter<T>;

    fn into_iter(self) -> Self::IntoIter {
        match self {
            SmallVec::One(item) => SmallVecIter::One(Some(item)),
            SmallVec::Many(items) => SmallVecIter::Many(items.into_iter()),
        }
    }
}

pub enum SmallVecIter<T> {
    One(Option<T>),
    Many(std::vec::IntoIter<T>),
}

impl<T> Iterator for SmallVecIter<T> {
    type Item = T;

    fn next(&mut self) -> Option<Self::Item> {
        match self {
            SmallVecIter::One(item) => item.take(),
            SmallVecIter::Many(iter) => iter.next(),
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        match self {
            SmallVecIter::One(Some(_)) => (1, Some(1)),
            SmallVecIter::One(None) => (0, Some(0)),
            SmallVecIter::Many(iter) => iter.size_hint(),
        }
    }
}

pub fn fold_module<F: Folder>(folder: &mut F, module: hir::Module) -> hir::Module {
    hir::Module {
        hir_id: module.hir_id,
        block: folder.fold_block(module.block),
        span: module.span,
    }
}

pub fn fold_block<F: Folder>(folder: &mut F, block: hir::Block) -> hir::Block {
    let mut new_stmts = Vec::with_capacity(block.stmts.len());

    for stmt in block.stmts {
        for folded in folder.fold_stmt(stmt) {
            new_stmts.push(folded);
        }
    }

    let new_expr = block.expr.map(|expr| folder.fold_expr(expr));

    hir::Block {
        hir_id: block.hir_id,
        stmts: new_stmts,
        expr: new_expr,
        scope: block.scope,
        span: block.span,
    }
}

pub fn fold_stmt<F: Folder>(folder: &mut F, stmt: hir::Stmt) -> SmallVec<hir::Stmt> {
    let kind = match stmt.kind {
        hir::StmtKind::Expr(expr) => hir::StmtKind::Expr(Box::new(folder.fold_expr(*expr))),
        hir::StmtKind::Let(pat, expr, ty) => hir::StmtKind::Let(
            Box::new(folder.fold_pat(*pat)),
            Box::new(folder.fold_expr(*expr)),
            Box::new(folder.fold_ty(*ty)),
        ),
        hir::StmtKind::Const(visibility, pat, expr, ty) => hir::StmtKind::Const(
            visibility,
            Box::new(folder.fold_pat(*pat)),
            Box::new(folder.fold_expr(*expr)),
            Box::new(folder.fold_ty(*ty)),
        ),
        hir::StmtKind::Return(expr) => {
            hir::StmtKind::Return(expr.map(|e| Box::new(folder.fold_expr(*e))))
        }
        hir::StmtKind::FunctionDeclaration(decl) => {
            hir::StmtKind::FunctionDeclaration(Box::new(fold_function_declaration(folder, *decl)))
        }
        hir::StmtKind::DynFunctionDeclaration(decl) => hir::StmtKind::DynFunctionDeclaration(decl),
        hir::StmtKind::EnumDeclaration(decl) => hir::StmtKind::EnumDeclaration(decl),
        hir::StmtKind::StructDeclaration(decl) => hir::StmtKind::StructDeclaration(decl),
        hir::StmtKind::ProtocolDeclaration(decl) => hir::StmtKind::ProtocolDeclaration(decl),
        hir::StmtKind::ImplBlock(mut impl_block) => {
            impl_block.methods = impl_block
                .methods
                .into_iter()
                .map(|decl| fold_function_declaration(folder, decl))
                .collect();
            hir::StmtKind::ImplBlock(impl_block)
        }
    };

    SmallVec::one(hir::Stmt {
        hir_id: stmt.hir_id,
        kind,
        span: stmt.span,
        leading_comments: stmt.leading_comments,
        trailing_comments: stmt.trailing_comments,
    })
}

pub fn fold_expr<F: Folder>(folder: &mut F, expr: hir::Expr) -> hir::Expr {
    let kind = match expr.kind {
        hir::ExprKind::Path(_) => expr.kind,
        hir::ExprKind::Literal(_) => expr.kind,
        hir::ExprKind::Wildcard => expr.kind,
        hir::ExprKind::Continue => expr.kind,
        hir::ExprKind::Unary(op, inner) => {
            hir::ExprKind::Unary(op, Box::new(folder.fold_expr(*inner)))
        }
        hir::ExprKind::Binary(op, lhs, rhs) => hir::ExprKind::Binary(
            op,
            Box::new(folder.fold_expr(*lhs)),
            Box::new(folder.fold_expr(*rhs)),
        ),
        hir::ExprKind::Let(pat, inner) => hir::ExprKind::Let(
            Box::new(folder.fold_pat(*pat)),
            Box::new(folder.fold_expr(*inner)),
        ),
        hir::ExprKind::Block(block) => hir::ExprKind::Block(Box::new(folder.fold_block(*block))),
        hir::ExprKind::Loop(block) => hir::ExprKind::Loop(Box::new(folder.fold_block(*block))),
        hir::ExprKind::Break(inner) => {
            hir::ExprKind::Break(inner.map(|e| Box::new(folder.fold_expr(*e))))
        }
        hir::ExprKind::Call(call) => {
            hir::ExprKind::Call(Box::new(fold_call_expression(folder, *call)))
        }
        hir::ExprKind::TailCall(call) => {
            hir::ExprKind::TailCall(Box::new(fold_call_expression(folder, *call)))
        }
        hir::ExprKind::Cast(inner, ty) => hir::ExprKind::Cast(
            Box::new(folder.fold_expr(*inner)),
            Box::new(folder.fold_ty(*ty)),
        ),
        hir::ExprKind::FieldAccess(base, ident) => {
            hir::ExprKind::FieldAccess(Box::new(folder.fold_expr(*base)), ident)
        }
        hir::ExprKind::IndexAccess(base, index) => hir::ExprKind::IndexAccess(
            Box::new(folder.fold_expr(*base)),
            Box::new(folder.fold_expr(*index)),
        ),
        hir::ExprKind::List(items) => {
            hir::ExprKind::List(items.into_iter().map(|e| folder.fold_expr(e)).collect())
        }
        hir::ExprKind::Dict(pairs) => hir::ExprKind::Dict(
            pairs
                .into_iter()
                .map(|(k, v)| (folder.fold_expr(k), folder.fold_expr(v)))
                .collect(),
        ),
        hir::ExprKind::FunctionExpression(decl) => {
            hir::ExprKind::FunctionExpression(Box::new(fold_function_declaration(folder, *decl)))
        }
        hir::ExprKind::IfElse(cond, then_block, else_branches) => hir::ExprKind::IfElse(
            Box::new(folder.fold_expr(*cond)),
            Box::new(folder.fold_block(*then_block)),
            else_branches
                .into_iter()
                .map(|clause| hir::ElseClause {
                    condition: clause.condition.map(|c| folder.fold_expr(c)),
                    consequence: folder.fold_block(clause.consequence),
                })
                .collect(),
        ),
        hir::ExprKind::Match(scrutinee, arms) => hir::ExprKind::Match(
            Box::new(folder.fold_expr(*scrutinee)),
            arms.into_iter()
                .map(|arm| hir::MatchArm {
                    hir_id: arm.hir_id,
                    pat: folder.fold_pat(arm.pat),
                    guard: arm.guard.map(|g| folder.fold_expr(g)),
                    block: folder.fold_block(arm.block),
                    pat_locals: arm.pat_locals,
                    leading_comments: arm.leading_comments,
                    trailing_comments: arm.trailing_comments,
                })
                .collect(),
        ),
        hir::ExprKind::Range(range) => hir::ExprKind::Range(Box::new(hir::RangeExpression {
            start: folder.fold_expr(range.start),
            end: folder.fold_expr(range.end),
            inclusive: range.inclusive,
        })),
        hir::ExprKind::TaggedString { tag, parts, exprs } => hir::ExprKind::TaggedString {
            tag: Box::new(folder.fold_expr(*tag)),
            parts,
            exprs: exprs.into_iter().map(|e| folder.fold_expr(e)).collect(),
        },
    };

    hir::Expr {
        hir_id: expr.hir_id,
        kind,
        ty: folder.fold_ty(expr.ty),
        span: expr.span,
    }
}

pub fn fold_pat<F: Folder>(folder: &mut F, pat: hir::Pat) -> hir::Pat {
    let kind = match pat.kind {
        hir::PatKind::Wildcard => hir::PatKind::Wildcard,
        hir::PatKind::Identifier(_, _) => pat.kind,
        hir::PatKind::Literal(_) => pat.kind,
        hir::PatKind::List(pats) => {
            hir::PatKind::List(pats.into_iter().map(|p| folder.fold_pat(p)).collect())
        }
        hir::PatKind::Rest(inner) => hir::PatKind::Rest(Box::new(folder.fold_pat(*inner))),
        hir::PatKind::Enum(path, fields) => hir::PatKind::Enum(
            path,
            fields
                .into_iter()
                .map(|(ident, p)| (ident, folder.fold_pat(p)))
                .collect(),
        ),
    };

    hir::Pat {
        kind,
        ty: folder.fold_ty(pat.ty),
        span: pat.span,
    }
}

fn fold_call_expression<F: Folder>(
    folder: &mut F,
    call: hir::CallExpression,
) -> hir::CallExpression {
    hir::CallExpression {
        hir_id: call.hir_id,
        callee: folder.fold_expr(call.callee),
        arguments: call
            .arguments
            .into_iter()
            .map(|arg| folder.fold_expr(arg))
            .collect(),
    }
}

fn fold_function_declaration<F: Folder>(
    folder: &mut F,
    decl: hir::FunctionDeclaration,
) -> hir::FunctionDeclaration {
    hir::FunctionDeclaration {
        hir_id: decl.hir_id,
        visibility: decl.visibility,
        name: folder.fold_expr(decl.name),
        parameters: decl.parameters,
        return_type: folder.fold_ty(decl.return_type),
        body: folder.fold_block(decl.body),
        span: decl.span,
    }
}
