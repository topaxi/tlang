use tlang_diagnostics::Diagnostic;
use tlang_hir as hir;

use crate::hir_opt::{HirOptContext, HirPass};

/// A HIR pass that validates `rec`-annotated tail calls.
///
/// Emits a warning diagnostic for every [`hir::ExprKind::TailCall`] node that
/// is **not** in tail position.  A call is in tail position when its result is
/// the direct return value of the enclosing function (or the completion
/// expression of an `if`/`match`/block that is itself in tail position).
///
/// The pass never modifies the HIR; it always returns `false`.
#[derive(Default)]
pub struct TailPositionAnalysis;

impl HirPass for TailPositionAnalysis {
    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        check_block(&module.block, false, &mut ctx.diagnostics);
        false
    }
}

fn check_block(block: &hir::Block, is_tail: bool, diagnostics: &mut Vec<Diagnostic>) {
    for stmt in &block.stmts {
        check_stmt(stmt, diagnostics);
    }
    if let Some(expr) = &block.expr {
        check_expr(expr, is_tail, diagnostics);
    }
}

fn check_stmt(stmt: &hir::Stmt, diagnostics: &mut Vec<Diagnostic>) {
    match &stmt.kind {
        hir::StmtKind::Let(_, expr, _) => check_expr(expr, false, diagnostics),
        hir::StmtKind::Expr(expr) => check_expr(expr, false, diagnostics),
        hir::StmtKind::Return(Some(expr)) => check_expr(expr, true, diagnostics),
        hir::StmtKind::Return(None) => {}
        hir::StmtKind::FunctionDeclaration(decl) => check_function_decl(decl, diagnostics),
        hir::StmtKind::DynFunctionDeclaration(_) => {}
        hir::StmtKind::EnumDeclaration(_) => {}
        hir::StmtKind::StructDeclaration(_) => {}
        hir::StmtKind::ProtocolDeclaration(_) => {}
        hir::StmtKind::ImplBlock(impl_block) => {
            for method in &impl_block.methods {
                check_function_decl(method, diagnostics);
            }
        }
    }
}

fn check_function_decl(decl: &hir::FunctionDeclaration, diagnostics: &mut Vec<Diagnostic>) {
    check_block(&decl.body, true, diagnostics);
}

fn check_expr(expr: &hir::Expr, is_tail: bool, diagnostics: &mut Vec<Diagnostic>) {
    match &expr.kind {
        hir::ExprKind::TailCall(call) => {
            if !is_tail {
                diagnostics.push(Diagnostic::warn(
                    "`rec` call is not in tail position; the `rec` annotation has no effect",
                    expr.span,
                ));
            }
            check_call_args(call, diagnostics);
        }
        hir::ExprKind::Call(call) => {
            check_call_args(call, diagnostics);
        }
        hir::ExprKind::Block(block) => {
            check_block(block, is_tail, diagnostics);
        }
        hir::ExprKind::Loop(block) => {
            check_block(block, false, diagnostics);
        }
        hir::ExprKind::IfElse(cond, then_block, else_clauses) => {
            check_expr(cond, false, diagnostics);
            check_block(then_block, is_tail, diagnostics);
            for else_clause in else_clauses {
                if let Some(c) = &else_clause.condition {
                    check_expr(c, false, diagnostics);
                }
                check_block(&else_clause.consequence, is_tail, diagnostics);
            }
        }
        hir::ExprKind::Match(scrutinee, arms) => {
            check_expr(scrutinee, false, diagnostics);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    check_expr(guard, false, diagnostics);
                }
                check_block(&arm.block, is_tail, diagnostics);
            }
        }
        hir::ExprKind::FunctionExpression(decl) => {
            check_function_decl(decl, diagnostics);
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            check_expr(lhs, false, diagnostics);
            check_expr(rhs, false, diagnostics);
        }
        hir::ExprKind::Unary(_, operand) => {
            check_expr(operand, false, diagnostics);
        }
        hir::ExprKind::FieldAccess(base, _) => {
            check_expr(base, false, diagnostics);
        }
        hir::ExprKind::IndexAccess(base, index) => {
            check_expr(base, false, diagnostics);
            check_expr(index, false, diagnostics);
        }
        hir::ExprKind::Cast(inner, _) => {
            check_expr(inner, false, diagnostics);
        }
        hir::ExprKind::Let(_, inner) => {
            check_expr(inner, false, diagnostics);
        }
        hir::ExprKind::List(exprs) => {
            for e in exprs {
                check_expr(e, false, diagnostics);
            }
        }
        hir::ExprKind::Dict(pairs) => {
            for (k, v) in pairs {
                check_expr(k, false, diagnostics);
                check_expr(v, false, diagnostics);
            }
        }
        hir::ExprKind::Break(Some(inner)) => {
            check_expr(inner, false, diagnostics);
        }
        hir::ExprKind::Literal(_)
        | hir::ExprKind::Path(_)
        | hir::ExprKind::Wildcard
        | hir::ExprKind::Continue
        | hir::ExprKind::Break(None)
        | hir::ExprKind::Range(_) => {}
    }
}

fn check_call_args(call: &hir::CallExpression, diagnostics: &mut Vec<Diagnostic>) {
    check_expr(&call.callee, false, diagnostics);
    for arg in &call.arguments {
        check_expr(arg, false, diagnostics);
    }
}
