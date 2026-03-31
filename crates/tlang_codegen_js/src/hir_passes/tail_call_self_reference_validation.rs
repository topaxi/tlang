use tlang_diagnostics::Diagnostic;
use tlang_hir as hir;
use tlang_span::HirId;

use tlang_hir_opt::HirPass;
use tlang_hir_opt::hir_opt::{HirOptContext, HirOptError};

/// A HIR pass that warns about `rec`-annotated tail calls that are **not**
/// self-referencing (i.e. mutual recursion).
///
/// In the JavaScript backend, only self-referencing tail calls can be compiled
/// into loops (`while (true) { … continue rec; }`). Mutual tail calls are
/// emitted as plain function calls and will **not** benefit from tail-call
/// optimization.
///
/// This pass must run **after** [`SymbolResolution`] so that callee paths have
/// their `res.hir_id()` populated.
///
/// The pass never modifies the HIR; it always returns `Ok(false)`.
#[derive(Default)]
pub struct TailCallSelfReferenceValidation {
    ran: bool,
}

impl HirPass for TailCallSelfReferenceValidation {
    fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        if !self.ran {
            self.ran = true;
            check_block(&module.block, None, &mut ctx.diagnostics);
        }
        Ok(false)
    }
}

fn check_block(block: &hir::Block, current_fn: Option<HirId>, diagnostics: &mut Vec<Diagnostic>) {
    for stmt in &block.stmts {
        check_stmt(stmt, current_fn, diagnostics);
    }
    if let Some(expr) = &block.expr {
        check_expr(expr, current_fn, diagnostics);
    }
}

fn check_stmt(stmt: &hir::Stmt, current_fn: Option<HirId>, diagnostics: &mut Vec<Diagnostic>) {
    match &stmt.kind {
        hir::StmtKind::Let(_, expr, _) => check_expr(expr, current_fn, diagnostics),
        hir::StmtKind::Const(_, _, expr, _) => check_expr(expr, current_fn, diagnostics),
        hir::StmtKind::Expr(expr) => check_expr(expr, current_fn, diagnostics),
        hir::StmtKind::Return(Some(expr)) => check_expr(expr, current_fn, diagnostics),
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
    check_block(&decl.body, Some(decl.hir_id), diagnostics);
}

fn check_expr(expr: &hir::Expr, current_fn: Option<HirId>, diagnostics: &mut Vec<Diagnostic>) {
    match &expr.kind {
        hir::ExprKind::TailCall(call) => {
            if let Some(fn_hir_id) = current_fn {
                let is_self_ref = matches!(
                    &call.callee.kind,
                    hir::ExprKind::Path(path) if path.res.hir_id() == Some(fn_hir_id)
                );
                if !is_self_ref {
                    diagnostics.push(Diagnostic::warn(
                        "`rec` call is not self-referencing; tail call optimization is not available in the JavaScript backend",
                        expr.span,
                    ));
                }
            }
            check_call_args(call, current_fn, diagnostics);
        }
        hir::ExprKind::Call(call) => {
            check_call_args(call, current_fn, diagnostics);
        }
        hir::ExprKind::Block(block) => {
            check_block(block, current_fn, diagnostics);
        }
        hir::ExprKind::Loop(block) => {
            check_block(block, current_fn, diagnostics);
        }
        hir::ExprKind::IfElse(cond, then_block, else_clauses) => {
            check_expr(cond, current_fn, diagnostics);
            check_block(then_block, current_fn, diagnostics);
            for else_clause in else_clauses {
                if let Some(c) = &else_clause.condition {
                    check_expr(c, current_fn, diagnostics);
                }
                check_block(&else_clause.consequence, current_fn, diagnostics);
            }
        }
        hir::ExprKind::Match(scrutinee, arms) => {
            check_expr(scrutinee, current_fn, diagnostics);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    check_expr(guard, current_fn, diagnostics);
                }
                check_block(&arm.block, current_fn, diagnostics);
            }
        }
        hir::ExprKind::FunctionExpression(decl) => {
            check_function_decl(decl, diagnostics);
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            check_expr(lhs, current_fn, diagnostics);
            check_expr(rhs, current_fn, diagnostics);
        }
        hir::ExprKind::Unary(_, operand) => {
            check_expr(operand, current_fn, diagnostics);
        }
        hir::ExprKind::FieldAccess(base, _) => {
            check_expr(base, current_fn, diagnostics);
        }
        hir::ExprKind::IndexAccess(base, index) => {
            check_expr(base, current_fn, diagnostics);
            check_expr(index, current_fn, diagnostics);
        }
        hir::ExprKind::Cast(inner, _) => {
            check_expr(inner, current_fn, diagnostics);
        }
        hir::ExprKind::Let(_, inner) => {
            check_expr(inner, current_fn, diagnostics);
        }
        hir::ExprKind::List(exprs) => {
            for e in exprs {
                check_expr(e, current_fn, diagnostics);
            }
        }
        hir::ExprKind::Dict(pairs) => {
            for (k, v) in pairs {
                check_expr(k, current_fn, diagnostics);
                check_expr(v, current_fn, diagnostics);
            }
        }
        hir::ExprKind::Break(Some(inner)) => {
            check_expr(inner, current_fn, diagnostics);
        }
        hir::ExprKind::Literal(_)
        | hir::ExprKind::Path(_)
        | hir::ExprKind::Wildcard
        | hir::ExprKind::Continue
        | hir::ExprKind::Break(None)
        | hir::ExprKind::Range(_) => {}
        hir::ExprKind::TaggedString { tag, exprs, .. } => {
            check_expr(tag, current_fn, diagnostics);
            for e in exprs {
                check_expr(e, current_fn, diagnostics);
            }
        }
    }
}

fn check_call_args(
    call: &hir::CallExpression,
    current_fn: Option<HirId>,
    diagnostics: &mut Vec<Diagnostic>,
) {
    check_expr(&call.callee, current_fn, diagnostics);
    for arg in &call.arguments {
        check_expr(arg, current_fn, diagnostics);
    }
}
