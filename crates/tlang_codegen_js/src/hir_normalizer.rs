use tlang_hir::{Visitor, hir, visit};
use tlang_hir_opt::HirPass;

/// A normalizer for our HIR to transform some constructs into a more close representation of what
/// JavaScript can do, for example converting `if` expressions into `if` statements.
pub struct HirNormalizer;

impl HirNormalizer {
    pub fn new() -> Self {
        HirNormalizer
    }

    pub fn normalize(&mut self, hir: &mut hir::Module) {
        self.optimize_module(hir);
    }
}

impl Default for HirNormalizer {
    fn default() -> Self {
        Self::new()
    }
}

impl<'hir> Visitor<'hir> for HirNormalizer {
    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt) {
        visit::walk_stmt(self, stmt);
    }

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr) {
        visit::walk_expr(self, expr);
    }
}

impl HirPass for HirNormalizer {
    fn optimize_module(&mut self, module: &mut hir::Module) -> bool {
        self.visit_module(module);
        false
    }
}

pub(crate) fn expr_can_render_as_js_expr(expr: &hir::Expr) -> bool {
    match &expr.kind {
        hir::ExprKind::Path(..) => true,
        hir::ExprKind::Literal(..) => true,
        hir::ExprKind::FunctionExpression(..) => true,
        hir::ExprKind::Wildcard => true,

        hir::ExprKind::Binary(_, lhs, rhs) => {
            expr_can_render_as_js_expr(lhs) && expr_can_render_as_js_expr(rhs)
        }
        hir::ExprKind::Unary(_, expr) => expr_can_render_as_js_expr(expr),
        hir::ExprKind::Call(call_expr) => {
            expr_can_render_as_js_expr(&call_expr.callee)
                && call_expr.arguments.iter().all(expr_can_render_as_js_expr)
        }
        hir::ExprKind::Cast(expr, _) => expr_can_render_as_js_expr(expr),
        hir::ExprKind::FieldAccess(base, _) => expr_can_render_as_js_expr(base),
        hir::ExprKind::IndexAccess(base, index) => {
            expr_can_render_as_js_expr(base) && expr_can_render_as_js_expr(index)
        }
        hir::ExprKind::List(exprs) => exprs.iter().all(expr_can_render_as_js_expr),
        hir::ExprKind::Dict(exprs) => exprs
            .iter()
            .all(|(k, v)| expr_can_render_as_js_expr(k) && expr_can_render_as_js_expr(v)),
        hir::ExprKind::Range(range_expr) => {
            expr_can_render_as_js_expr(&range_expr.start)
                && expr_can_render_as_js_expr(&range_expr.end)
        }
        hir::ExprKind::IfElse(expr, then_branch, else_branches) => {
            if_else_can_render_as_ternary(expr, then_branch, else_branches)
        }
        _ => false,
    }
}

pub(crate) fn if_else_can_render_as_ternary(
    expr: &hir::Expr,
    then_branch: &hir::Block,
    else_branches: &[hir::ElseClause],
) -> bool {
    then_branch.stmts.is_empty()
        && expr_can_render_as_js_expr(expr)
        && expr_can_render_as_js_expr(then_branch.expr.as_ref().unwrap())
        && else_branches.iter().all(|else_branch| {
            else_branch.consequence.stmts.is_empty()
                && (else_branch.condition.is_none()
                    || expr_can_render_as_js_expr(else_branch.condition.as_ref().unwrap()))
                && expr_can_render_as_js_expr(else_branch.consequence.expr.as_ref().unwrap())
        })
}
