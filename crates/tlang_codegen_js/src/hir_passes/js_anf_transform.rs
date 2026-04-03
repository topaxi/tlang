use tlang_hir::{self as hir};
use tlang_hir_opt::anf_transform::{AnfFilter, AnfTransform};
use tlang_hir_opt::hir_opt::{HirOptContext, HirOptError, HirPass};

/// Returns true if the expression can be directly emitted as a JavaScript expression.
/// Mirrors `expr_can_render_as_js_expr` in the codegen.
fn is_js_expressible(expr: &hir::Expr) -> bool {
    match &expr.kind {
        hir::ExprKind::Path(..) | hir::ExprKind::Literal(..) | hir::ExprKind::Wildcard => true,
        hir::ExprKind::Binary(_, lhs, rhs) => is_js_expressible(lhs) && is_js_expressible(rhs),
        hir::ExprKind::Unary(_, inner) => is_js_expressible(inner),
        hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
            is_js_expressible(&call.callee) && call.arguments.iter().all(is_js_expressible)
        }
        hir::ExprKind::Cast(inner, _) => is_js_expressible(inner),
        hir::ExprKind::FieldAccess(base, _) => is_js_expressible(base),
        hir::ExprKind::IndexAccess(base, index) => {
            is_js_expressible(base) && is_js_expressible(index)
        }
        hir::ExprKind::List(items) => items.iter().all(is_js_expressible),
        hir::ExprKind::Dict(pairs) => pairs
            .iter()
            .all(|(k, v)| is_js_expressible(k) && is_js_expressible(v)),
        hir::ExprKind::FunctionExpression(..) | hir::ExprKind::Range(..) => true,
        hir::ExprKind::TaggedString { tag, exprs, .. } => {
            is_js_expressible(tag) && exprs.iter().all(is_js_expressible)
        }
        hir::ExprKind::IfElse(cond, then_block, else_branches) => {
            can_render_as_ternary(cond, then_block, else_branches)
        }
        hir::ExprKind::Let(..)
        | hir::ExprKind::Block(..)
        | hir::ExprKind::Loop(..)
        | hir::ExprKind::Break(..)
        | hir::ExprKind::Continue
        | hir::ExprKind::Match(..) => false,
        hir::ExprKind::Implements(inner, _) => is_js_expressible(inner),
    }
}

/// Check whether an if/else can be rendered as a JS ternary expression.
/// Must match the codegen's `should_render_if_else_as_ternary` constraints:
/// - Exactly one else branch (no nested else-if chains)
/// - All branches have expressible completions with no statements
fn can_render_as_ternary(
    cond: &hir::Expr,
    then_block: &hir::Block,
    else_branches: &[hir::ElseClause],
) -> bool {
    // The codegen won't nest ternary expressions for else-if chains.
    else_branches.len() == 1
        && else_branches[0].condition.is_none()
        && then_block.stmts.is_empty()
        && then_block.expr.as_ref().is_some_and(is_js_expressible)
        && is_js_expressible(cond)
        && else_branches[0].consequence.stmts.is_empty()
        && else_branches[0]
            .consequence
            .expr
            .as_ref()
            .is_some_and(is_js_expressible)
}

/// JS-specific ANF filter: only lifts expressions that cannot be represented
/// as JavaScript expressions (if/else with completions, match, block, etc.).
#[derive(Default)]
pub struct JsAnfFilter;

impl AnfFilter for JsAnfFilter {
    fn needs_lifting(&self, expr: &hir::Expr) -> bool {
        !is_js_expressible(expr)
    }
}

/// A selective ANF pass that only lifts expressions which cannot be expressed
/// as JavaScript expressions (if/else with completions, match, block, etc.)
/// when they appear in value positions (let initialisers, call arguments,
/// binary operands, etc.).
///
/// After this pass, every expression in a value position is JS-expressible.
pub struct JsAnfTransform(AnfTransform<JsAnfFilter>);

impl Default for JsAnfTransform {
    fn default() -> Self {
        Self(AnfTransform::new(JsAnfFilter))
    }
}

impl HirPass for JsAnfTransform {
    fn name(&self) -> &'static str {
        "JsAnfTransform"
    }

    fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        self.0.optimize_hir(module, ctx)
    }
}
