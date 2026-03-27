use tlang_ast::node::UnaryOp;
use tlang_ast::token::Literal;
use tlang_hir::{self as hir};
use tlang_hir_opt::hir_opt::{HirOptContext, HirOptError, HirPass};

/// HIR pass that simplifies conditional returns of boolean literals into
/// direct returns of the condition (or its negation).
///
/// Before:
/// ```text
/// if (cond) { return true; } else { return false; }
/// ```
///
/// After:
/// ```text
/// return cond;
/// ```
///
/// This pass handles `IfElse` nodes, covering multi-overload enum helpers
/// like `Option.is_some` / `Result.is_ok` after the ANF return optimization
/// has converted match arms into if/else branches.
#[derive(Default)]
pub struct BooleanReturnSimplification {
    changed: bool,
}

impl HirPass for BooleanReturnSimplification {
    fn name(&self) -> &'static str {
        "BooleanReturnSimplification"
    }

    fn optimize_hir(
        &mut self,
        module: &mut hir::Module,
        _ctx: &mut HirOptContext,
    ) -> Result<bool, HirOptError> {
        self.changed = false;
        self.visit_block(&mut module.block);
        Ok(self.changed)
    }
}

// ---------------------------------------------------------------------------
// Visitor: walk the entire HIR to find function bodies
// ---------------------------------------------------------------------------

impl BooleanReturnSimplification {
    fn visit_block(&mut self, block: &mut hir::Block) {
        for stmt in &mut block.stmts {
            self.visit_stmt(stmt);
        }
        if let Some(ref mut expr) = block.expr {
            self.visit_expr(expr);
        }
    }

    fn visit_stmt(&mut self, stmt: &mut hir::Stmt) {
        match &mut stmt.kind {
            hir::StmtKind::FunctionDeclaration(decl) => {
                self.visit_fn_body(&mut decl.body);
            }
            hir::StmtKind::ImplBlock(impl_block) => {
                for method in &mut impl_block.methods {
                    self.visit_fn_body(&mut method.body);
                }
            }
            hir::StmtKind::ProtocolDeclaration(decl) => {
                for method in &mut decl.methods {
                    if let Some(ref mut body) = method.body {
                        self.visit_fn_body(body);
                    }
                }
            }
            hir::StmtKind::Expr(expr) => self.visit_expr(expr),
            hir::StmtKind::Let(_, expr, _) => self.visit_expr(expr),
            hir::StmtKind::Return(Some(expr)) => self.visit_expr(expr),
            _ => {}
        }
    }

    fn visit_expr(&mut self, expr: &mut hir::Expr) {
        match &mut expr.kind {
            hir::ExprKind::FunctionExpression(decl) => {
                self.visit_fn_body(&mut decl.body);
            }
            hir::ExprKind::IfElse(cond, then_block, else_branches) => {
                self.visit_expr(cond);
                self.visit_block(then_block);
                for clause in else_branches {
                    if let Some(ref mut c) = clause.condition {
                        self.visit_expr(c);
                    }
                    self.visit_block(&mut clause.consequence);
                }
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                self.visit_expr(scrutinee);
                for arm in arms {
                    self.visit_block(&mut arm.block);
                }
            }
            hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
                self.visit_block(block);
            }
            hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
                self.visit_expr(&mut call.callee);
                for arg in &mut call.arguments {
                    self.visit_expr(arg);
                }
            }
            hir::ExprKind::Binary(_, lhs, rhs) => {
                self.visit_expr(lhs);
                self.visit_expr(rhs);
            }
            hir::ExprKind::Unary(_, inner) | hir::ExprKind::Cast(inner, _) => {
                self.visit_expr(inner);
            }
            hir::ExprKind::FieldAccess(base, _) => self.visit_expr(base),
            hir::ExprKind::IndexAccess(base, index) => {
                self.visit_expr(base);
                self.visit_expr(index);
            }
            hir::ExprKind::List(items) => {
                for item in items {
                    self.visit_expr(item);
                }
            }
            hir::ExprKind::Dict(pairs) => {
                for (k, v) in pairs {
                    self.visit_expr(k);
                    self.visit_expr(v);
                }
            }
            _ => {}
        }
    }

    /// Visit a function body: first recurse into nested functions, then try to
    /// optimize the body itself.
    fn visit_fn_body(&mut self, body: &mut hir::Block) {
        self.visit_block(body);
        self.try_optimize_fn_body(body);
    }
}

// ---------------------------------------------------------------------------
// Pattern detection + rewriting
// ---------------------------------------------------------------------------

impl BooleanReturnSimplification {
    /// Detect and apply the boolean-return simplification on a function body.
    fn try_optimize_fn_body(&mut self, body: &mut hir::Block) {
        for stmt in &mut body.stmts {
            if try_simplify_stmt(stmt) {
                self.changed = true;
            }
        }
    }
}

/// Try to simplify a single statement. Returns `true` if the statement was
/// changed.
fn try_simplify_stmt(stmt: &mut hir::Stmt) -> bool {
    // First, detect the pattern by reading only.
    let simplification = match &stmt.kind {
        hir::StmtKind::Expr(expr) => match &expr.kind {
            hir::ExprKind::IfElse(_, then_block, else_branches) => {
                detect_if_else_bool_return(then_block, else_branches)
            }
            _ => None,
        },
        _ => None,
    };

    let Some(negate) = simplification else {
        return false;
    };

    // Now mutate: extract the condition from the IfElse and build a Return.
    let old_kind = std::mem::replace(&mut stmt.kind, hir::StmtKind::Return(None));
    let hir::StmtKind::Expr(expr) = old_kind else {
        unreachable!();
    };
    let hir::ExprKind::IfElse(cond, _, _) = expr.kind else {
        unreachable!();
    };

    let return_expr = if negate { negate_expr(*cond) } else { *cond };

    stmt.kind = hir::StmtKind::Return(Some(Box::new(return_expr)));
    true
}

/// Detect the boolean-return pattern in an if/else. Returns `Some(negate)`
/// where `negate` indicates whether the condition should be negated:
/// - `Some(false)`: then=true, else=false → return cond
/// - `Some(true)`:  then=false, else=true → return !cond
/// - `None`: not a matching pattern
fn detect_if_else_bool_return(
    then_block: &hir::Block,
    else_branches: &[hir::ElseClause],
) -> Option<bool> {
    // Must have exactly one unconditional else branch.
    if else_branches.len() != 1 || else_branches[0].condition.is_some() {
        return None;
    }

    let b1 = block_returns_bool(then_block)?;
    let b2 = block_returns_bool(&else_branches[0].consequence)?;

    // Both branches return the same boolean — not our pattern.
    if b1 == b2 {
        return None;
    }

    // b1=true, b2=false → return cond (negate=false)
    // b1=false, b2=true → return !cond (negate=true)
    Some(!b1)
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Check if a block consists of a single `return <bool_literal>;` statement
/// and return the boolean value.
fn block_returns_bool(block: &hir::Block) -> Option<bool> {
    if block.stmts.len() != 1 || block.expr.is_some() {
        return None;
    }

    if let hir::StmtKind::Return(Some(expr)) = &block.stmts[0].kind
        && let hir::ExprKind::Literal(lit) = &expr.kind
        && let Literal::Boolean(b) = **lit
    {
        return Some(b);
    }

    None
}

/// Wrap an expression in `!expr` (logical NOT).
fn negate_expr(expr: hir::Expr) -> hir::Expr {
    hir::Expr {
        hir_id: expr.hir_id,
        span: expr.span,
        ty: expr.ty.clone(),
        kind: hir::ExprKind::Unary(UnaryOp::Not, Box::new(expr)),
    }
}
