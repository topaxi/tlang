use tlang_hir::hir;

/// Check if an expression can be rendered as a JavaScript expression (vs statement)
pub fn expr_can_render_as_js_expr(expr: &hir::Expr) -> bool {
    match &expr.kind {
        hir::ExprKind::Path(..) | 
        hir::ExprKind::Literal(..) |
        hir::ExprKind::Wildcard => true,
        
        hir::ExprKind::Binary(_, lhs, rhs) => {
            expr_can_render_as_js_expr(lhs) && expr_can_render_as_js_expr(rhs)
        }
        
        hir::ExprKind::Unary(_, operand) => expr_can_render_as_js_expr(operand),
        
        hir::ExprKind::Call(call) => {
            call.arguments.iter().all(|arg| expr_can_render_as_js_expr(arg))
        }
        
        hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
            if_else_can_render_as_ternary(condition, then_branch, else_branches)
        }
        
        hir::ExprKind::List(elements) => {
            elements.iter().all(|elem| expr_can_render_as_js_expr(elem))
        }
        
        hir::ExprKind::FieldAccess(obj, _) => expr_can_render_as_js_expr(obj),
        
        // These expressions cannot be rendered as JavaScript expressions
        hir::ExprKind::Block(..) |
        hir::ExprKind::Loop(..) |
        hir::ExprKind::Break(..) |
        hir::ExprKind::Continue |
        hir::ExprKind::Match(..) |
        hir::ExprKind::TailCall(..) |
        hir::ExprKind::FunctionExpression(..) => false,
        
        _ => false,
    }
}

/// Check if an if-else expression can be rendered as a ternary operator
pub fn if_else_can_render_as_ternary(
    condition: &hir::Expr,
    then_branch: &hir::Block,
    else_branches: &[hir::ElseClause],
) -> bool {
    // Must have exactly one else clause (no else-if chains)
    if else_branches.len() != 1 {
        return false;
    }
    
    let else_branch = &else_branches[0];
    
    // Both branches must have no statements and only completion expressions
    if !then_branch.stmts.is_empty() || !else_branch.consequence.stmts.is_empty() {
        return false;
    }
    
    // Must have completion expressions in both branches
    let then_expr = match &then_branch.expr {
        Some(expr) => expr,
        None => return false,
    };
    
    let else_expr = match &else_branch.consequence.expr {
        Some(expr) => expr,
        None => return false,
    };
    
    // All parts must be simple expressions
    if !expr_can_render_as_js_expr(condition) {
        return false;
    }
    
    expr_can_render_as_js_expr(then_expr) && expr_can_render_as_js_expr(else_expr)
}