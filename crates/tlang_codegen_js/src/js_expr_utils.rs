use tlang_hir::hir;

/// Check if an expression can be rendered as a JavaScript expression (vs statement)
pub fn expr_can_render_as_js_expr(expr: &hir::Expr) -> bool {
    match &expr.kind {
        hir::ExprKind::Path(..) | hir::ExprKind::Literal(..) | hir::ExprKind::Wildcard => true,

        hir::ExprKind::Binary(_, lhs, rhs) => {
            expr_can_render_as_js_expr(lhs) && expr_can_render_as_js_expr(rhs)
        }

        hir::ExprKind::Unary(_, operand) => expr_can_render_as_js_expr(operand),

        hir::ExprKind::Call(call) => call.arguments.iter().all(expr_can_render_as_js_expr),

        hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
            if_else_can_render_as_ternary(condition, then_branch, else_branches)
        }

        hir::ExprKind::List(elements) => elements.iter().all(expr_can_render_as_js_expr),

        hir::ExprKind::FieldAccess(obj, _) => expr_can_render_as_js_expr(obj),

        hir::ExprKind::IndexAccess(obj, index) => {
            expr_can_render_as_js_expr(obj) && expr_can_render_as_js_expr(index)
        }

        hir::ExprKind::Dict(pairs) => pairs.iter().all(|(key, value)| {
            expr_can_render_as_js_expr(key) && expr_can_render_as_js_expr(value)
        }),

        hir::ExprKind::FunctionExpression(..) => true,

        // These expressions cannot be rendered as JavaScript expressions
        hir::ExprKind::Block(..)
        | hir::ExprKind::Loop(..)
        | hir::ExprKind::Break(..)
        | hir::ExprKind::Continue
        | hir::ExprKind::Match(..)
        | hir::ExprKind::TailCall(..) => false,

        _ => false,
    }
}

/// Check if an expression can be handled directly in assignment context (like let statements)
/// This is less restrictive than expr_can_render_as_js_expr because some constructs
/// can be handled directly as assignments even if they're not valid JS expressions.
///
/// Example: Dictionary literals with complex values that need transformation
/// can still be assigned directly in some cases.
pub fn expr_can_render_as_assignment_rhs(expr: &hir::Expr) -> bool {
    match &expr.kind {
        // All JS expressions can be assigned
        _ if expr_can_render_as_js_expr(expr) => true,

        // Dictionary literals can be directly assigned
        hir::ExprKind::Dict(pairs) => pairs
            .iter()
            .all(|(_, value)| expr_can_render_as_js_expr(value)),

        // Simple block expressions can potentially be directly assigned
        hir::ExprKind::Block(_block) => {
            // For now, be conservative and flatten all blocks to temp variables
            // This ensures consistency with expected behavior
            false
        }

        // These still need flattening
        hir::ExprKind::Loop(..)
        | hir::ExprKind::Break(..)
        | hir::ExprKind::Continue
        | hir::ExprKind::Match(..)
        | hir::ExprKind::TailCall(..) => false,

        _ => false,
    }
}

/// Check if an expression can be rendered as a JavaScript statement.
/// This is for expressions that appear as standalone statements in statement position.
/// 
/// This function determines if the entire expression can be skipped for HIR JS pass transformation.
/// If this function returns true, the whole expression can be skipped for transformation.
///
/// Example: Simple expressions like `x + y` can be rendered directly as `x + y;` statements,
/// but complex expressions like `match x { ... }` need transformation to if-else statements.
pub fn expr_can_render_as_js_stmt(expr: &hir::Expr) -> bool {
    match &expr.kind {
        // All JS expressions can be statements
        _ if expr_can_render_as_js_expr(expr) => true,

        // If-else can be rendered as JavaScript if-else statements
        hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
            // The condition must be a simple expression
            if !expr_can_render_as_js_expr(condition) {
                return false;
            }

            // All branches must be simple blocks (no complex expressions within)
            if !block_can_render_as_js_stmt_block(then_branch) {
                return false;
            }

            else_branches
                .iter()
                .all(|else_clause| block_can_render_as_js_stmt_block(&else_clause.consequence))
        }

        // Function calls can be statements
        hir::ExprKind::Call(..) => true,

        // Block expressions can be rendered as JavaScript statements
        hir::ExprKind::Block(block) => block_can_render_as_js_stmt_block(block),

        // Loop expressions can be rendered as JavaScript statements when in statement position
        hir::ExprKind::Loop(..) => true,

        // Match expressions can be rendered as JavaScript statements if all arms have return statements
        // or if all arms end with control flow statements (break/continue)
        hir::ExprKind::Match(_, arms) => {
            // If all arms have return statements or control flow statements, this match can be rendered as a JS statement
            arms.iter().all(|arm| {
                if let Some(last_stmt) = arm.block.stmts.last() {
                    match &last_stmt.kind {
                        hir::StmtKind::Return(_) => true,
                        hir::StmtKind::Expr(expr) => {
                            matches!(expr.kind, hir::ExprKind::Break(_) | hir::ExprKind::Continue)
                        }
                        _ => false,
                    }
                } else {
                    false
                }
            })
        }

        // These expressions cannot be rendered as JavaScript statements
        hir::ExprKind::Break(..)
        | hir::ExprKind::Continue
        | hir::ExprKind::TailCall(..)
        | hir::ExprKind::FunctionExpression(..) => false,

        _ => false,
    }
}

/// Check if a block can be rendered as a JavaScript statement block.
/// 
/// A block might not be renderable in statement position if it contains complex expressions
/// that need transformation (like match expressions or loop expressions with break values).
///
/// Example: `{ let x = 1; x + 1 }` can be rendered directly, but 
/// `{ let x = match y { ... }; x }` needs transformation if the match expression is complex.
fn block_can_render_as_js_stmt_block(block: &hir::Block) -> bool {
    // All statements in the block must be simple
    for stmt in &block.stmts {
        match &stmt.kind {
            hir::StmtKind::Let(_, expr, _) => {
                if !expr_can_render_as_assignment_rhs(expr) {
                    return false;
                }
            }
            hir::StmtKind::Expr(expr) => {
                if !expr_can_render_as_js_stmt(expr) {
                    return false;
                }
            }
            hir::StmtKind::Return(Some(expr)) => {
                if !expr_can_render_as_js_expr(expr) {
                    return false;
                }
            }
            hir::StmtKind::Return(None) => {} // Simple return

            // These declarations are fine as JavaScript statements
            hir::StmtKind::FunctionDeclaration(..)
            | hir::StmtKind::DynFunctionDeclaration(..)
            | hir::StmtKind::EnumDeclaration(..)
            | hir::StmtKind::StructDeclaration(..) => {}
        }
    }

    // Completion expression (if any) must be simple, except for loop expressions
    // which can be converted to statements within the block
    if let Some(completion_expr) = &block.expr {
        match &completion_expr.kind {
            hir::ExprKind::Loop(..) => {
                // Loop expressions in block completion position can be converted to statements
                true
            }
            _ => expr_can_render_as_js_expr(completion_expr),
        }
    } else {
        true
    }
}
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
