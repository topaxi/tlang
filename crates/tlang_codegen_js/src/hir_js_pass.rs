use tlang_ast::node::Ident;
use tlang_hir::{Visitor, hir, visit};
use tlang_hir_opt::{HirOptContext, HirPass};
use tlang_span::{HirId, Span};

use crate::function_generator::fn_identifier_to_string;

#[derive(Debug, Default)]
pub struct HirJsPass {
    temp_var_counter: usize,
    changes_made: bool,
    /// Debug counter to track pass iterations
    iteration_counter: usize,
    /// Track TailCall expressions that have been processed to avoid re-processing
    processed_tail_calls: std::collections::HashSet<HirId>,
}

impl HirJsPass {
    pub fn new() -> Self {
        Self {
            temp_var_counter: 0,
            changes_made: false,
            iteration_counter: 0,
            processed_tail_calls: std::collections::HashSet::new(),
        }
    }

    /// Create a new statement while preserving comments from the original statement
    fn create_stmt_preserving_comments(
        &self,
        hir_id: HirId,
        kind: hir::StmtKind,
        span: Span,
        original_stmt: &hir::Stmt,
    ) -> hir::Stmt {
        hir::Stmt {
            hir_id,
            kind,
            span,
            leading_comments: original_stmt.leading_comments.clone(),
            trailing_comments: original_stmt.trailing_comments.clone(),
        }
    }

    fn generate_temp_var_name(&mut self) -> String {
        let name = format!("$hir${}", self.temp_var_counter);
        self.temp_var_counter += 1;
        name
    }

    fn create_temp_var_path(
        &mut self,
        ctx: &mut HirOptContext,
        temp_name: &str,
        span: Span,
    ) -> hir::Expr {
        let hir_id = ctx.hir_id_allocator.next_id();
        let ident = Ident::new(temp_name, span);
        let path_segment = hir::PathSegment { ident };
        let path = hir::Path::new(vec![path_segment], span);

        hir::Expr {
            hir_id,
            kind: hir::ExprKind::Path(Box::new(path)),
            span,
        }
    }

    fn create_assignment_stmt(
        &mut self,
        ctx: &mut HirOptContext,
        temp_name: &str,
        value_expr: hir::Expr,
        span: Span,
    ) -> hir::Stmt {
        let hir_id = ctx.hir_id_allocator.next_id();
        let temp_path = self.create_temp_var_path(ctx, temp_name, span);

        // Create an assignment using a binary operator (this is a simplification)
        // In a more complete implementation, we might need a dedicated assignment statement type
        let assignment_expr = hir::Expr {
            hir_id: ctx.hir_id_allocator.next_id(),
            kind: hir::ExprKind::Binary(
                hir::BinaryOpKind::Assign,
                Box::new(temp_path),
                Box::new(value_expr),
            ),
            span,
        };

        // Note: This creates a new synthetic statement, so no original comments to preserve
        hir::Stmt::new(hir_id, hir::StmtKind::Expr(Box::new(assignment_expr)), span)
    }

    /// Generic function to flatten expressions within any statement
    /// Returns (modified_statement, statements_to_insert_before)
    fn flatten_statement_expressions(
        &mut self,
        stmt: hir::Stmt,
        ctx: &mut HirOptContext,
    ) -> (hir::Stmt, Vec<hir::Stmt>) {
        let mut temp_stmts = Vec::new();

        // Clone the original statement for reference to preserve comments
        let original_stmt = stmt.clone();

        match stmt.kind {
            // Handle let statements with complex expressions
            hir::StmtKind::Let(pat, expr, ty) => {
                // For let statements with complex expressions, flatten based on context
                let needs_flattening = match &expr.kind {
                    hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                        // For if-else expressions in let statements:
                        // - Always flatten if-else-if expressions (multiple else branches)
                        // - Only flatten simple if-else if they cannot be ternaries
                        if else_branches.len() > 1 {
                            // if-else-if expressions cannot be ternaries, always flatten
                            true
                        } else {
                            // Simple if-else, only flatten if they cannot be ternaries
                            !if_else_can_render_as_ternary(condition, then_branch, else_branches)
                        }
                    }
                    hir::ExprKind::Match(..) => {
                        // Match expressions need to be transformed to use temp variables
                        // Always flatten match expressions to ensure proper JavaScript generation
                        true
                    }
                    _ => !expr_can_render_as_js_expr(&expr)
                };
                
                if needs_flattening {
                    // Use generic flattening for the expression
                    let (flattened_expr, mut temp_var_stmts) =
                        self.flatten_expression_to_temp_var(*expr, ctx);

                    // Add the temporary variable statements
                    temp_stmts.append(&mut temp_var_stmts);

                    // Create the modified let statement using the flattened expression
                    let modified_let = self.create_stmt_preserving_comments(
                        original_stmt.hir_id,
                        hir::StmtKind::Let(pat, Box::new(flattened_expr), ty),
                        original_stmt.span,
                        &original_stmt,
                    );

                    self.changes_made = true;
                    (modified_let, temp_stmts)
                } else {
                    // Expression can be rendered as JS, but check subexpressions
                    let (flattened_expr, mut sub_temp_stmts) =
                        self.flatten_subexpressions_generically(*expr, ctx);

                    if !sub_temp_stmts.is_empty() {
                        // Add the temporary variable statements
                        temp_stmts.append(&mut sub_temp_stmts);

                        // Create the modified let statement using the flattened expression
                        let modified_let = self.create_stmt_preserving_comments(
                            original_stmt.hir_id,
                            hir::StmtKind::Let(pat, Box::new(flattened_expr), ty),
                            original_stmt.span,
                            &original_stmt,
                        );

                        self.changes_made = true;
                        (modified_let, temp_stmts)
                    } else {
                        // No changes needed - reconstruct the original statement preserving comments
                        let reconstructed_stmt = self.create_stmt_preserving_comments(
                            original_stmt.hir_id,
                            hir::StmtKind::Let(pat, Box::new(flattened_expr), ty),
                            original_stmt.span,
                            &original_stmt,
                        );
                        (reconstructed_stmt, temp_stmts)
                    }
                }
            }
            // Handle return statements with complex expressions
            hir::StmtKind::Return(Some(expr)) => {
                // Special handling for TailCall expressions - don't flatten them
                if let hir::ExprKind::TailCall(_) = &expr.kind {
                    // TailCall expressions in return statements should be preserved
                    // Return the original statement without any changes
                    (original_stmt, temp_stmts)
                } else if !expr_can_render_as_js_expr(&expr) {
                    // Use generic flattening for the expression
                    let (flattened_expr, mut temp_var_stmts) =
                        self.flatten_expression_to_temp_var(*expr, ctx);

                    // Add the temporary variable statements
                    temp_stmts.append(&mut temp_var_stmts);

                    // Create the modified return statement using the flattened expression
                    let modified_return = self.create_stmt_preserving_comments(
                        original_stmt.hir_id,
                        hir::StmtKind::Return(Some(Box::new(flattened_expr))),
                        original_stmt.span,
                        &original_stmt,
                    );

                    self.changes_made = true;
                    (modified_return, temp_stmts)
                } else {
                    // Expression can be rendered as JS, but check subexpressions
                    let (flattened_expr, mut sub_temp_stmts) =
                        self.flatten_subexpressions_generically(*expr, ctx);

                    if !sub_temp_stmts.is_empty() {
                        // Add the temporary variable statements
                        temp_stmts.append(&mut sub_temp_stmts);

                        // Create the modified return statement using the flattened expression
                        let modified_return = self.create_stmt_preserving_comments(
                            original_stmt.hir_id,
                            hir::StmtKind::Return(Some(Box::new(flattened_expr))),
                            original_stmt.span,
                            &original_stmt,
                        );

                        self.changes_made = true;
                        (modified_return, temp_stmts)
                    } else {
                        // No changes needed - reconstruct the original statement preserving comments
                        let reconstructed_stmt = self.create_stmt_preserving_comments(
                            original_stmt.hir_id,
                            hir::StmtKind::Return(Some(Box::new(flattened_expr))),
                            original_stmt.span,
                            &original_stmt,
                        );
                        (reconstructed_stmt, temp_stmts)
                    }
                }
            }
            // Handle expression statements
            hir::StmtKind::Expr(ref expr) => {
                // For expression statements, we should be conservative:
                // - Blocks and loops in statement position can be handled natively by JavaScript
                // - Only flatten subexpressions when they contain complex nested expressions

                match &expr.kind {
                    // Blocks and loops in statement position can be handled natively by JavaScript
                    hir::ExprKind::Block(_) => {
                        // For blocks, we need to check their contents for loop expressions that need transformation
                        let (flattened_expr, mut sub_temp_stmts) =
                            self.flatten_subexpressions_generically(*expr.clone(), ctx);

                        if !sub_temp_stmts.is_empty() {
                            // Add the temporary variable statements
                            temp_stmts.append(&mut sub_temp_stmts);

                            // Create the modified expression statement
                            let modified_expr_stmt = self.create_stmt_preserving_comments(
                                original_stmt.hir_id,
                                hir::StmtKind::Expr(Box::new(flattened_expr)),
                                original_stmt.span,
                                &original_stmt,
                            );

                            self.changes_made = true;
                            (modified_expr_stmt, temp_stmts)
                        } else {
                            // No changes needed - let the block pass through
                            (original_stmt, temp_stmts)
                        }
                    }
                    hir::ExprKind::Loop(_) => {
                        // Loops in statement position need special handling for JavaScript generation
                        // They should be transformed to use proper for(;;) constructs
                        (original_stmt, temp_stmts)
                    }
                    _ => {
                        // For other expression types, apply generic flattening for complex subexpressions
                        let (flattened_expr, mut sub_temp_stmts) =
                            self.flatten_subexpressions_generically(*expr.clone(), ctx);

                        if !sub_temp_stmts.is_empty() {
                            // Add the temporary variable statements
                            temp_stmts.append(&mut sub_temp_stmts);

                            // Create the modified expression statement
                            let modified_expr_stmt = self.create_stmt_preserving_comments(
                                original_stmt.hir_id,
                                hir::StmtKind::Expr(Box::new(flattened_expr)),
                                original_stmt.span,
                                &original_stmt,
                            );

                            self.changes_made = true;
                            (modified_expr_stmt, temp_stmts)
                        } else {
                            // No changes needed - reconstruct the original statement preserving comments
                            let reconstructed_stmt = self.create_stmt_preserving_comments(
                                original_stmt.hir_id,
                                hir::StmtKind::Expr(Box::new(flattened_expr)),
                                original_stmt.span,
                                &original_stmt,
                            );
                            (reconstructed_stmt, temp_stmts)
                        }
                    }
                }
            }
            // Handle function declarations by processing their bodies
            hir::StmtKind::FunctionDeclaration(func_decl) => {
                // Process the function body to handle any complex expressions in it
                // We need to visit the function body to apply HIR JS transformations
                
                // Clone the function declaration and process its body
                let mut modified_func = func_decl.as_ref().clone();
                
                // Extract function name and track context for tail call detection
                let function_name = fn_identifier_to_string(&modified_func.name);
                
                self.visit_block_with_function_context(&mut modified_func.body, ctx, Some(&function_name));
                
                // Create the modified function declaration statement
                let modified_stmt = self.create_stmt_preserving_comments(
                    original_stmt.hir_id,
                    hir::StmtKind::FunctionDeclaration(Box::new(modified_func)),
                    original_stmt.span,
                    &original_stmt,
                );
                
                (modified_stmt, temp_stmts)
            }
            _ => {
                // For other statement types (like Return(None)), just return as-is
                (stmt, temp_stmts)
            }
        }
    }

    /// Generic function to flatten a complex expression to a temporary variable
    /// Returns (flattened_expr, statements_to_insert)
    fn flatten_expression_to_temp_var(
        &mut self,
        expr: hir::Expr,
        ctx: &mut HirOptContext,
    ) -> (hir::Expr, Vec<hir::Stmt>) {
        // Check if this expression is already a temp variable call - don't process it again
        if let hir::ExprKind::Call(call_expr) = &expr.kind {
            if let hir::ExprKind::Path(path) = &call_expr.callee.kind {
                if path.segments.len() == 1 && (
                    path.segments[0].ident.as_str() == "__TEMP_VAR_IF_ELSE__" ||
                    path.segments[0].ident.as_str() == "__TEMP_VAR_LOOP__" ||
                    path.segments[0].ident.as_str() == "__TEMP_VAR_BLOCK__"
                ) {
                    // This is already a temp variable call - return as-is
                    return (expr, Vec::new());
                }
            }
        }

        // First, try to flatten subexpressions
        let (expr_with_flattened_subs, mut sub_stmts) =
            self.flatten_subexpressions_generically(expr, ctx);

        // For if-else expressions, only flatten if they cannot be ternaries
        // For loop expressions, check if they're in assignment contexts where they need special handling
        // For break/continue/return expressions, always flatten as they cannot be JS expressions
        // For other expressions, check if they can be rendered as JS
        let needs_flattening = match &expr_with_flattened_subs.kind {
            hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                // Always flatten if-else-if expressions (multiple else branches)
                // Only flatten simple if-else expressions that cannot be rendered as ternaries
                if else_branches.len() > 1 {
                    true // if-else-if expressions cannot be ternaries, always flatten
                } else {
                    !if_else_can_render_as_ternary(condition, then_branch, else_branches)
                }
            }
            hir::ExprKind::Loop(..) => true,   // Loop expressions need transformation for JavaScript
            hir::ExprKind::Break(..) => true, // Break expressions always need flattening
            hir::ExprKind::Continue => true,  // Continue expressions always need flattening
            _ => !expr_can_render_as_js_expr(&expr_with_flattened_subs)
        };

        if needs_flattening {
            // This expression needs flattening
            let temp_name = self.generate_temp_var_name();
            let span = expr_with_flattened_subs.span;

            // Create temporary variable assignments for this expression
            let mut temp_stmts = self.create_temp_var_assignment_for_expr(
                ctx,
                &temp_name,
                expr_with_flattened_subs,
                span,
            );

            // Combine the subexpression statements with the temp variable statements
            sub_stmts.append(&mut temp_stmts);

            // Return the temporary variable reference and the statements
            let temp_var_expr = self.create_temp_var_path(ctx, &temp_name, span);
            (temp_var_expr, sub_stmts)
        } else {
            // Expression can be rendered as JS after flattening subexpressions
            (expr_with_flattened_subs, sub_stmts)
        }
    }

    /// Recursively flatten subexpressions in all contexts
    fn flatten_subexpressions_generically(
        &mut self,
        mut expr: hir::Expr,
        ctx: &mut HirOptContext,
    ) -> (hir::Expr, Vec<hir::Stmt>) {
        let mut statements = Vec::new();

        match &mut expr.kind {
            // Handle Binary expressions FIRST, especially short-circuit operators
            hir::ExprKind::Binary(op_kind, lhs, rhs) => {
                // Handle short-circuit operators (|| and &&) specially to preserve semantics
                match op_kind {
                    hir::BinaryOpKind::Or | hir::BinaryOpKind::And => {
                        // For short-circuit operators, we need to be very careful about evaluation order
                        // to preserve short-circuit semantics

                        // Always process the left operand normally
                        if !expr_can_render_as_js_expr(lhs) {
                            let span = lhs.span;
                            let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                            let expr_to_flatten = std::mem::replace(lhs.as_mut(), placeholder);
                            let (flattened, mut stmts) =
                                self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                            **lhs = flattened;
                            statements.append(&mut stmts);
                            self.changes_made = true;
                        }

                        // For the right operand, we MUST NOT flatten any subexpressions if they
                        // would cause side effects or evaluation, as this would break short-circuit
                        // semantics. If the right operand cannot be rendered as JS expression,
                        // then the entire short-circuit expression needs to be transformed.

                        // Do NOT process the right operand subexpressions here for short-circuit
                        // operators. Let the higher-level flattening handle the entire expression.
                    }
                    _ => {
                        // For non-short-circuit operators, flatten both operands normally
                        if !expr_can_render_as_js_expr(lhs) {
                            let span = lhs.span;
                            let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                            let expr_to_flatten = std::mem::replace(lhs.as_mut(), placeholder);
                            let (flattened, mut stmts) =
                                self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                            **lhs = flattened;
                            statements.append(&mut stmts);
                            self.changes_made = true;
                        }
                        if !expr_can_render_as_js_expr(rhs) {
                            let span = rhs.span;
                            let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                            let expr_to_flatten = std::mem::replace(rhs.as_mut(), placeholder);
                            let (flattened, mut stmts) =
                                self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                            **rhs = flattened;
                            statements.append(&mut stmts);
                            self.changes_made = true;
                        }
                    }
                }
            }
            hir::ExprKind::Call(call_expr) => {
                // Flatten function arguments
                for arg in &mut call_expr.arguments {
                    if !expr_can_render_as_js_expr(arg) {
                        let span = arg.span;
                        let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                        let expr_to_flatten = std::mem::replace(arg, placeholder);
                        let (flattened, mut stmts) =
                            self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                        *arg = flattened;
                        statements.append(&mut stmts);
                        self.changes_made = true;
                    }
                }
            }
            hir::ExprKind::TailCall(_call_expr) => {
                // Check if this TailCall has already been processed
                if self.processed_tail_calls.contains(&expr.hir_id) {
                    // Skip processing this TailCall to avoid infinite loops
                    return (expr, statements);
                }
                
                // TailCall expressions should be handled by the JavaScript codegen for tail call optimization.
                // The HIR JS pass should not flatten TailCall arguments as this interferes with 
                // tail call recognition in the codegen phase.
                // Arguments will be flattened by the JavaScript generator when needed.
            }
            hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                // Flatten condition if it's complex
                if !expr_can_render_as_js_expr(condition) {
                    let span = condition.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(condition.as_mut(), placeholder);
                    let (flattened, mut stmts) =
                        self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    **condition = flattened;
                    statements.append(&mut stmts);
                    self.changes_made = true;
                }

                // Recursively handle then and else expressions
                if let Some(then_expr) = &mut then_branch.expr {
                    if !expr_can_render_as_js_expr(then_expr) {
                        let span = then_expr.span;
                        let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                        let expr_to_flatten = std::mem::replace(then_expr, placeholder);
                        let (flattened, mut stmts) =
                            self.flatten_subexpressions_generically(expr_to_flatten, ctx);
                        *then_expr = flattened;
                        statements.append(&mut stmts);
                        self.changes_made = true;
                    }
                }

                for else_branch in else_branches {
                    if let Some(else_expr) = &mut else_branch.consequence.expr {
                        if !expr_can_render_as_js_expr(else_expr) {
                            let span = else_expr.span;
                            let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                            let expr_to_flatten = std::mem::replace(else_expr, placeholder);
                            let (flattened, mut stmts) =
                                self.flatten_subexpressions_generically(expr_to_flatten, ctx);
                            *else_expr = flattened;
                            statements.append(&mut stmts);
                            self.changes_made = true;
                        }
                    }
                }
            }
            hir::ExprKind::List(exprs) => {
                // Flatten list elements
                for expr_elem in exprs {
                    if !expr_can_render_as_js_expr(expr_elem) {
                        let span = expr_elem.span;
                        let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                        let expr_to_flatten = std::mem::replace(expr_elem, placeholder);
                        let (flattened, mut stmts) =
                            self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                        *expr_elem = flattened;
                        statements.append(&mut stmts);
                        self.changes_made = true;
                    }
                }
            }
            hir::ExprKind::Dict(exprs) => {
                // Flatten dictionary key-value pairs
                for (key, value) in exprs {
                    if !expr_can_render_as_js_expr(key) {
                        let span = key.span;
                        let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                        let expr_to_flatten = std::mem::replace(key, placeholder);
                        let (flattened, mut stmts) =
                            self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                        *key = flattened;
                        statements.append(&mut stmts);
                        self.changes_made = true;
                    }
                    if !expr_can_render_as_js_expr(value) {
                        let span = value.span;
                        let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                        let expr_to_flatten = std::mem::replace(value, placeholder);
                        let (flattened, mut stmts) =
                            self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                        *value = flattened;
                        statements.append(&mut stmts);
                        self.changes_made = true;
                    }
                }
            }
            hir::ExprKind::IndexAccess(base, index) => {
                // Flatten base and index expressions
                if !expr_can_render_as_js_expr(base) {
                    let span = base.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(base.as_mut(), placeholder);
                    let (flattened, mut stmts) =
                        self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    **base = flattened;
                    statements.append(&mut stmts);
                    self.changes_made = true;
                }
                if !expr_can_render_as_js_expr(index) {
                    let span = index.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(index.as_mut(), placeholder);
                    let (flattened, mut stmts) =
                        self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    **index = flattened;
                    statements.append(&mut stmts);
                    self.changes_made = true;
                }
            }
            hir::ExprKind::FieldAccess(base, _) => {
                // Flatten base expression
                if !expr_can_render_as_js_expr(base) {
                    let span = base.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(base.as_mut(), placeholder);
                    let (flattened, mut stmts) =
                        self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    **base = flattened;
                    statements.append(&mut stmts);
                    self.changes_made = true;
                }
            }
            hir::ExprKind::Unary(_, operand) => {
                // Flatten unary operand
                if !expr_can_render_as_js_expr(operand) {
                    let span = operand.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(operand.as_mut(), placeholder);
                    let (flattened, mut stmts) =
                        self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    **operand = flattened;
                    statements.append(&mut stmts);
                    self.changes_made = true;
                }
            }
            hir::ExprKind::Cast(operand, _) => {
                // Flatten cast operand
                if !expr_can_render_as_js_expr(operand) {
                    let span = operand.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(operand.as_mut(), placeholder);
                    let (flattened, mut stmts) =
                        self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    **operand = flattened;
                    statements.append(&mut stmts);
                    self.changes_made = true;
                }
            }
            hir::ExprKind::Break(Some(break_expr)) => {
                // For break expressions with values, flatten the value expression
                if !expr_can_render_as_js_expr(break_expr) {
                    let span = break_expr.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(break_expr.as_mut(), placeholder);
                    let (flattened, mut stmts) =
                        self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    **break_expr = flattened;
                    statements.append(&mut stmts);
                    self.changes_made = true;
                }
            }
            hir::ExprKind::Break(None) => {
                // Plain break without value - no subexpressions to flatten
            }
            hir::ExprKind::Continue => {
                // Continue has no subexpressions to flatten
            }
            hir::ExprKind::Loop(_) => {
                // Loop expressions found within other expressions should be flattened to temp variables
                // This handles cases like: assignment = loop { ... }
                // We should never reach here if the outer logic is correct, but handle it defensively
                // The loop should be transformed by flatten_expression_to_temp_var
                // For now, just mark that we found a loop that needs handling at a higher level
                // This will be caught by the outer !expr_can_render_as_js_expr check
            }
            hir::ExprKind::Block(_) => {
                // Block expressions found within other expressions should be flattened to temp variables
                // This handles cases like: assignment = { ... }
                // Similar to loops, this should be handled at a higher level
            }
            _ => {
                // For other expression types, no subexpressions to flatten
            }
        }

        (expr, statements)
    }

    fn transform_break_statements_in_block(
        &mut self,
        block: &mut hir::Block,
        temp_name: &str,
        ctx: &mut HirOptContext,
    ) {
        // Transform statements that contain break expressions
        let mut new_stmts = Vec::new();
        
        for stmt in &mut block.stmts {
            match &mut stmt.kind {
                hir::StmtKind::Expr(expr) => {
                    if let hir::ExprKind::Break(Some(break_expr)) = &expr.kind {
                        // Transform: break value; -> temp_var = value; break;
                        let span = expr.span;
                        
                        // Add assignment statement
                        let assignment_stmt = self.create_assignment_stmt(ctx, temp_name, *break_expr.clone(), span);
                        new_stmts.push(assignment_stmt);
                        
                        // Add plain break statement
                        let plain_break = hir::Stmt::new(
                            ctx.hir_id_allocator.next_id(),
                            hir::StmtKind::Expr(Box::new(hir::Expr {
                                hir_id: ctx.hir_id_allocator.next_id(),
                                kind: hir::ExprKind::Break(None),
                                span,
                            })),
                            span,
                        );
                        new_stmts.push(plain_break);
                    } else {
                        // For other expressions, recursively transform
                        self.transform_break_statements_in_expr(expr, temp_name, ctx);
                        new_stmts.push(stmt.clone());
                    }
                }
                _ => {
                    // For other statement types, recursively transform
                    self.transform_break_statements_in_stmt(stmt, temp_name, ctx);
                    new_stmts.push(stmt.clone());
                }
            }
        }
        
        block.stmts = new_stmts;
        
        // Also check the block's completion expression for break statements
        if let Some(ref mut expr) = block.expr {
            if let hir::ExprKind::Break(Some(break_expr)) = &expr.kind {
                // Transform break in completion position -> temp_var = value; block.expr = None
                let assignment_stmt = self.create_assignment_stmt(ctx, temp_name, *break_expr.clone(), expr.span);
                block.stmts.push(assignment_stmt);
                
                // Add plain break and clear completion expression
                let plain_break = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Break(None),
                        span: expr.span,
                    })),
                    expr.span,
                );
                block.stmts.push(plain_break);
                block.expr = None;
            } else {
                self.transform_break_statements_in_expr(expr, temp_name, ctx);
            }
        }
    }
    
    fn transform_break_statements_in_stmt(
        &mut self,
        stmt: &mut hir::Stmt,
        temp_name: &str,
        ctx: &mut HirOptContext,
    ) {
        match &mut stmt.kind {
            hir::StmtKind::Let(_, expr, _) => {
                self.transform_break_statements_in_expr(expr, temp_name, ctx);
            }
            hir::StmtKind::Return(Some(expr)) => {
                self.transform_break_statements_in_expr(expr, temp_name, ctx);
            }
            hir::StmtKind::Expr(expr) => {
                // For non-break expressions, recursively transform
                if !matches!(expr.kind, hir::ExprKind::Break(_)) {
                    self.transform_break_statements_in_expr(expr, temp_name, ctx);
                }
            }
            _ => {}
        }
    }
    
    fn transform_break_statements_in_expr(
        &mut self,
        expr: &mut hir::Expr,
        temp_name: &str,
        ctx: &mut HirOptContext,
    ) {
        match &mut expr.kind {
            hir::ExprKind::Break(Some(break_expr)) => {
                // Transform: break value; -> (temp_var = value, break)
                // We need to create a sequence that assigns and then breaks
                let span = expr.span;
                
                // Create assignment to temp variable
                let temp_path = self.create_temp_var_path(ctx, temp_name, span);
                let assignment_expr = hir::Expr {
                    hir_id: ctx.hir_id_allocator.next_id(),
                    kind: hir::ExprKind::Binary(
                        hir::BinaryOpKind::Assign,
                        Box::new(temp_path),
                        break_expr.clone(),
                    ),
                    span,
                };
                
                // Replace the break expression with just the assignment (break value becomes just assignment)
                // The break itself will be handled differently - we need to insert assignment before break
                *expr = assignment_expr;
            }
            hir::ExprKind::Break(None) => {
                // Plain break without value - no transformation needed
            }
            hir::ExprKind::Block(block) => {
                self.transform_break_statements_in_block(block, temp_name, ctx);
            }
            hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                self.transform_break_statements_in_expr(condition, temp_name, ctx);
                self.transform_break_statements_in_block(then_branch, temp_name, ctx);
                for else_branch in else_branches {
                    if let Some(ref mut cond) = else_branch.condition {
                        self.transform_break_statements_in_expr(cond, temp_name, ctx);
                    }
                    self.transform_break_statements_in_block(&mut else_branch.consequence, temp_name, ctx);
                }
            }
            hir::ExprKind::Call(call_expr) => {
                for arg in &mut call_expr.arguments {
                    self.transform_break_statements_in_expr(arg, temp_name, ctx);
                }
            }
            hir::ExprKind::Binary(_, lhs, rhs) => {
                self.transform_break_statements_in_expr(lhs, temp_name, ctx);
                self.transform_break_statements_in_expr(rhs, temp_name, ctx);
            }
            hir::ExprKind::Unary(_, operand) => {
                self.transform_break_statements_in_expr(operand, temp_name, ctx);
            }
            hir::ExprKind::List(exprs) => {
                for expr in exprs {
                    self.transform_break_statements_in_expr(expr, temp_name, ctx);
                }
            }
            hir::ExprKind::Dict(pairs) => {
                for (key, value) in pairs {
                    self.transform_break_statements_in_expr(key, temp_name, ctx);
                    self.transform_break_statements_in_expr(value, temp_name, ctx);
                }
            }
            hir::ExprKind::IndexAccess(base, index) => {
                self.transform_break_statements_in_expr(base, temp_name, ctx);
                self.transform_break_statements_in_expr(index, temp_name, ctx);
            }
            hir::ExprKind::FieldAccess(base, _) => {
                self.transform_break_statements_in_expr(base, temp_name, ctx);
            }
            hir::ExprKind::Cast(operand, _) => {
                self.transform_break_statements_in_expr(operand, temp_name, ctx);
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                self.transform_break_statements_in_expr(scrutinee, temp_name, ctx);
                for arm in arms {
                    self.transform_break_statements_in_block(&mut arm.block, temp_name, ctx);
                }
            }
            _ => {
                // For other expression types, no nested expressions to transform
            }
        }
    }

    fn create_temp_var_assignment_for_expr(
        &mut self,
        ctx: &mut HirOptContext,
        temp_name: &str,
        expr: hir::Expr,
        span: Span,
    ) -> Vec<hir::Stmt> {
        self.create_temp_var_assignment_for_expr_internal(ctx, temp_name, expr, span, false, None)
    }

    fn create_temp_var_assignment_for_expr_internal(
        &mut self,
        ctx: &mut HirOptContext,
        temp_name: &str,
        expr: hir::Expr,
        span: Span,
        use_return_statements: bool,
        function_context: Option<&str>,
    ) -> Vec<hir::Stmt> {
        let mut statements = Vec::new();

        // Only create temp variable declaration if we're not using return statements
        if !use_return_statements {
            // For all expressions (including loops), create a let declaration with wildcard as initial value
            let temp_var_decl = hir::Stmt::new(
                ctx.hir_id_allocator.next_id(),
                hir::StmtKind::Let(
                    Box::new(hir::Pat {
                        kind: hir::PatKind::Identifier(
                            ctx.hir_id_allocator.next_id(),
                            Box::new(Ident::new(temp_name, span))
                        ),
                        span,
                    }),
                    Box::new(hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Wildcard,
                        span,
                    }),
                    Box::new(hir::Ty {
                        kind: hir::TyKind::Unknown,
                        span,
                    }), // Unknown type for now
                ),
                span,
            );
            statements.push(temp_var_decl);
        }

        match &expr.kind {
            hir::ExprKind::Block(block) => {
                if block.has_completion() {
                    // Transform block expression to regular statements
                    let mut new_block = block.as_ref().clone();

                    // Move the completion expression to an assignment statement
                    if let Some(completion_expr) = new_block.expr.take() {
                        match &completion_expr.kind {
                            hir::ExprKind::Loop(loop_block) => {
                                // For loop completion expressions, add the loop directly as a statement
                                // with proper break transformations, not as an assignment
                                let mut new_loop_block = loop_block.as_ref().clone();
                                
                                // Transform break statements in the loop to assign to the temp variable
                                self.transform_break_statements_in_block(&mut new_loop_block, temp_name, ctx);
                                
                                // Add the loop as a regular statement
                                let loop_stmt = hir::Stmt::new(
                                    ctx.hir_id_allocator.next_id(),
                                    hir::StmtKind::Expr(Box::new(hir::Expr {
                                        hir_id: ctx.hir_id_allocator.next_id(),
                                        kind: hir::ExprKind::Loop(Box::new(new_loop_block)),
                                        span: completion_expr.span,
                                    })),
                                    completion_expr.span,
                                );
                                new_block.stmts.push(loop_stmt);
                            }
                            _ => {
                                // For non-loop completion expressions, create assignment as usual
                                let assignment_stmt =
                                    self.create_assignment_stmt(ctx, temp_name, completion_expr, span);
                                new_block.stmts.push(assignment_stmt);
                            }
                        }
                    }

                    // Add the block as a regular statement
                    statements.push(hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Expr(Box::new(hir::Expr {
                            hir_id: ctx.hir_id_allocator.next_id(),
                            kind: hir::ExprKind::Block(Box::new(new_block)),
                            span,
                        })),
                        span,
                    ));
                }
            }
            hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                // Transform if/else expressions to regular if/else statements
                let mut new_then_branch = then_branch.as_ref().clone();
                if let Some(completion_expr) = new_then_branch.expr.take() {
                    if use_return_statements {
                        // Use direct return statements instead of temp variable assignments
                        let return_stmt = hir::Stmt::new(
                            ctx.hir_id_allocator.next_id(),
                            hir::StmtKind::Return(Some(Box::new(completion_expr))),
                            span,
                        );
                        new_then_branch.stmts.push(return_stmt);
                    } else {
                        // If the completion expression is an if-else, we need to flatten it too
                        match &completion_expr.kind {
                            hir::ExprKind::IfElse(..) => {
                                // Recursively flatten nested if-else expressions
                                let (flattened_expr, temp_stmts) = self.flatten_expression_to_temp_var(completion_expr, ctx);
                                new_then_branch.stmts.extend(temp_stmts);
                                let assignment_stmt = self.create_assignment_stmt(ctx, temp_name, flattened_expr, span);
                                new_then_branch.stmts.push(assignment_stmt);
                            }
                            _ => {
                                let assignment_stmt = self.create_assignment_stmt(ctx, temp_name, completion_expr, span);
                                new_then_branch.stmts.push(assignment_stmt);
                            }
                        }
                    }
                }

                let mut new_else_branches = Vec::new();
                for else_branch in else_branches {
                    let mut new_else_consequence = else_branch.consequence.clone();
                    if let Some(completion_expr) = new_else_consequence.expr.take() {
                        if use_return_statements {
                            // Use direct return statements instead of temp variable assignments
                            let return_stmt = hir::Stmt::new(
                                ctx.hir_id_allocator.next_id(),
                                hir::StmtKind::Return(Some(Box::new(completion_expr))),
                                span,
                            );
                            new_else_consequence.stmts.push(return_stmt);
                        } else {
                            // If the completion expression is an if-else, we need to flatten it too
                            match &completion_expr.kind {
                                hir::ExprKind::IfElse(..) => {
                                    // Recursively flatten nested if-else expressions
                                    let (flattened_expr, temp_stmts) = self.flatten_expression_to_temp_var(completion_expr, ctx);
                                    new_else_consequence.stmts.extend(temp_stmts);
                                    let assignment_stmt = self.create_assignment_stmt(ctx, temp_name, flattened_expr, span);
                                    new_else_consequence.stmts.push(assignment_stmt);
                                }
                                _ => {
                                    let assignment_stmt = self.create_assignment_stmt(ctx, temp_name, completion_expr, span);
                                    new_else_consequence.stmts.push(assignment_stmt);
                                }
                            }
                        }
                    }
                    new_else_branches.push(hir::ElseClause {
                        condition: else_branch.condition.clone(),
                        consequence: new_else_consequence,
                    });
                }

                // Add the if/else statement directly
                statements.push(hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::IfElse(
                            condition.clone(),
                            Box::new(new_then_branch),
                            new_else_branches,
                        ),
                        span,
                    })),
                    span,
                ));
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                // Transform match expression to use wildcard sentinel approach
                // Similar to loop expressions, we need to transform each arm to assign to the temp variable
                
                // First, flatten any complex expressions in the scrutinee
                let (flattened_scrutinee, mut scrutinee_stmts) = if !expr_can_render_as_js_expr(scrutinee) {
                    self.flatten_expression_to_temp_var(*scrutinee.clone(), ctx)
                } else {
                    (*scrutinee.clone(), Vec::new())
                };
                
                // Add any statements from flattening the scrutinee
                statements.append(&mut scrutinee_stmts);
                
                // Transform each arm
                let mut new_arms = Vec::new();
                for (_i, arm) in arms.iter().enumerate() {
                    let mut new_arm_block = arm.block.clone();
                    
                    // NOTE: We don't call visit_block here because we want to handle the completion expression ourselves
                    // The recursive visit would consume the completion expression before we can transform it to return statements
                    
                    // Transform the arm completion expression to assign to temp variable or return directly
                    if let Some(completion_expr) = new_arm_block.expr.take() {
                        if use_return_statements {
                            // Use direct return statements instead of temp variable assignments
                            // Check if the completion expression is a TailCall and mark it as processed
                            if let hir::ExprKind::TailCall(_) = &completion_expr.kind {
                                // Mark this TailCall as processed to avoid re-processing
                                self.processed_tail_calls.insert(completion_expr.hir_id);
                            }
                            
                            let return_stmt = hir::Stmt::new(
                                ctx.hir_id_allocator.next_id(),
                                hir::StmtKind::Return(Some(Box::new(completion_expr))),
                                span,
                            );
                            new_arm_block.stmts.push(return_stmt);
                        } else {
                            // For complex expressions, flatten them first
                            match &completion_expr.kind {
                                hir::ExprKind::TailCall(_) => {
                                    // For TailCall expressions, check if already processed
                                    if self.processed_tail_calls.contains(&completion_expr.hir_id) {
                                        // Already processed - create a simple assignment
                                        let assignment_stmt = self.create_assignment_stmt(ctx, temp_name, completion_expr, span);
                                        new_arm_block.stmts.push(assignment_stmt);
                                    } else {
                                        // Handle TailCall with special care to avoid infinite loops
                                        let assignment_stmt = self.create_assignment_stmt(ctx, temp_name, completion_expr, span);
                                        new_arm_block.stmts.push(assignment_stmt);
                                        // Don't mark as processed here since we're not in function completion context
                                    }
                                }
                                hir::ExprKind::IfElse(..) | hir::ExprKind::Loop(..) | hir::ExprKind::Match(..) | hir::ExprKind::Block(..) => {
                                    // Recursively flatten nested complex expressions
                                    let (flattened_expr, temp_stmts) = self.flatten_expression_to_temp_var(completion_expr, ctx);
                                    new_arm_block.stmts.extend(temp_stmts);
                                    let assignment_stmt = self.create_assignment_stmt(ctx, temp_name, flattened_expr, span);
                                    new_arm_block.stmts.push(assignment_stmt);
                                }
                                _ => {
                                    // For simple expressions, create direct assignment
                                    let assignment_stmt = self.create_assignment_stmt(ctx, temp_name, completion_expr, span);
                                    new_arm_block.stmts.push(assignment_stmt);
                                }
                            }
                        }
                    }
                    
                    new_arms.push(hir::MatchArm {
                        pat: arm.pat.clone(),
                        guard: arm.guard.clone(),
                        block: new_arm_block,
                        leading_comments: arm.leading_comments.clone(),
                        trailing_comments: arm.trailing_comments.clone(),
                    });
                }
                
                // Add the match statement directly
                statements.push(hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Match(
                            Box::new(flattened_scrutinee),
                            new_arms,
                        ),
                        span,
                    })),
                    span,
                ));
            }
            hir::ExprKind::Loop(loop_block) => {
                // Transform loop expression to regular statements
                let mut new_loop_block = loop_block.as_ref().clone();
                
                // FIRST: Recursively apply HIR JS pass to all expressions in the loop block
                // This ensures nested complex expressions (match, block, etc.) are transformed
                if let Some(function_name) = function_context {
                    self.visit_block_with_function_context(&mut new_loop_block, ctx, Some(function_name));
                } else {
                    self.visit_block(&mut new_loop_block, ctx);
                }
                
                // THEN: Transform break statements in the loop to assign to the temp variable
                self.transform_break_statements_in_block(&mut new_loop_block, temp_name, ctx);
                
                // Add the loop as a regular statement
                statements.push(hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Loop(Box::new(new_loop_block)),
                        span,
                    })),
                    span,
                ));
            }
            hir::ExprKind::Call(_call_expr) => {
                // For call expressions, we need to flatten the arguments first
                let (flattened_call_expr, mut temp_stmts) =
                    self.flatten_subexpressions_generically(expr, ctx);

                // Add any statements from flattening the arguments
                statements.append(&mut temp_stmts);

                // Now create the assignment with the flattened call expression
                if use_return_statements {
                    let return_stmt = hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Return(Some(Box::new(flattened_call_expr))),
                        span,
                    );
                    statements.push(return_stmt);
                } else {
                    statements.push(self.create_assignment_stmt(
                        ctx,
                        temp_name,
                        flattened_call_expr,
                        span,
                    ));
                }
            }
            hir::ExprKind::TailCall(_call_expr) => {
                // For tail call expressions, treat them differently based on context
                if use_return_statements {
                    // In function completion contexts, preserve TailCall as-is and mark as processed
                    // Mark this TailCall as processed to avoid re-processing in future iterations
                    self.processed_tail_calls.insert(expr.hir_id);
                    
                    let return_stmt = hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Return(Some(Box::new(expr))),
                        span,
                    );
                    statements.push(return_stmt);
                } else {
                    // For non-function completion contexts, check if already processed
                    if self.processed_tail_calls.contains(&expr.hir_id) {
                        // Already processed - create a simple assignment to avoid infinite loops
                        statements.push(self.create_assignment_stmt(ctx, temp_name, expr, span));
                    } else {
                        // Flatten the arguments but preserve the TailCall structure
                        let (flattened_call_expr, mut temp_stmts) =
                            self.flatten_subexpressions_generically(expr, ctx);

                        // Add any statements from flattening the arguments
                        statements.append(&mut temp_stmts);

                        statements.push(self.create_assignment_stmt(
                            ctx,
                            temp_name,
                            flattened_call_expr,
                            span,
                        ));
                    }
                }
            }
            hir::ExprKind::Binary(op_kind, lhs, rhs) => {
                // Handle short-circuit operators specially
                match op_kind {
                    hir::BinaryOpKind::Or => {
                        // Transform a || b into: if (a) { temp = a; } else { temp = b; }
                        // This preserves short-circuit semantics

                        // First, flatten the operands' subexpressions if needed
                        let (flattened_lhs, mut lhs_stmts) = if !expr_can_render_as_js_expr(lhs) {
                            self.flatten_expression_to_temp_var(*lhs.clone(), ctx)
                        } else {
                            let (flattened, stmts) =
                                self.flatten_subexpressions_generically(*lhs.clone(), ctx);
                            (flattened, stmts)
                        };
                        statements.append(&mut lhs_stmts);

                        let (flattened_rhs, mut rhs_stmts) = if !expr_can_render_as_js_expr(rhs) {
                            self.flatten_expression_to_temp_var(*rhs.clone(), ctx)
                        } else {
                            let (flattened, stmts) =
                                self.flatten_subexpressions_generically(*rhs.clone(), ctx);
                            (flattened, stmts)
                        };

                        // Create the if-else structure that preserves short-circuit semantics
                        let then_block = hir::Block::new(
                            ctx.hir_id_allocator.next_id(),
                            {
                                let mut then_stmts = Vec::new();
                                then_stmts.append(&mut lhs_stmts.clone());
                                then_stmts.push(self.create_assignment_stmt(
                                    ctx,
                                    temp_name,
                                    flattened_lhs.clone(),
                                    span,
                                ));
                                then_stmts
                            },
                            None,
                            span,
                        );

                        let else_block = hir::Block::new(
                            ctx.hir_id_allocator.next_id(),
                            {
                                let mut else_stmts = Vec::new();
                                else_stmts.append(&mut rhs_stmts);
                                else_stmts.push(self.create_assignment_stmt(
                                    ctx,
                                    temp_name,
                                    flattened_rhs,
                                    span,
                                ));
                                else_stmts
                            },
                            None,
                            span,
                        );

                        let if_else_expr = hir::Expr {
                            hir_id: ctx.hir_id_allocator.next_id(),
                            kind: hir::ExprKind::IfElse(
                                Box::new(flattened_lhs.clone()),
                                Box::new(then_block),
                                vec![hir::ElseClause {
                                    condition: None,
                                    consequence: else_block,
                                }],
                            ),
                            span,
                        };

                        statements.push(hir::Stmt::new(
                            ctx.hir_id_allocator.next_id(),
                            hir::StmtKind::Expr(Box::new(if_else_expr)),
                            span,
                        ));
                    }
                    hir::BinaryOpKind::And => {
                        // Transform a && b into: if (a) { temp = b; } else { temp = a; }
                        // This preserves short-circuit semantics

                        // First, flatten the operands' subexpressions if needed
                        let (flattened_lhs, mut lhs_stmts) = if !expr_can_render_as_js_expr(lhs) {
                            self.flatten_expression_to_temp_var(*lhs.clone(), ctx)
                        } else {
                            let (flattened, stmts) =
                                self.flatten_subexpressions_generically(*lhs.clone(), ctx);
                            (flattened, stmts)
                        };
                        statements.append(&mut lhs_stmts);

                        let (flattened_rhs, mut rhs_stmts) = if !expr_can_render_as_js_expr(rhs) {
                            self.flatten_expression_to_temp_var(*rhs.clone(), ctx)
                        } else {
                            let (flattened, stmts) =
                                self.flatten_subexpressions_generically(*rhs.clone(), ctx);
                            (flattened, stmts)
                        };

                        let then_block = hir::Block::new(
                            ctx.hir_id_allocator.next_id(),
                            {
                                let mut then_stmts = Vec::new();
                                then_stmts.append(&mut rhs_stmts);
                                then_stmts.push(self.create_assignment_stmt(
                                    ctx,
                                    temp_name,
                                    flattened_rhs,
                                    span,
                                ));
                                then_stmts
                            },
                            None,
                            span,
                        );

                        let else_block = hir::Block::new(
                            ctx.hir_id_allocator.next_id(),
                            {
                                let mut else_stmts = Vec::new();
                                else_stmts.append(&mut lhs_stmts.clone());
                                else_stmts.push(self.create_assignment_stmt(
                                    ctx,
                                    temp_name,
                                    flattened_lhs.clone(),
                                    span,
                                ));
                                else_stmts
                            },
                            None,
                            span,
                        );

                        let if_else_expr = hir::Expr {
                            hir_id: ctx.hir_id_allocator.next_id(),
                            kind: hir::ExprKind::IfElse(
                                Box::new(flattened_lhs),
                                Box::new(then_block),
                                vec![hir::ElseClause {
                                    condition: None,
                                    consequence: else_block,
                                }],
                            ),
                            span,
                        };

                        statements.push(hir::Stmt::new(
                            ctx.hir_id_allocator.next_id(),
                            hir::StmtKind::Expr(Box::new(if_else_expr)),
                            span,
                        ));
                    }
                    _ => {
                        // For other binary operators, create a simple assignment after flattening operands
                        // First, flatten the operands if needed
                        let (flattened_lhs, mut lhs_stmts) = if !expr_can_render_as_js_expr(lhs) {
                            self.flatten_expression_to_temp_var(*lhs.clone(), ctx)
                        } else {
                            let (flattened, stmts) =
                                self.flatten_subexpressions_generically(*lhs.clone(), ctx);
                            (flattened, stmts)
                        };

                        let (flattened_rhs, mut rhs_stmts) = if !expr_can_render_as_js_expr(rhs) {
                            self.flatten_expression_to_temp_var(*rhs.clone(), ctx)
                        } else {
                            let (flattened, stmts) =
                                self.flatten_subexpressions_generically(*rhs.clone(), ctx);
                            (flattened, stmts)
                        };

                        // Add the operand flattening statements
                        statements.append(&mut lhs_stmts);
                        statements.append(&mut rhs_stmts);

                        // Create the binary expression with flattened operands
                        let binary_expr = hir::Expr {
                            hir_id: ctx.hir_id_allocator.next_id(),
                            kind: hir::ExprKind::Binary(
                                *op_kind,
                                Box::new(flattened_lhs),
                                Box::new(flattened_rhs),
                            ),
                            span,
                        };

                        // Assign the result to the temporary variable
                        statements.push(self.create_assignment_stmt(
                            ctx,
                            temp_name,
                            binary_expr,
                            span,
                        ));
                    }
                }
            }
            hir::ExprKind::Break(Some(break_expr)) => {
                // Transform break expressions with values to: temp_var = value; break;
                // First create the assignment for the break value
                let assignment_stmt = self.create_assignment_stmt(ctx, temp_name, *break_expr.clone(), span);
                statements.push(assignment_stmt);
                
                // Then create a plain break statement
                let plain_break_stmt = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Break(None),
                        span,
                    })),
                    span,
                );
                statements.push(plain_break_stmt);
            }
            hir::ExprKind::Break(None) => {
                // Transform plain break expressions to just a break statement
                // No temp variable assignment needed since there's no value
                let plain_break_stmt = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Break(None),
                        span,
                    })),
                    span,
                );
                statements.push(plain_break_stmt);
            }
            hir::ExprKind::Continue => {
                // Transform continue expressions to just a continue statement
                // No temp variable assignment needed since continue has no value
                let continue_stmt = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Continue,
                        span,
                    })),
                    span,
                );
                statements.push(continue_stmt);
            }
            _ => {
                // For other expressions, create assignment or return statement
                if use_return_statements {
                    let return_stmt = hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Return(Some(Box::new(expr))),
                        span,
                    );
                    statements.push(return_stmt);
                } else {
                    statements.push(self.create_assignment_stmt(ctx, temp_name, expr, span));
                }
            }
        }

        statements
    }

    /// Create statements for function completion expressions using direct return statements
    /// This is used instead of temp variables when we're in a function completion position
    fn create_return_statements_for_expr(
        &mut self,
        ctx: &mut HirOptContext,
        expr: hir::Expr,
        span: Span,
    ) -> Vec<hir::Stmt> {
        // Use the internal method with return statements enabled and function context
        self.create_temp_var_assignment_for_expr_internal(ctx, "", expr, span, true, Some("function"))
    }
}

/// Check if an if-else expression can be rendered as a ternary operator in JavaScript
pub fn if_else_can_render_as_ternary(
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

/// Check if an if-else can be rendered as JavaScript if-statements
/// This is more permissive than ternary rendering and allows statements in branches
pub fn if_else_can_render_as_js_statements(
    expr: &hir::Expr,
    then_branch: &hir::Block,
    else_branches: &[hir::ElseClause],
) -> bool {
    // The condition must be a simple expression
    expr_can_render_as_js_expr(expr)
        // All statements in branches must be simple
        && then_branch.stmts.iter().all(|stmt| stmt_can_render_as_js(stmt))
        // The completion expression (if any) must be simple
        && then_branch.expr.as_ref().map_or(true, |e| expr_can_render_as_js_expr(e))
        // All else branches must be simple
        && else_branches.iter().all(|else_branch| {
            // Else condition (if any) must be simple
            (else_branch.condition.is_none()
                || expr_can_render_as_js_expr(else_branch.condition.as_ref().unwrap()))
                // All statements in else branch must be simple
                && else_branch.consequence.stmts.iter().all(|stmt| stmt_can_render_as_js(stmt))
                // The completion expression (if any) must be simple
                && else_branch.consequence.expr.as_ref().map_or(true, |e| expr_can_render_as_js_expr(e))
        })
}

/// Check if a statement can be rendered as JavaScript without special HIR flattening
fn stmt_can_render_as_js(stmt: &hir::Stmt) -> bool {
    match &stmt.kind {
        hir::StmtKind::Let(_, expr, _) => expr_can_render_as_js_expr(expr),
        hir::StmtKind::Return(None) => true,
        hir::StmtKind::Return(Some(expr)) => expr_can_render_as_js_expr(expr),
        hir::StmtKind::Expr(expr) => expr_can_render_as_js_expr(expr),
        hir::StmtKind::FunctionDeclaration(_) => true,
        hir::StmtKind::DynFunctionDeclaration(_) => true,
        hir::StmtKind::EnumDeclaration(_) => true,
        hir::StmtKind::StructDeclaration(_) => true,
    }
}

/// Check if an expression can be rendered as a JavaScript expression (vs statement)
pub fn expr_can_render_as_js_expr(expr: &hir::Expr) -> bool {
    match &expr.kind {
        hir::ExprKind::Path(..) => true,
        hir::ExprKind::Let(..) => false,
        hir::ExprKind::Literal(..) => true,
        hir::ExprKind::Binary(_, lhs, rhs) => {
            expr_can_render_as_js_expr(lhs) && expr_can_render_as_js_expr(rhs)
        }
        hir::ExprKind::Unary(_, expr) => expr_can_render_as_js_expr(expr),
        hir::ExprKind::Block(..) => false,
        hir::ExprKind::Loop(..) => false,
        hir::ExprKind::Break(..) => false,
        hir::ExprKind::Continue => false,
        hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
            // If-else can be rendered as a JS expression only if:
            // 1. It has only one else clause (no if-else-if)
            // 2. It can be a ternary operator
            else_branches.len() == 1 
                && if_else_can_render_as_ternary(condition, then_branch, else_branches)
        }
        hir::ExprKind::Match(..) => false,
        hir::ExprKind::Call(call_expr) => {
            call_expr.arguments.iter().all(expr_can_render_as_js_expr)
        }
        hir::ExprKind::Cast(expr, _) => expr_can_render_as_js_expr(expr),
        hir::ExprKind::FieldAccess(base, _) => expr_can_render_as_js_expr(base),
        hir::ExprKind::IndexAccess(base, index) => {
            expr_can_render_as_js_expr(base) && expr_can_render_as_js_expr(index)
        }
        hir::ExprKind::TailCall(_) => false,
        hir::ExprKind::List(exprs) => exprs.iter().all(expr_can_render_as_js_expr),
        hir::ExprKind::Dict(exprs) => exprs
            .iter()
            .all(|kv| expr_can_render_as_js_expr(&kv.0) && expr_can_render_as_js_expr(&kv.1)),
        hir::ExprKind::FunctionExpression(..) => true,
        hir::ExprKind::Range(..) => true,
        hir::ExprKind::Wildcard => true,
    }
}

impl HirPass for HirJsPass {
    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.iteration_counter += 1;
        self.changes_made = false;
        
        // Don't reset processed_tail_calls - let it persist across iterations
        // This prevents TailCall expressions from being re-processed
        
        self.visit_module(module, ctx);
        self.changes_made
    }
}

impl<'hir> Visitor<'hir> for HirJsPass {
    type Context = HirOptContext;

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut Self::Context) {
        // We need to visit expressions to handle nested complex expressions
        // The key is to transform bottom-up (children first, then parent)
        match &mut expr.kind {
            hir::ExprKind::Block(..) | hir::ExprKind::IfElse(..) | hir::ExprKind::Loop(..) | hir::ExprKind::Match(..) => {
                // For complex expressions, first visit their children
                visit::walk_expr(self, expr, ctx);
                // Note: the actual transformation happens in visit_block when these are in expression positions
            }
            _ => {
                // For other expressions, continue walking
                visit::walk_expr(self, expr, ctx);
            }
        }
    }

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, ctx: &mut Self::Context) {
        // Check if this is a return statement with a TailCall - if so, don't process the TailCall further
        match &mut stmt.kind {
            hir::StmtKind::Return(Some(expr)) => {
                match &expr.kind {
                    hir::ExprKind::TailCall(_) => {
                        // Don't walk into TailCall expressions in return statements to preserve them
                        return;
                    }
                    _ => {
                        // For other expressions in return statements, continue normal processing
                        visit::walk_stmt(self, stmt, ctx);
                    }
                }
            }
            hir::StmtKind::FunctionDeclaration(func_decl) => {
                // Handle function declarations by providing function context to block visitor
                let function_name = crate::function_generator::fn_identifier_to_string(&func_decl.name);
                
                // Visit function metadata first
                self.visit_expr(&mut func_decl.name, ctx);
                self.enter_scope(func_decl.hir_id, ctx);
                
                for param in &mut func_decl.parameters {
                    self.visit_ident(&mut param.name, ctx);
                    self.visit_ty(&mut param.type_annotation, ctx);
                }
                
                // Visit the function body with function context
                self.visit_block_with_function_context(&mut func_decl.body, ctx, Some(&function_name));
                self.leave_scope(func_decl.hir_id, ctx);
                
                // Don't call walk_stmt to avoid double-processing
            }
            hir::StmtKind::DynFunctionDeclaration(_func_decl) => {
                // DynFunctionDeclaration doesn't have a body to process, it's just metadata
                // No need to process it specially
                visit::walk_stmt(self, stmt, ctx);
            }
            _ => {
                // Always walk other statements to ensure nested expressions are visited
                visit::walk_stmt(self, stmt, ctx);
            }
        }
    }

    fn visit_block(&mut self, block: &'hir mut hir::Block, ctx: &mut Self::Context) {
        // Call the helper method with no function context
        self.visit_block_with_function_context(block, ctx, None);
    }
}

impl HirJsPass {
    /// Visit a block with optional function context for tail call detection  
    fn visit_block_with_function_context(&mut self, block: &mut hir::Block, ctx: &mut HirOptContext, function_name: Option<&str>) {
        // FIRST: Handle block completion expressions - move loops to statements before processing
        // Visit the block expression if it exists
        if let Some(expr) = &mut block.expr {
            match &expr.kind {
                // Loops in block completion position should be transformed like other expressions
                hir::ExprKind::Loop(_) => {
                    // Transform loop expressions using the standard flattening approach
                    let span = expr.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(expr, placeholder);
                    let (flattened_expr, temp_stmts) =
                        self.flatten_subexpressions_generically(expr_to_flatten, ctx);
                    *expr = flattened_expr;

                    // If we generated statements for the loop expression, add them to the block
                    if !temp_stmts.is_empty() {
                        block.stmts.extend(temp_stmts);
                        self.changes_made = true;
                    }
                }
                // Match expressions in block completion position should be transformed to statements
                hir::ExprKind::Match(..) => {
                    let span = expr.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(expr, placeholder);
                    
                    // Check if we're in a function completion position - use return statements instead of temp variables
                    let is_function_completion = function_name.is_some();
                    
                    let temp_stmts = if is_function_completion {
                        // Use direct return statements for function completion
                        self.create_return_statements_for_expr(ctx, expr_to_flatten, span)
                    } else {
                        // Use temp variables for other completion positions
                        let (flattened_expr, stmts) = self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                        *expr = flattened_expr;
                        stmts
                    };

                    // If we generated statements for the match expression, add them to the block
                    if !temp_stmts.is_empty() {
                        block.stmts.extend(temp_stmts);
                        self.changes_made = true;
                        
                        // If we used return statements, clear the completion expression
                        if is_function_completion {
                            block.expr = None;
                        }
                    }
                }
                // If-else expressions that cannot be rendered as JavaScript expressions should be transformed
                hir::ExprKind::IfElse(_condition, _then_branch, else_branches) => {
                    // Only transform if-else-if expressions (multiple else branches) as they cannot be ternaries
                    // Simple if-else expressions should be handled by the JavaScript generator if possible
                    if else_branches.len() > 1 {
                        let span = expr.span;
                        let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                        let expr_to_flatten = std::mem::replace(expr, placeholder);
                        
                        // Check if we're in a function completion position - use return statements instead of temp variables
                        let is_function_completion = function_name.is_some();
                        
                        let temp_stmts = if is_function_completion {
                            // Use direct return statements for function completion
                            self.create_return_statements_for_expr(ctx, expr_to_flatten, span)
                        } else {
                            // Use temp variables for other completion positions
                            let (flattened_expr, stmts) = self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                            *expr = flattened_expr;
                            stmts
                        };

                        // If we generated statements for the if-else expression, add them to the block
                        if !temp_stmts.is_empty() {
                            block.stmts.extend(temp_stmts);
                            self.changes_made = true;
                            
                            // If we used return statements, clear the completion expression
                            if is_function_completion {
                                block.expr = None;
                            }
                        }
                    } else {
                        // For simple if-else expressions, check if they're in tail call position
                        if self.is_in_tail_call_position_stateless(expr, function_name) {
                            // If-else in tail call position should be transformed to use return statements
                            // while preserving any TailCall expressions within the branches
                            let span = expr.span;
                            let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                            let expr_to_flatten = std::mem::replace(expr, placeholder);
                            
                            // Check if we're in a function completion position - use return statements
                            let is_function_completion = function_name.is_some();
                            
                            if is_function_completion {
                                // Use direct return statements for function completion
                                let temp_stmts = self.create_return_statements_for_expr(ctx, expr_to_flatten, span);
                                
                                // If we generated statements for the if-else expression, add them to the block
                                if !temp_stmts.is_empty() {
                                    block.stmts.extend(temp_stmts);
                                    self.changes_made = true;
                                    
                                    // Clear the completion expression since we moved it to statements
                                    block.expr = None;
                                }
                            } else {
                                // Not in function completion, use regular flattening
                                let (flattened_expr, temp_stmts) = self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                                *expr = flattened_expr;
                                
                                if !temp_stmts.is_empty() {
                                    block.stmts.extend(temp_stmts);
                                    self.changes_made = true;
                                }
                            }
                        } else {
                            // Continue to regular processing by falling through to the catch-all case
                            // We need to explicitly handle this case or it will be handled by the catch-all
                            // Check if this expression can be rendered as JS, and if not, flatten it
                            if !expr_can_render_as_js_expr(expr) {
                                let span = expr.span;
                                let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                                let expr_to_flatten = std::mem::replace(expr, placeholder);
                                let (flattened_expr, temp_stmts) =
                                    self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                                *expr = flattened_expr;

                                // If we generated statements for the expression, add them to the block
                                if !temp_stmts.is_empty() {
                                    block.stmts.extend(temp_stmts);
                                    self.changes_made = true;
                                }
                            } else {
                                // Process subexpressions but don't flatten the main expression
                                let span = expr.span;
                                let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                                let expr_to_flatten = std::mem::replace(expr, placeholder);
                                let (flattened_expr, temp_stmts) =
                                    self.flatten_subexpressions_generically(expr_to_flatten, ctx);
                                *expr = flattened_expr;

                                // If we generated statements for subexpressions, add them to the block
                                if !temp_stmts.is_empty() {
                                    block.stmts.extend(temp_stmts);
                                    self.changes_made = true;
                                }
                            }
                        }
                    }
                }
                // Break expressions in block completion position should be transformed
                hir::ExprKind::Break(..) => {
                    // Transform break expressions using the specialized flattening approach
                    let span = expr.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(expr, placeholder);
                    let (flattened_expr, temp_stmts) =
                        self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    *expr = flattened_expr;

                    // If we generated statements for the break expression, add them to the block
                    if !temp_stmts.is_empty() {
                        block.stmts.extend(temp_stmts);
                        self.changes_made = true;
                    }
                }
                // Continue expressions in block completion position should be transformed
                hir::ExprKind::Continue => {
                    // Transform continue expressions using the specialized flattening approach
                    let span = expr.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(expr, placeholder);
                    let (flattened_expr, temp_stmts) =
                        self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    *expr = flattened_expr;

                    // If we generated statements for the continue expression, add them to the block
                    if !temp_stmts.is_empty() {
                        block.stmts.extend(temp_stmts);
                        self.changes_made = true;
                    }
                }
                hir::ExprKind::Block(inner_block) => {
                    // For block completion expressions, check if they contain loops that need special handling
                    if let Some(ref inner_expr) = inner_block.expr {
                        if matches!(inner_expr.kind, hir::ExprKind::Loop(_)) {
                            // This block contains a loop completion expression
                            // Move the entire block to statements so the loop can be handled properly
                            let completion_expr = std::mem::replace(
                                expr,
                                hir::Expr {
                                    hir_id: ctx.hir_id_allocator.next_id(),
                                    kind: hir::ExprKind::Literal(Box::new(
                                        tlang_ast::token::Literal::String("()".into()),
                                    )),
                                    span: expr.span,
                                },
                            );

                            // Convert the completion expression to a statement
                            let completion_stmt = hir::Stmt::new(
                                ctx.hir_id_allocator.next_id(),
                                hir::StmtKind::Expr(Box::new(completion_expr)),
                                expr.span,
                            );

                            // Add it as the last statement
                            block.stmts.push(completion_stmt);

                            // Clear the block expression since we moved it to statements
                            block.expr = None;

                            self.changes_made = true;
                        } else {
                            // Regular block without loop - handle with generic flattening
                            let span = expr.span;
                            let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                            let expr_to_flatten = std::mem::replace(expr, placeholder);
                            let (flattened_expr, temp_stmts) =
                                self.flatten_subexpressions_generically(expr_to_flatten, ctx);
                            *expr = flattened_expr;

                            // If we generated statements for the block expression, add them to the block
                            if !temp_stmts.is_empty() {
                                block.stmts.extend(temp_stmts);
                                self.changes_made = true;
                            }
                        }
                    } else {
                        // Block without completion expression - move to statements
                        let completion_expr = std::mem::replace(
                            expr,
                            hir::Expr {
                                hir_id: ctx.hir_id_allocator.next_id(),
                                kind: hir::ExprKind::Literal(Box::new(
                                    tlang_ast::token::Literal::String("()".into()),
                                )),
                                span: expr.span,
                            },
                        );

                        // Convert the completion expression to a statement
                        let completion_stmt = hir::Stmt::new(
                            ctx.hir_id_allocator.next_id(),
                            hir::StmtKind::Expr(Box::new(completion_expr)),
                            expr.span,
                        );

                        // Add it as the last statement
                        block.stmts.push(completion_stmt);

                        // Clear the block expression since we moved it to statements
                        block.expr = None;

                        self.changes_made = true;
                    }
                }
                _ => {
                    // For other expressions, check if they need flattening but avoid loop transformation
                    // SPECIAL CASE: Handle TailCall expressions directly
                    if let hir::ExprKind::TailCall(_) = &expr.kind {
                        // TailCall expressions should be converted to return statements in function completion position
                        // Check if already processed to avoid infinite loops
                        if self.processed_tail_calls.contains(&expr.hir_id) {
                            // Skip processing - this TailCall has already been converted
                            return;
                        }
                        
                        // Check if we're in a function completion position
                        let is_function_completion = function_name.is_some();
                        
                        if is_function_completion {
                            // TailCall in function completion position - convert to return statement
                            let span = expr.span;
                            
                            // Mark this TailCall as processed to avoid re-processing
                            self.processed_tail_calls.insert(expr.hir_id);
                            
                            let return_stmt = hir::Stmt::new(
                                ctx.hir_id_allocator.next_id(),
                                hir::StmtKind::Return(Some(Box::new(expr.clone()))),
                                span,
                            );
                            
                            // Add the return statement to the block
                            block.stmts.push(return_stmt);
                            
                            // Clear the completion expression since we moved it to statements
                            block.expr = None;
                            
                            self.changes_made = true;
                        }
                        // For non-function completion contexts, leave TailCall as-is
                        // The JavaScript generator will handle it appropriately
                    } else if self.is_in_tail_call_position_stateless(expr, function_name) {
                        // This expression contains tail calls and is in tail position
                        // Don't flatten it to preserve tail call optimization
                        // But still process subexpressions that are not in tail position
                        let span = expr.span;
                        let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                        let expr_to_flatten = std::mem::replace(expr, placeholder);
                        let (flattened_expr, temp_stmts) =
                            self.flatten_subexpressions_generically(expr_to_flatten, ctx);
                        *expr = flattened_expr;

                        // If we generated statements for subexpressions, add them to the block
                        if !temp_stmts.is_empty() {
                            block.stmts.extend(temp_stmts);
                            self.changes_made = true;
                        }
                    } else if !expr_can_render_as_js_expr(expr) {
                        let span = expr.span;
                        let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                        let expr_to_flatten = std::mem::replace(expr, placeholder);
                        
                        // Check if we're in a function completion position - use return statements instead of temp variables
                        let is_function_completion = function_name.is_some();
                        
                        let temp_stmts = if is_function_completion {
                            // Use direct return statements for function completion
                            self.create_return_statements_for_expr(ctx, expr_to_flatten, span)
                        } else {
                            // Use temp variables for other completion positions
                            let (flattened_expr, stmts) = self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                            *expr = flattened_expr;
                            stmts
                        };

                        // If we generated statements for the expression, add them to the block
                        if !temp_stmts.is_empty() {
                            block.stmts.extend(temp_stmts);
                            self.changes_made = true;
                            
                            // If we used return statements, clear the completion expression
                            if is_function_completion {
                                block.expr = None;
                            }
                        }
                    } else {
                        // Expression can be rendered as JS, but still process subexpressions
                        let span = expr.span;
                        let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                        let expr_to_flatten = std::mem::replace(expr, placeholder);
                        let (flattened_expr, temp_stmts) =
                            self.flatten_subexpressions_generically(expr_to_flatten, ctx);
                        *expr = flattened_expr;

                        // If we generated statements for subexpressions, add them to the block
                        if !temp_stmts.is_empty() {
                            block.stmts.extend(temp_stmts);
                            self.changes_made = true;
                        }
                    }
                }
            }
        }

        // Handle simple completion expressions in function context
        // Convert them to return statements if we're in a function completion position
        if let Some(expr) = &mut block.expr {
            if let Some(_function_name) = function_name {
                // If we're in a function context and we still have a completion expression,
                // it means it's a simple expression that can be rendered as JS
                // Convert it to a return statement
                let completion_expr = std::mem::replace(
                    expr,
                    hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Literal(Box::new(
                            tlang_ast::token::Literal::String("()".into()),
                        )),
                        span: expr.span,
                    },
                );

                // Create a return statement
                let return_stmt = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Return(Some(Box::new(completion_expr))),
                    expr.span,
                );

                // Add the return statement and clear the completion expression
                block.stmts.push(return_stmt);
                block.expr = None;
                self.changes_made = true;
            }
        }

        // SECOND: Transform all statements using the generic flattening approach
        let mut new_stmts = Vec::new();

        for stmt in &mut block.stmts {
            // Apply generic flattening to ANY statement that might contain complex expressions
            let (modified_stmt, mut temp_stmts) =
                self.flatten_statement_expressions(stmt.clone(), ctx);

            // Add any temporary variable statements first
            new_stmts.append(&mut temp_stmts);

            // Add the modified statement
            new_stmts.push(modified_stmt);
        }

        // Replace the statements and handle nested visiting properly
        block.stmts = new_stmts;
        
        // After transformation, we need to visit nested structures in all statements
        // This ensures that nested loops, blocks, and other complex expressions are processed
        for stmt in &mut block.stmts {
            match &mut stmt.kind {
                // Visit function declarations to handle their nested content
                hir::StmtKind::FunctionDeclaration(_) => {
                    self.visit_stmt(stmt, ctx);
                }
                // Visit expression statements that might contain nested loops or blocks
                hir::StmtKind::Expr(expr) => {
                    match &expr.kind {
                        // Visit loop expressions to ensure their bodies are processed
                        hir::ExprKind::Loop(_) => {
                            self.visit_expr(expr, ctx);
                        }
                        // Visit block expressions to ensure their contents are processed
                        hir::ExprKind::Block(_) => {
                            self.visit_expr(expr, ctx);
                        }
                        // Visit other complex expressions that might contain nested structures
                        hir::ExprKind::IfElse(..) | hir::ExprKind::Match(..) => {
                            self.visit_expr(expr, ctx);
                        }
                        _ => {
                            // For other expressions, no nested visiting needed
                        }
                    }
                }
                // Let statements have already been transformed, but they might contain
                // nested complex expressions in the pattern or value
                hir::StmtKind::Let(_, expr, _) => {
                    match &expr.kind {
                        hir::ExprKind::Loop(_) | hir::ExprKind::Block(_) | hir::ExprKind::IfElse(..) | hir::ExprKind::Match(..) => {
                            self.visit_expr(expr, ctx);
                        }
                        _ => {}
                    }
                }
                // Skip other statement types to avoid unnecessary processing
                _ => {}
            }
        }
    }
}

impl HirJsPass {
    /// Check if an expression contains tail calls to the specified function
    fn is_expr_tail_recursive(&self, function_name: &str, node: &hir::Expr) -> bool {
        // Recursively traverse nodes to check for tail recursive calls to the function itself.
        // We currently only support tail recursion to the function itself, not any other function.
        // Therefore we look for RecursiveCall nodes which reference the current function name.
        match &node.kind {
            hir::ExprKind::TailCall(call_expr) => {
                // If the function is an identifier, check if it's the same as the current function name.
                if let hir::ExprKind::Path(_) = &call_expr.callee.kind {
                    let callee_name = fn_identifier_to_string(&call_expr.callee);
                    if callee_name == function_name {
                        return true;
                    } else {
                        return false;
                    }
                }
                false
            }
            hir::ExprKind::Block(block) => self.is_block_tail_recursive(function_name, block),
            hir::ExprKind::Match(_, arms, ..) => arms
                .iter()
                .any(|arm| self.is_block_tail_recursive(function_name, &arm.block)),
            hir::ExprKind::IfElse(_expr, then_branch, else_branches) => {
                // Note: we don't check the condition expression for tail calls as they are not in tail position
                (if then_branch.expr.is_some() {
                    self.is_expr_tail_recursive(
                        function_name,
                        then_branch.expr.as_ref().unwrap(),
                    )
                } else if let Some(stmt) = then_branch.stmts.last() {
                    self.is_stmt_tail_recursive(function_name, stmt)
                } else {
                    false
                })
                || else_branches.iter().any(|else_clause| {
                    if else_clause.consequence.expr.is_some() {
                        self.is_expr_tail_recursive(
                            function_name,
                            else_clause.consequence.expr.as_ref().unwrap(),
                        )
                    } else if let Some(stmt) = else_clause.consequence.stmts.last() {
                        self.is_stmt_tail_recursive(function_name, stmt)
                    } else {
                        false
                    }
                })
            }
            _ => false,
        }
    }

    /// Check if a statement contains tail calls to the specified function
    fn is_stmt_tail_recursive(&self, function_name: &str, stmt: &hir::Stmt) -> bool {
        match &stmt.kind {
            hir::StmtKind::Expr(expr) => self.is_expr_tail_recursive(function_name, expr),
            hir::StmtKind::Return(expr) => {
                if let Some(expr) = expr.as_ref() {
                    self.is_expr_tail_recursive(function_name, expr)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Check if a block contains tail calls to the specified function
    fn is_block_tail_recursive(&self, function_name: &str, block: &hir::Block) -> bool {
        for statement in &block.stmts {
            if self.is_stmt_tail_recursive(function_name, statement) {
                return true;
            }
        }

        if let Some(ref expression) = block.expr {
            return self.is_expr_tail_recursive(function_name, expression);
        }

        false
    }

    /// Check if we're currently processing a function that could have tail calls
    fn is_in_tail_call_position_stateless(&self, expr: &hir::Expr, current_function_name: Option<&str>) -> bool {
        if let Some(function_name) = current_function_name {
            self.is_expr_tail_recursive(function_name, expr)
        } else {
            false
        }
    }
}
