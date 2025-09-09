use tlang_ast::node::Ident;
use tlang_hir::{Visitor, hir, visit::walk_expr};
use tlang_hir_opt::{HirOptContext, HirPass};
use tlang_span::Span;

use crate::js_expr_utils::{
    expr_can_render_as_assignment_rhs, expr_can_render_as_js_expr, expr_can_render_as_js_stmt,
};

/// A simplified HIR JS pass that focuses on flattening expressions that can't be represented in JavaScript
/// This pass assumes that return statements have already been handled by ReturnStatementPass
#[derive(Debug, Default)]
pub struct SimplifiedHirJsPass {
    temp_var_counter: usize,
    changes_made: bool,
}

impl SimplifiedHirJsPass {
    pub fn new() -> Self {
        Self {
            temp_var_counter: 0,
            changes_made: false,
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

        let assignment_expr = hir::Expr {
            hir_id: ctx.hir_id_allocator.next_id(),
            kind: hir::ExprKind::Binary(
                hir::BinaryOpKind::Assign,
                Box::new(temp_path),
                Box::new(value_expr),
            ),
            span,
        };

        hir::Stmt::new(hir_id, hir::StmtKind::Expr(Box::new(assignment_expr)), span)
    }

    fn create_temp_var_declaration(
        &mut self,
        ctx: &mut HirOptContext,
        temp_name: &str,
        span: Span,
    ) -> hir::Stmt {
        let hir_id = ctx.hir_id_allocator.next_id();
        let ident = Ident::new(temp_name, span);
        let pattern = hir::Pat {
            kind: hir::PatKind::Identifier(ctx.hir_id_allocator.next_id(), Box::new(ident)),
            span,
        };

        // Create wildcard expression for uninitialized value
        let wildcard_expr = hir::Expr {
            hir_id: ctx.hir_id_allocator.next_id(),
            kind: hir::ExprKind::Wildcard,
            span,
        };

        // Create a default type annotation
        let default_ty = hir::Ty {
            kind: hir::TyKind::Unknown,
            span,
        };

        hir::Stmt::new(
            hir_id,
            hir::StmtKind::Let(
                Box::new(pattern),
                Box::new(wildcard_expr),
                Box::new(default_ty),
            ),
            span,
        )
    }

    /// Check if a loop expression contains break statements with values
    fn loop_has_break_with_value(&self, expr: &hir::Expr) -> bool {
        if let hir::ExprKind::Loop(body) = &expr.kind {
            self.block_has_break_with_value(body)
        } else {
            false
        }
    }

    /// Check if a block contains break statements with values
    fn block_has_break_with_value(&self, block: &hir::Block) -> bool {
        // Check statements
        for stmt in &block.stmts {
            if self.stmt_has_break_with_value(stmt) {
                return true;
            }
        }

        // Check completion expression
        if let Some(expr) = &block.expr {
            if self.expr_has_break_with_value(expr) {
                return true;
            }
        }

        false
    }

    /// Check if a block contains return statements
    fn block_has_return_statements(&self, block: &hir::Block) -> bool {
        // Check statements
        for stmt in &block.stmts {
            if self.stmt_has_return(stmt) {
                return true;
            }
        }

        // Check completion expression
        if let Some(expr) = &block.expr {
            if self.expr_has_return(expr) {
                return true;
            }
        }

        false
    }

    /// Check if a statement contains break expressions with values
    fn stmt_has_break_with_value(&self, stmt: &hir::Stmt) -> bool {
        match &stmt.kind {
            hir::StmtKind::Expr(expr) => self.expr_has_break_with_value(expr),
            hir::StmtKind::Let(_, expr, _) => self.expr_has_break_with_value(expr),
            hir::StmtKind::Return(Some(expr)) => self.expr_has_break_with_value(expr),
            _ => false,
        }
    }

    /// Check if an expression contains break expressions with values
    fn expr_has_break_with_value(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Break(Some(_)) => true,
            hir::ExprKind::Break(None) | hir::ExprKind::Continue => false,
            hir::ExprKind::Block(block) => self.block_has_break_with_value(block),
            hir::ExprKind::IfElse(cond, then_branch, else_branches) => {
                self.expr_has_break_with_value(cond)
                    || self.block_has_break_with_value(then_branch)
                    || else_branches
                        .iter()
                        .any(|clause| self.block_has_break_with_value(&clause.consequence))
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                self.expr_has_break_with_value(scrutinee)
                    || arms
                        .iter()
                        .any(|arm| self.block_has_break_with_value(&arm.block))
            }
            hir::ExprKind::Binary(_, lhs, rhs) => {
                self.expr_has_break_with_value(lhs) || self.expr_has_break_with_value(rhs)
            }
            hir::ExprKind::Unary(_, operand) => self.expr_has_break_with_value(operand),
            hir::ExprKind::Call(call) => call
                .arguments
                .iter()
                .any(|arg| self.expr_has_break_with_value(arg)),
            hir::ExprKind::List(elements) => elements
                .iter()
                .any(|elem| self.expr_has_break_with_value(elem)),
            hir::ExprKind::FieldAccess(obj, _) => self.expr_has_break_with_value(obj),
            hir::ExprKind::IndexAccess(obj, index) => {
                self.expr_has_break_with_value(obj) || self.expr_has_break_with_value(index)
            }
            _ => false,
        }
    }

    /// Check if a statement contains return statements
    fn stmt_has_return(&self, stmt: &hir::Stmt) -> bool {
        match &stmt.kind {
            hir::StmtKind::Return(_) => true,
            hir::StmtKind::Expr(expr) => self.expr_has_return(expr),
            hir::StmtKind::Let(_, expr, _) => self.expr_has_return(expr),
            _ => false,
        }
    }

    /// Check if an expression contains return statements
    fn expr_has_return(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Block(block) => self.block_has_return_statements(block),
            hir::ExprKind::IfElse(cond, then_branch, else_branches) => {
                self.expr_has_return(cond)
                    || self.block_has_return_statements(then_branch)
                    || else_branches
                        .iter()
                        .any(|clause| self.block_has_return_statements(&clause.consequence))
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                self.expr_has_return(scrutinee)
                    || arms
                        .iter()
                        .any(|arm| self.block_has_return_statements(&arm.block))
            }
            hir::ExprKind::Binary(_, lhs, rhs) => {
                self.expr_has_return(lhs) || self.expr_has_return(rhs)
            }
            hir::ExprKind::Unary(_, operand) => self.expr_has_return(operand),
            hir::ExprKind::Call(call) => call.arguments.iter().any(|arg| self.expr_has_return(arg)),
            hir::ExprKind::List(elements) => elements.iter().any(|elem| self.expr_has_return(elem)),
            hir::ExprKind::FieldAccess(obj, _) => self.expr_has_return(obj),
            hir::ExprKind::IndexAccess(obj, index) => {
                self.expr_has_return(obj) || self.expr_has_return(index)
            }
            _ => false,
        }
    }

    /// Flatten expressions that cannot be represented in JavaScript to temp variables
    fn flatten_expression_to_temp_var(
        &mut self,
        expr: hir::Expr,
        ctx: &mut HirOptContext,
    ) -> (hir::Expr, Vec<hir::Stmt>) {
        let span = expr.span;

        // Handle complex expressions specially - only generate temp variables when actually needed
        match &expr.kind {
            hir::ExprKind::Match(_, arms) => {
                // Check if all arms have return statements - if so, no temp variable assignment is needed
                let all_arms_have_returns = arms.iter().all(|arm| {
                    if let Some(last_stmt) = arm.block.stmts.last() {
                        matches!(last_stmt.kind, hir::StmtKind::Return(_))
                    } else {
                        false
                    }
                });

                if all_arms_have_returns {
                    // Create temp variable but don't assign to it (since all arms return)
                    // This maintains the expected HIR structure for functions with completion expressions
                    let temp_name = self.generate_temp_var_name();
                    let temp_declaration = self.create_temp_var_declaration(ctx, &temp_name, span);
                    let temp_ref = self.create_temp_var_path(ctx, &temp_name, span);
                    let statements = self.transform_match_to_statements(expr, "", ctx);
                    let mut all_stmts = vec![temp_declaration];
                    all_stmts.extend(statements);
                    (temp_ref, all_stmts)
                } else {
                    // For match expressions, generate temp variable and transform to proper statements
                    let temp_name = self.generate_temp_var_name();
                    let temp_declaration = self.create_temp_var_declaration(ctx, &temp_name, span);
                    let temp_ref = self.create_temp_var_path(ctx, &temp_name, span);
                    let statements = self.transform_match_to_statements(expr, &temp_name, ctx);
                    let mut all_stmts = vec![temp_declaration];
                    all_stmts.extend(statements);
                    (temp_ref, all_stmts)
                }
            }
            hir::ExprKind::Break(None) => {
                // For break expressions without values, move to statement position
                let plain_break = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Break(None),
                        span,
                    })),
                    span,
                );

                // Return wildcard expression (since code after break is dead)
                let wildcard_expr = hir::Expr {
                    hir_id: ctx.hir_id_allocator.next_id(),
                    kind: hir::ExprKind::Wildcard,
                    span,
                };

                (wildcard_expr, vec![plain_break])
            }
            hir::ExprKind::Continue => {
                // For continue expressions, move to statement position
                let plain_continue = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Continue,
                        span,
                    })),
                    span,
                );

                // Return wildcard expression (since code after continue is dead)
                let wildcard_expr = hir::Expr {
                    hir_id: ctx.hir_id_allocator.next_id(),
                    kind: hir::ExprKind::Wildcard,
                    span,
                };

                (wildcard_expr, vec![plain_continue])
            }
            _ => {
                // For all other cases, generate temp variable normally
                let temp_name = self.generate_temp_var_name();
                let temp_declaration = self.create_temp_var_declaration(ctx, &temp_name, span);
                let temp_ref = self.create_temp_var_path(ctx, &temp_name, span);

                match &expr.kind {
                    hir::ExprKind::Block(_block) => {
                        // For block expressions, transform the block to assign to temp variable
                        let statements = self.transform_block_to_statements(expr, &temp_name, ctx);
                        let mut all_stmts = vec![temp_declaration];
                        all_stmts.extend(statements);
                        (temp_ref, all_stmts)
                    }
                    hir::ExprKind::Loop(..) => {
                        // For loop expressions, inline the transformation instead of using transform_loop_expression
                        // We need to handle the break statements and make the loop a statement

                        // Transform the loop body to assign to temp variable on break
                        let mut transformed_loop = expr.clone();
                        if let hir::ExprKind::Loop(body) = &mut transformed_loop.kind {
                            self.transform_break_statements_in_block(body, &temp_name, ctx);
                        }

                        // Create loop statement (not assigned to anything)
                        let loop_stmt = hir::Stmt::new(
                            ctx.hir_id_allocator.next_id(),
                            hir::StmtKind::Expr(Box::new(transformed_loop)),
                            span,
                        );

                        let all_stmts = vec![temp_declaration, loop_stmt];
                        (temp_ref, all_stmts)
                    }
                    hir::ExprKind::IfElse(..) => {
                        // For if-else expressions, transform to proper statements
                        let statements =
                            self.transform_if_else_to_statements(expr, &temp_name, ctx);
                        let mut all_stmts = vec![temp_declaration];
                        all_stmts.extend(statements);
                        (temp_ref, all_stmts)
                    }
                    hir::ExprKind::Break(Some(break_value)) => {
                        // For break expressions with values, transform to:
                        // let result = break value; -> temp_var = value; break; let result = temp_var;

                        // Extract the break value
                        let value_expr = *break_value.clone();
                        let assignment_stmt =
                            self.create_assignment_stmt(ctx, &temp_name, value_expr, span);

                        // Create plain break statement
                        let plain_break = hir::Stmt::new(
                            ctx.hir_id_allocator.next_id(),
                            hir::StmtKind::Expr(Box::new(hir::Expr {
                                hir_id: ctx.hir_id_allocator.next_id(),
                                kind: hir::ExprKind::Break(None),
                                span,
                            })),
                            span,
                        );

                        (
                            temp_ref,
                            vec![temp_declaration, assignment_stmt, plain_break],
                        )
                    }
                    hir::ExprKind::Binary(hir::BinaryOpKind::Or, lhs, rhs) => {
                        // Transform: a || b -> if a { temp = a; } else { temp = b; }
                        // Need to flatten both operands first if they're complex
                        let mut all_stmts = vec![temp_declaration];

                        // Flatten left operand if needed
                        let (left_expr, left_stmts) = if !self.can_render_as_js_expr(lhs) {
                            self.flatten_expression_to_temp_var((**lhs).clone(), ctx)
                        } else {
                            ((**lhs).clone(), vec![])
                        };
                        all_stmts.extend(left_stmts);

                        // Flatten right operand if needed
                        let (right_expr, right_stmts) = if !self.can_render_as_js_expr(rhs) {
                            self.flatten_expression_to_temp_var((**rhs).clone(), ctx)
                        } else {
                            ((**rhs).clone(), vec![])
                        };

                        // Create if-else structure: if left { temp = left; } else { right_stmts; temp = right; }
                        let then_block = hir::Block::new(
                            ctx.hir_id_allocator.next_id(),
                            vec![self.create_assignment_stmt(
                                ctx,
                                &temp_name,
                                left_expr.clone(),
                                span,
                            )],
                            None,
                            span,
                        );

                        let mut else_stmts = right_stmts;
                        else_stmts
                            .push(self.create_assignment_stmt(ctx, &temp_name, right_expr, span));
                        let else_block =
                            hir::Block::new(ctx.hir_id_allocator.next_id(), else_stmts, None, span);

                        let if_else_expr = hir::Expr {
                            hir_id: ctx.hir_id_allocator.next_id(),
                            kind: hir::ExprKind::IfElse(
                                Box::new(left_expr),
                                Box::new(then_block),
                                vec![hir::ElseClause {
                                    condition: None,
                                    consequence: else_block,
                                }],
                            ),
                            span,
                        };

                        let if_else_stmt = hir::Stmt::new(
                            ctx.hir_id_allocator.next_id(),
                            hir::StmtKind::Expr(Box::new(if_else_expr)),
                            span,
                        );
                        all_stmts.push(if_else_stmt);

                        (temp_ref, all_stmts)
                    }
                    hir::ExprKind::Binary(hir::BinaryOpKind::And, lhs, rhs) => {
                        // Transform: a && b -> if a { temp = b; } else { temp = false; }
                        // Need to flatten both operands first if they're complex
                        let mut all_stmts = vec![temp_declaration];

                        // Flatten left operand if needed
                        let (left_expr, left_stmts) = if !self.can_render_as_js_expr(lhs) {
                            self.flatten_expression_to_temp_var((**lhs).clone(), ctx)
                        } else {
                            ((**lhs).clone(), vec![])
                        };
                        all_stmts.extend(left_stmts);

                        // Flatten right operand if needed
                        let (right_expr, right_stmts) = if !self.can_render_as_js_expr(rhs) {
                            self.flatten_expression_to_temp_var((**rhs).clone(), ctx)
                        } else {
                            ((**rhs).clone(), vec![])
                        };

                        // Create if-else structure: if left { right_stmts; temp = right; } else { temp = false; }
                        let mut then_stmts = right_stmts;
                        then_stmts
                            .push(self.create_assignment_stmt(ctx, &temp_name, right_expr, span));
                        let then_block =
                            hir::Block::new(ctx.hir_id_allocator.next_id(), then_stmts, None, span);

                        let false_expr = hir::Expr {
                            hir_id: ctx.hir_id_allocator.next_id(),
                            kind: hir::ExprKind::Literal(Box::new(
                                tlang_ast::token::Literal::Boolean(false),
                            )),
                            span,
                        };

                        let else_block = hir::Block::new(
                            ctx.hir_id_allocator.next_id(),
                            vec![self.create_assignment_stmt(ctx, &temp_name, false_expr, span)],
                            None,
                            span,
                        );

                        let if_else_expr = hir::Expr {
                            hir_id: ctx.hir_id_allocator.next_id(),
                            kind: hir::ExprKind::IfElse(
                                Box::new(left_expr),
                                Box::new(then_block),
                                vec![hir::ElseClause {
                                    condition: None,
                                    consequence: else_block,
                                }],
                            ),
                            span,
                        };

                        let if_else_stmt = hir::Stmt::new(
                            ctx.hir_id_allocator.next_id(),
                            hir::StmtKind::Expr(Box::new(if_else_expr)),
                            span,
                        );
                        all_stmts.push(if_else_stmt);

                        (temp_ref, all_stmts)
                    }
                    _ => {
                        // For simple expressions, create assignment statement
                        let assignment_stmt =
                            self.create_assignment_stmt(ctx, &temp_name, expr, span);
                        (temp_ref, vec![temp_declaration, assignment_stmt])
                    }
                }
            }
        }
    }

    /// Check if an expression can be represented as a JavaScript expression or statement
    /// This is more comprehensive than the basic check and accounts for transformed expressions
    fn can_render_as_js_expr(&self, expr: &hir::Expr) -> bool {
        // First check the basic JS expression rules
        if expr_can_render_as_js_expr(expr) {
            return true;
        }

        // Check for match expressions that have been transformed with return statements or control flow statements
        // These can be rendered as JavaScript statements and don't need further transformation
        if let hir::ExprKind::Match(_, arms) = &expr.kind {
            // If all arms have return statements or plain control flow statements (break without values),
            // this match can be rendered as JS. However, break statements with values need transformation
            // to assign to temp variables, so they don't qualify.
            let all_arms_have_simple_control_flow = arms.iter().all(|arm| {
                // Check if the last statement is a return or control flow statement
                if let Some(last_stmt) = arm.block.stmts.last() {
                    match &last_stmt.kind {
                        hir::StmtKind::Return(_) => true,
                        hir::StmtKind::Expr(expr) => {
                            match &expr.kind {
                                hir::ExprKind::Break(None) | hir::ExprKind::Continue => true,
                                hir::ExprKind::Break(Some(_)) => false, // Break with value needs transformation
                                _ => false,
                            }
                        }
                        _ => false,
                    }
                } else {
                    // No statements, check if the completion expression is a simple control flow statement
                    if let Some(completion_expr) = &arm.block.expr {
                        match &completion_expr.kind {
                            hir::ExprKind::Break(None) | hir::ExprKind::Continue => true,
                            hir::ExprKind::Break(Some(_)) => false, // Break with value needs transformation
                            _ => false,
                        }
                    } else {
                        false
                    }
                }
            });

            if all_arms_have_simple_control_flow {
                return true;
            }
        }

        false
    }

    /// Check if an expression can be handled directly as a statement without temp variables
    /// This is for expressions that appear in statement context where we don't need their value
    fn can_render_as_js_stmt(&self, expr: &hir::Expr) -> bool {
        // Use the utility function which handles most cases correctly
        expr_can_render_as_js_stmt(expr)
    }

    /// Check if an expression is a temp variable that we created
    fn is_temp_variable(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Path(path) => {
                if let Some(first_segment) = path.segments.first() {
                    first_segment.ident.as_str().starts_with("$hir$")
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Check if an expression contains temp variables (for more comprehensive detection)
    fn contains_temp_variables(&self, expr: &hir::Expr) -> bool {
        if self.is_temp_variable(expr) {
            return true;
        }

        match &expr.kind {
            hir::ExprKind::Binary(_, lhs, rhs) => {
                self.contains_temp_variables(lhs) || self.contains_temp_variables(rhs)
            }
            hir::ExprKind::Unary(_, operand) => self.contains_temp_variables(operand),
            hir::ExprKind::Call(call) => call
                .arguments
                .iter()
                .any(|arg| self.contains_temp_variables(arg)),
            hir::ExprKind::List(elements) => elements
                .iter()
                .any(|elem| self.contains_temp_variables(elem)),
            hir::ExprKind::FieldAccess(obj, _) => self.contains_temp_variables(obj),
            hir::ExprKind::Loop(block) => {
                // Check if the loop body contains temp variables
                self.block_contains_temp_variables(block)
            }
            hir::ExprKind::Block(block) => {
                // Check if the block contains temp variables
                self.block_contains_temp_variables(block)
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                // Check if the match scrutinee or any arm contains temp variables
                if self.contains_temp_variables(scrutinee) {
                    return true;
                }
                arms.iter()
                    .any(|arm| self.block_contains_temp_variables(&arm.block))
            }
            _ => false,
        }
    }

    fn block_contains_temp_variables(&self, block: &hir::Block) -> bool {
        // Check statements for temp variables
        for stmt in &block.stmts {
            if self.stmt_contains_temp_variables(stmt) {
                return true;
            }
        }

        // Check completion expression
        if let Some(ref expr) = block.expr
            && self.contains_temp_variables(expr)
        {
            return true;
        }

        false
    }

    fn stmt_contains_temp_variables(&self, stmt: &hir::Stmt) -> bool {
        match &stmt.kind {
            hir::StmtKind::Let(_, expr, _) => self.contains_temp_variables(expr),
            hir::StmtKind::Expr(expr) => self.contains_temp_variables(expr),
            hir::StmtKind::Return(Some(expr)) => self.contains_temp_variables(expr),
            hir::StmtKind::Return(None) => false,
            _ => false,
        }
    }

    /// Process sub-expressions that might need flattening before processing the containing expression
    fn process_sub_expressions(
        &mut self,
        expr: &mut hir::Expr,
        ctx: &mut HirOptContext,
    ) -> Vec<hir::Stmt> {
        let mut additional_stmts = Vec::new();

        match &mut expr.kind {
            hir::ExprKind::Binary(op_kind, lhs, rhs) => {
                // Special handling for short-circuit operators
                match op_kind {
                    hir::BinaryOpKind::Or | hir::BinaryOpKind::And => {
                        // Short-circuit operators need special handling when operands are complex
                        // Transform: a || b -> if a { temp = a; } else { temp = b; }
                        // Transform: a && b -> if a { temp = b; } else { temp = false; }
                        if (!self.can_render_as_js_expr(lhs) && !self.contains_temp_variables(lhs))
                            || (!self.can_render_as_js_expr(rhs)
                                && !self.contains_temp_variables(rhs))
                        {
                            // We have complex operands, so transform the entire expression to if-else
                            // Mark the entire expression as needing transformation by returning
                            // empty additional_stmts - the parent will call flatten_expression_to_temp_var
                            return additional_stmts;
                        }

                        // Process operands normally if both are simple
                        if !self.can_render_as_js_expr(lhs) && !self.contains_temp_variables(lhs) {
                            let (flattened_expr, mut temp_stmts) =
                                self.flatten_expression_to_temp_var((**lhs).clone(), ctx);
                            additional_stmts.append(&mut temp_stmts);
                            **lhs = flattened_expr;
                            self.changes_made = true;
                        }

                        if !self.can_render_as_js_expr(rhs) && !self.contains_temp_variables(rhs) {
                            let (flattened_expr, mut temp_stmts) =
                                self.flatten_expression_to_temp_var((**rhs).clone(), ctx);
                            additional_stmts.append(&mut temp_stmts);
                            **rhs = flattened_expr;
                            self.changes_made = true;
                        }
                    }
                    _ => {
                        // Regular binary operators - process operands normally
                        if !self.can_render_as_js_expr(lhs) && !self.contains_temp_variables(lhs) {
                            let (flattened_expr, mut temp_stmts) =
                                self.flatten_expression_to_temp_var((**lhs).clone(), ctx);
                            additional_stmts.append(&mut temp_stmts);
                            **lhs = flattened_expr;
                            self.changes_made = true;
                        }

                        if !self.can_render_as_js_expr(rhs) && !self.contains_temp_variables(rhs) {
                            let (flattened_expr, mut temp_stmts) =
                                self.flatten_expression_to_temp_var((**rhs).clone(), ctx);
                            additional_stmts.append(&mut temp_stmts);
                            **rhs = flattened_expr;
                            self.changes_made = true;
                        }
                    }
                }
            }
            hir::ExprKind::Unary(_, operand) => {
                // Process operand
                if !self.can_render_as_js_expr(operand) && !self.contains_temp_variables(operand) {
                    let (flattened_expr, mut temp_stmts) =
                        self.flatten_expression_to_temp_var((**operand).clone(), ctx);
                    additional_stmts.append(&mut temp_stmts);
                    **operand = flattened_expr;
                    self.changes_made = true;
                }
            }
            hir::ExprKind::Call(call) => {
                // Process function call arguments
                for arg in &mut call.arguments {
                    if !self.can_render_as_js_expr(arg) && !self.contains_temp_variables(arg) {
                        let (flattened_expr, mut temp_stmts) =
                            self.flatten_expression_to_temp_var(arg.clone(), ctx);
                        additional_stmts.append(&mut temp_stmts);
                        *arg = flattened_expr;
                        self.changes_made = true;
                    }
                }
            }
            hir::ExprKind::List(elements) => {
                // Process list elements
                for element in elements {
                    if !self.can_render_as_js_expr(element)
                        && !self.contains_temp_variables(element)
                    {
                        let (flattened_expr, mut temp_stmts) =
                            self.flatten_expression_to_temp_var(element.clone(), ctx);
                        additional_stmts.append(&mut temp_stmts);
                        *element = flattened_expr;
                        self.changes_made = true;
                    }
                }
            }
            hir::ExprKind::FieldAccess(obj, _) => {
                // Process object expression
                if !self.can_render_as_js_expr(obj) && !self.contains_temp_variables(obj) {
                    // Skip break/continue expressions - they should not be flattened to temp variables
                    match &obj.kind {
                        hir::ExprKind::Break(..) | hir::ExprKind::Continue => {
                            // Leave break/continue expressions as-is
                        }
                        _ => {
                            let (flattened_expr, mut temp_stmts) =
                                self.flatten_expression_to_temp_var((**obj).clone(), ctx);
                            additional_stmts.append(&mut temp_stmts);
                            **obj = flattened_expr;
                            self.changes_made = true;
                        }
                    }
                }
            }
            hir::ExprKind::IndexAccess(obj, index) => {
                // Process object expression
                if !self.can_render_as_js_expr(obj) && !self.contains_temp_variables(obj) {
                    let (flattened_expr, mut temp_stmts) =
                        self.flatten_expression_to_temp_var((**obj).clone(), ctx);
                    additional_stmts.append(&mut temp_stmts);
                    **obj = flattened_expr;
                    self.changes_made = true;
                }

                // Process index expression
                if !self.can_render_as_js_expr(index) && !self.contains_temp_variables(index) {
                    let (flattened_expr, mut temp_stmts) =
                        self.flatten_expression_to_temp_var((**index).clone(), ctx);
                    additional_stmts.append(&mut temp_stmts);
                    **index = flattened_expr;
                    self.changes_made = true;
                }
            }
            hir::ExprKind::IfElse(condition, _then_branch, _else_branches) => {
                // Process condition first (it might contain complex expressions like loops)
                if !self.can_render_as_js_expr(condition)
                    && !self.contains_temp_variables(condition)
                {
                    let (flattened_expr, mut temp_stmts) =
                        self.flatten_expression_to_temp_var((**condition).clone(), ctx);
                    additional_stmts.append(&mut temp_stmts);
                    **condition = flattened_expr;
                    self.changes_made = true;
                }

                // Note: then_branch and else_branches are processed by the visitor pattern
                // if they contain nested expressions that need processing
            }
            hir::ExprKind::Match(scrutinee, _arms) => {
                // Process scrutinee first (it might contain complex expressions like loops)
                if !self.can_render_as_js_expr(scrutinee)
                    && !self.contains_temp_variables(scrutinee)
                {
                    let (flattened_expr, mut temp_stmts) =
                        self.flatten_expression_to_temp_var((**scrutinee).clone(), ctx);
                    additional_stmts.append(&mut temp_stmts);
                    **scrutinee = flattened_expr;
                    self.changes_made = true;
                }

                // Note: match arms are processed by the visitor pattern
                // if they contain nested expressions that need processing
            }
            _ => {
                // For other expressions, no sub-expression processing needed
            }
        }

        additional_stmts
    }

    /// Transform break statements in a block to assign to temp variable
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
                        let assignment_stmt =
                            self.create_assignment_stmt(ctx, temp_name, *break_expr.clone(), span);
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
                    } else if let hir::ExprKind::Break(None) = &expr.kind {
                        // Plain break without value - just add it as-is, no temp variable assignment needed
                        new_stmts.push(stmt.clone());
                    } else {
                        // For other expressions, recursively transform
                        self.transform_break_statements_in_expr(expr, temp_name, ctx);
                        new_stmts.push(stmt.clone());
                    }
                }
                _ => {
                    new_stmts.push(stmt.clone());
                }
            }
        }

        block.stmts = new_stmts;

        // Also check the block's completion expression for break statements
        if let Some(ref mut expr) = block.expr {
            if let hir::ExprKind::Break(Some(break_expr)) = &expr.kind {
                // Transform break in completion position -> temp_var = value; block.expr = None
                let assignment_stmt =
                    self.create_assignment_stmt(ctx, temp_name, *break_expr.clone(), expr.span);
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
            } else if let hir::ExprKind::Break(None) = &expr.kind {
                // Plain break without value in completion position - convert to statement
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

    /// Transform break statements in expressions
    fn transform_break_statements_in_expr(
        &mut self,
        expr: &mut hir::Expr,
        temp_name: &str,
        ctx: &mut HirOptContext,
    ) {
        match &mut expr.kind {
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
                    self.transform_break_statements_in_block(
                        &mut else_branch.consequence,
                        temp_name,
                        ctx,
                    );
                }
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                // Transform break statements in match scrutinee
                self.transform_break_statements_in_expr(scrutinee, temp_name, ctx);

                // Transform break statements in each match arm
                for arm in arms {
                    if let Some(ref mut guard) = arm.guard {
                        self.transform_break_statements_in_expr(guard, temp_name, ctx);
                    }
                    self.transform_break_statements_in_block(&mut arm.block, temp_name, ctx);
                }
            }
            _ => {
                // For other expression types, use the visitor to walk through them
                walk_expr(self, expr, ctx);
            }
        }
    }

    /// Transform a block expression to statements that assign completion value to temp variable
    fn transform_block_to_statements(
        &mut self,
        expr: hir::Expr,
        temp_name: &str,
        ctx: &mut HirOptContext,
    ) -> Vec<hir::Stmt> {
        let span = expr.span;
        if let hir::ExprKind::Block(block) = expr.kind {
            // Preserve block structure by creating a block statement
            let mut block_stmts = block.stmts;

            // Handle completion expression by adding assignment within the block
            if let Some(completion_expr) = block.expr {
                // Special handling for loop expressions - they can't be on the RHS of assignments
                if let hir::ExprKind::Loop(..) = &completion_expr.kind {
                    // Create temp variable for loop completion
                    let loop_temp_name = self.generate_temp_var_name();
                    let loop_span = completion_expr.span;

                    // Create temp variable declaration for loop result
                    let loop_temp_declaration =
                        self.create_temp_var_declaration(ctx, &loop_temp_name, loop_span);
                    block_stmts.push(loop_temp_declaration);

                    // Transform the loop to assign to temp variable on break
                    let mut transformed_loop = completion_expr;
                    if let hir::ExprKind::Loop(body) = &mut transformed_loop.kind {
                        // First, visit the loop body to process any complex expressions
                        self.visit_block(body, ctx);

                        // Then transform break statements to assign to temp variable
                        self.transform_break_statements_in_block(body, &loop_temp_name, ctx);

                        // Finally, ensure loop body completion assigns to loop completion variable
                        if let Some(ref mut completion_expr) = body.expr {
                            // If completion is a temp variable reference, convert to assignment
                            if self.is_temp_variable(completion_expr) {
                                let completion_assignment = self.create_assignment_stmt(
                                    ctx,
                                    &loop_temp_name,
                                    completion_expr.clone(),
                                    completion_expr.span,
                                );
                                body.stmts.push(completion_assignment);

                                // Set completion to None since it's now captured in statement
                                body.expr = None;
                            }
                        }
                    }

                    // Add loop as statement
                    let loop_stmt = hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Expr(Box::new(transformed_loop)),
                        loop_span,
                    );
                    block_stmts.push(loop_stmt);

                    // Add assignment to capture loop result for block completion
                    let loop_temp_ref = self.create_temp_var_path(ctx, &loop_temp_name, loop_span);
                    let block_completion_assignment =
                        self.create_assignment_stmt(ctx, temp_name, loop_temp_ref, loop_span);
                    block_stmts.push(block_completion_assignment);
                } else {
                    // Check if the block has return statements that would make the completion assignment unreachable
                    let block_ref = hir::Block::new(
                        ctx.hir_id_allocator.next_id(),
                        block_stmts.clone(),
                        Some(completion_expr.clone()),
                        span,
                    );

                    if self.block_has_return_statements(&block_ref) {
                        // Block has return statements, so completion expression assignment would be stray
                        // Just add the completion expression as a statement without assignment
                        let expr_stmt = hir::Stmt::new(
                            ctx.hir_id_allocator.next_id(),
                            hir::StmtKind::Expr(Box::new(completion_expr)),
                            span,
                        );
                        block_stmts.push(expr_stmt);
                    } else {
                        // No return statements, create assignment as before
                        let assignment_stmt =
                            self.create_assignment_stmt(ctx, temp_name, completion_expr, span);
                        block_stmts.push(assignment_stmt);
                    }
                }
            }

            // Create a new block with the modified statements and wrap it in a block expression statement
            let modified_block = hir::Block::new(
                ctx.hir_id_allocator.next_id(),
                block_stmts,
                None, // No completion expression since we converted it to assignment
                span,
            );

            let block_expr_stmt = hir::Stmt::new(
                ctx.hir_id_allocator.next_id(),
                hir::StmtKind::Expr(Box::new(hir::Expr {
                    hir_id: ctx.hir_id_allocator.next_id(),
                    kind: hir::ExprKind::Block(Box::new(modified_block)),
                    span,
                })),
                span,
            );

            vec![block_expr_stmt]
        } else {
            // Fallback: create a simple assignment statement
            vec![self.create_assignment_stmt(ctx, temp_name, expr, span)]
        }
    }

    /// Transform a match expression to statements that assign results to temp variable
    fn transform_match_to_statements(
        &mut self,
        expr: hir::Expr,
        temp_name: &str,
        ctx: &mut HirOptContext,
    ) -> Vec<hir::Stmt> {
        let span = expr.span;
        if let hir::ExprKind::Match(scrutinee, arms) = expr.kind {
            // Check if all arms already have return statements (from ReturnStatementPass)
            // or simple control flow statements (break without values / continue)
            let all_arms_have_simple_control_flow = arms.iter().all(|arm| {
                // Check last statement
                if let Some(last_stmt) = arm.block.stmts.last() {
                    match &last_stmt.kind {
                        hir::StmtKind::Return(_) => return true,
                        hir::StmtKind::Expr(expr) => {
                            match &expr.kind {
                                hir::ExprKind::Break(None) | hir::ExprKind::Continue => {
                                    return true;
                                }
                                hir::ExprKind::Break(Some(_)) => return false, // Break with value needs transformation
                                _ => {}
                            }
                        }
                        _ => {}
                    }
                }

                // Check completion expression
                if let Some(completion_expr) = &arm.block.expr {
                    match &completion_expr.kind {
                        hir::ExprKind::Break(None) | hir::ExprKind::Continue => return true,
                        hir::ExprKind::Break(Some(_)) => return false, // Break with value needs transformation
                        _ => {}
                    }
                }

                false
            });

            // If all arms have simple control flow statements or temp_name is empty, don't create assignment statements
            if all_arms_have_simple_control_flow || temp_name.is_empty() {
                // Just create the match statement without temp variable assignments
                let match_stmt = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Match(scrutinee, arms),
                        span,
                    })),
                    span,
                );
                return vec![match_stmt];
            }

            // Transform each arm to assign to temp variable
            let mut transformed_arms = Vec::new();

            for arm in arms {
                let mut new_arm = arm;

                // Transform the completion expression in each arm
                if let Some(completion_expr) = new_arm.block.expr.take() {
                    // Handle break/continue expressions specially
                    match &completion_expr.kind {
                        hir::ExprKind::Break(Some(break_value)) => {
                            // For break with value: temp_var = value; break;
                            let assignment_stmt = self.create_assignment_stmt(
                                ctx,
                                temp_name,
                                (**break_value).clone(),
                                span,
                            );
                            new_arm.block.stmts.push(assignment_stmt);

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
                            new_arm.block.stmts.push(plain_break);
                        }
                        hir::ExprKind::Break(None) | hir::ExprKind::Continue => {
                            // For break/continue without value, add them as statements directly
                            let stmt = hir::Stmt::new(
                                ctx.hir_id_allocator.next_id(),
                                hir::StmtKind::Expr(Box::new(completion_expr)),
                                span,
                            );
                            new_arm.block.stmts.push(stmt);
                        }
                        _ => {
                            // For other expressions, process normally
                            let mut flattened_completion = completion_expr;
                            if !self.can_render_as_js_expr(&flattened_completion)
                                && !self.contains_temp_variables(&flattened_completion)
                            {
                                let mut sub_stmts =
                                    self.process_sub_expressions(&mut flattened_completion, ctx);
                                new_arm.block.stmts.append(&mut sub_stmts);
                            }

                            // Only create assignment if temp_name is provided
                            if !temp_name.is_empty() {
                                let assignment_stmt = self.create_assignment_stmt(
                                    ctx,
                                    temp_name,
                                    flattened_completion,
                                    span,
                                );
                                new_arm.block.stmts.push(assignment_stmt);
                            } else {
                                // If no temp_name, add the expression as a statement
                                let expr_stmt = hir::Stmt::new(
                                    ctx.hir_id_allocator.next_id(),
                                    hir::StmtKind::Expr(Box::new(flattened_completion)),
                                    span,
                                );
                                new_arm.block.stmts.push(expr_stmt);
                            }
                        }
                    }
                }

                transformed_arms.push(new_arm);
            }

            // Create the match statement
            let match_stmt = hir::Stmt::new(
                ctx.hir_id_allocator.next_id(),
                hir::StmtKind::Expr(Box::new(hir::Expr {
                    hir_id: ctx.hir_id_allocator.next_id(),
                    kind: hir::ExprKind::Match(scrutinee, transformed_arms),
                    span,
                })),
                span,
            );

            vec![match_stmt]
        } else {
            // Fallback: create a simple assignment statement if temp_name is provided
            if !temp_name.is_empty() {
                vec![self.create_assignment_stmt(ctx, temp_name, expr, span)]
            } else {
                // No temp variable, just wrap in an expression statement
                vec![hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(expr)),
                    span,
                )]
            }
        }
    }

    /// Transform an if-else expression to statements that assign results to temp variable
    fn transform_if_else_to_statements(
        &mut self,
        expr: hir::Expr,
        temp_name: &str,
        ctx: &mut HirOptContext,
    ) -> Vec<hir::Stmt> {
        let span = expr.span;
        if let hir::ExprKind::IfElse(condition, then_branch, else_branches) = expr.kind {
            // Transform branches to assign to temp variable
            let mut new_then_branch = *then_branch;
            if let Some(completion_expr) = new_then_branch.expr.take() {
                // If the completion expression is complex, flatten it first
                if !self.can_render_as_js_expr(&completion_expr)
                    && !self.contains_temp_variables(&completion_expr)
                {
                    if let hir::ExprKind::Block(block) = completion_expr.kind {
                        // For block expressions, flatten the contents directly into the branch
                        new_then_branch.stmts.extend(block.stmts);
                        if let Some(block_completion) = block.expr {
                            let assignment_stmt =
                                self.create_assignment_stmt(ctx, temp_name, block_completion, span);
                            new_then_branch.stmts.push(assignment_stmt);
                        }
                    } else {
                        // For other complex expressions, create a nested flattening
                        let (flattened_expr, temp_stmts) =
                            self.flatten_expression_to_temp_var(completion_expr, ctx);
                        new_then_branch.stmts.extend(temp_stmts);
                        let assignment_stmt =
                            self.create_assignment_stmt(ctx, temp_name, flattened_expr, span);
                        new_then_branch.stmts.push(assignment_stmt);
                    }
                } else {
                    // For simple expressions, create assignment directly
                    let assignment_stmt =
                        self.create_assignment_stmt(ctx, temp_name, completion_expr, span);
                    new_then_branch.stmts.push(assignment_stmt);
                }
            }

            let mut new_else_branches = Vec::new();
            for else_branch in else_branches {
                let mut new_else_branch = else_branch;
                if let Some(completion_expr) = new_else_branch.consequence.expr.take() {
                    // If the completion expression is complex, flatten it first
                    if !self.can_render_as_js_expr(&completion_expr)
                        && !self.contains_temp_variables(&completion_expr)
                    {
                        if let hir::ExprKind::Block(block) = completion_expr.kind {
                            // For block expressions, flatten the contents directly into the branch
                            new_else_branch.consequence.stmts.extend(block.stmts);
                            if let Some(block_completion) = block.expr {
                                let assignment_stmt = self.create_assignment_stmt(
                                    ctx,
                                    temp_name,
                                    block_completion,
                                    span,
                                );
                                new_else_branch.consequence.stmts.push(assignment_stmt);
                            }
                        } else {
                            // For other complex expressions, create a nested flattening
                            let (flattened_expr, temp_stmts) =
                                self.flatten_expression_to_temp_var(completion_expr, ctx);
                            new_else_branch.consequence.stmts.extend(temp_stmts);
                            let assignment_stmt =
                                self.create_assignment_stmt(ctx, temp_name, flattened_expr, span);
                            new_else_branch.consequence.stmts.push(assignment_stmt);
                        }
                    } else {
                        // For simple expressions, create assignment directly
                        let assignment_stmt =
                            self.create_assignment_stmt(ctx, temp_name, completion_expr, span);
                        new_else_branch.consequence.stmts.push(assignment_stmt);
                    }
                }
                new_else_branches.push(new_else_branch);
            }

            // Create the if-else statement
            let if_else_stmt = hir::Stmt::new(
                ctx.hir_id_allocator.next_id(),
                hir::StmtKind::Expr(Box::new(hir::Expr {
                    hir_id: ctx.hir_id_allocator.next_id(),
                    kind: hir::ExprKind::IfElse(
                        condition,
                        Box::new(new_then_branch),
                        new_else_branches,
                    ),
                    span,
                })),
                span,
            );

            vec![if_else_stmt]
        } else {
            // Fallback: create a simple assignment statement
            vec![self.create_assignment_stmt(ctx, temp_name, expr, span)]
        }
    }

    /// Process statements and flatten complex expressions in let statements
    fn process_stmt(&mut self, stmt: &mut hir::Stmt, ctx: &mut HirOptContext) -> Vec<hir::Stmt> {
        let mut additional_stmts = Vec::new();

        match &mut stmt.kind {
            hir::StmtKind::Let(_, expr, _) => {
                // First, recursively process sub-expressions
                let mut temp_stmts = self.process_sub_expressions(expr, ctx);
                additional_stmts.append(&mut temp_stmts);

                // Then check if the expression itself needs flattening
                if !expr_can_render_as_assignment_rhs(expr) {
                    // For complex expressions in let statements, flatten them
                    if let hir::ExprKind::Loop(..) = &expr.kind {
                        // Check if this loop has break statements with values but hasn't been transformed yet
                        if self.loop_has_break_with_value(expr)
                            && !self.contains_temp_variables(expr)
                        {
                            // Special handling for loop expressions with break values - they need temp variables
                            // to capture the break value as the "return" value of the loop
                            let temp_name = self.generate_temp_var_name();
                            let span = expr.span;

                            // Create temp variable declaration
                            let temp_declaration =
                                self.create_temp_var_declaration(ctx, &temp_name, span);
                            additional_stmts.push(temp_declaration);

                            // Transform the loop body to assign to temp variable on break
                            let mut transformed_loop = (**expr).clone();
                            if let hir::ExprKind::Loop(body) = &mut transformed_loop.kind {
                                self.transform_break_statements_in_block(body, &temp_name, ctx);
                            }

                            // Create loop statement (not assigned to anything)
                            let loop_stmt = hir::Stmt::new(
                                ctx.hir_id_allocator.next_id(),
                                hir::StmtKind::Expr(Box::new(transformed_loop)),
                                span,
                            );
                            additional_stmts.push(loop_stmt);

                            // Replace the loop expression with temp variable reference
                            **expr = self.create_temp_var_path(ctx, &temp_name, span);
                            self.changes_made = true;
                        } else {
                            // Loop without break values or already processed - can be converted to statement directly
                            // Replace with wildcard since the loop doesn't produce a value
                            let span = expr.span;

                            // Create loop statement (not assigned to anything)
                            let loop_stmt = hir::Stmt::new(
                                ctx.hir_id_allocator.next_id(),
                                hir::StmtKind::Expr(Box::new((**expr).clone())),
                                span,
                            );
                            additional_stmts.push(loop_stmt);

                            // Replace the loop expression with wildcard
                            **expr = hir::Expr {
                                hir_id: ctx.hir_id_allocator.next_id(),
                                kind: hir::ExprKind::Wildcard,
                                span,
                            };
                            self.changes_made = true;
                        }
                    } else {
                        // General flattening for other complex expressions (including blocks)
                        // Skip temp variables that we created, but process blocks even if they contain temp variables
                        if !(self.is_temp_variable(expr)
                            || (self.contains_temp_variables(expr)
                                && !matches!(&expr.kind, hir::ExprKind::Block(_))))
                        {
                            let (flattened_expr, mut temp_stmts) =
                                self.flatten_expression_to_temp_var((**expr).clone(), ctx);

                            additional_stmts.append(&mut temp_stmts);
                            **expr = flattened_expr;
                            self.changes_made = true;
                        }
                    }
                }
            }
            hir::StmtKind::Expr(expr) => {
                // Special handling for assignment expressions with match on RHS (even if they contain temp vars)
                if let hir::ExprKind::Binary(hir::BinaryOpKind::Assign, _lhs, rhs) = &mut expr.kind
                    && let hir::ExprKind::Match(..) = &rhs.kind
                {
                    // Transform assignment with match: lhs = match ... ->
                    // temp_var = ...; match { arms assign to temp_var }; lhs = temp_var
                    let temp_name = self.generate_temp_var_name();
                    let span = expr.span;

                    // Create temp variable declaration
                    let temp_declaration = self.create_temp_var_declaration(ctx, &temp_name, span);
                    additional_stmts.push(temp_declaration);

                    // Transform match to assign to temp variable in each arm
                    let match_statements =
                        self.transform_match_to_statements((**rhs).clone(), &temp_name, ctx);
                    additional_stmts.extend(match_statements);

                    // Update the RHS of the assignment to use the temp variable
                    let temp_ref = self.create_temp_var_path(ctx, &temp_name, span);
                    **rhs = temp_ref;
                    self.changes_made = true;
                    return additional_stmts;
                }

                // Skip temp variables that we created
                if self.is_temp_variable(expr) || self.contains_temp_variables(expr) {
                    return additional_stmts;
                }

                // First, recursively process sub-expressions
                let mut temp_stmts = self.process_sub_expressions(expr, ctx);
                additional_stmts.append(&mut temp_stmts);

                if !expr_can_render_as_js_stmt(expr)
                    && !self.contains_temp_variables(expr)
                    && let hir::ExprKind::Block(..) | hir::ExprKind::Match(..) = &expr.kind
                {
                    // These complex expressions need to be flattened
                    let (flattened_expr, mut temp_stmts) =
                        self.flatten_expression_to_temp_var((**expr).clone(), ctx);

                    additional_stmts.append(&mut temp_stmts);
                    **expr = flattened_expr;
                    self.changes_made = true;
                }

                // Always process loop expressions in expression statements, even if they can render as JS statements
                // because they may contain break/continue expressions that need transformation
                if let hir::ExprKind::Loop(..) = &expr.kind
                    && !self.contains_temp_variables(expr)
                {
                    // Visit the loop body to transform any break/continue expressions
                    self.visit_expr(expr, ctx);
                }
            }
            hir::StmtKind::Return(Some(expr)) => {
                // Skip temp variables that we created
                if self.is_temp_variable(expr) || self.contains_temp_variables(expr) {
                    return additional_stmts;
                }

                // First, recursively process sub-expressions
                let mut temp_stmts = self.process_sub_expressions(expr, ctx);
                additional_stmts.append(&mut temp_stmts);

                // Handle complex expressions in return statements
                // NOTE: TailCall expressions in return statements should be left as-is
                // since they will be handled by the JavaScript generator's tail call optimization
                if !self.can_render_as_js_expr(expr) {
                    // Skip TailCall expressions in return statements - they're handled by codegen
                    if let hir::ExprKind::TailCall(..) = &expr.kind {
                        return additional_stmts;
                    }

                    if let hir::ExprKind::Loop(..) = &expr.kind {
                        // Check if this loop has break statements with values but hasn't been transformed yet
                        if self.loop_has_break_with_value(expr)
                            && !self.contains_temp_variables(expr)
                        {
                            // Special handling for loop expressions with break values in return statements
                            let temp_name = self.generate_temp_var_name();
                            let span = expr.span;

                            // Create temp variable declaration
                            let temp_declaration =
                                self.create_temp_var_declaration(ctx, &temp_name, span);
                            additional_stmts.push(temp_declaration);

                            // Transform the loop body to assign to temp variable on break
                            let mut transformed_loop = (**expr).clone();
                            if let hir::ExprKind::Loop(body) = &mut transformed_loop.kind {
                                self.transform_break_statements_in_block(body, &temp_name, ctx);
                            }

                            // Create loop statement (not assigned to anything)
                            let loop_stmt = hir::Stmt::new(
                                ctx.hir_id_allocator.next_id(),
                                hir::StmtKind::Expr(Box::new(transformed_loop)),
                                span,
                            );
                            additional_stmts.push(loop_stmt);

                            // Replace the loop expression with temp variable reference
                            **expr = self.create_temp_var_path(ctx, &temp_name, span);
                            self.changes_made = true;
                        } else {
                            // Loop without break values - this is unusual in return context
                            // but handle it by returning undefined after the loop
                            let span = expr.span;

                            // Create loop statement (not assigned to anything)
                            let loop_stmt = hir::Stmt::new(
                                ctx.hir_id_allocator.next_id(),
                                hir::StmtKind::Expr(Box::new((**expr).clone())),
                                span,
                            );
                            additional_stmts.push(loop_stmt);

                            // Replace the loop expression with wildcard (undefined)
                            **expr = hir::Expr {
                                hir_id: ctx.hir_id_allocator.next_id(),
                                kind: hir::ExprKind::Wildcard,
                                span,
                            };
                            self.changes_made = true;
                        }
                    } else {
                        // General flattening for other complex expressions
                        let (flattened_expr, mut temp_stmts) =
                            self.flatten_expression_to_temp_var((**expr).clone(), ctx);

                        additional_stmts.append(&mut temp_stmts);
                        **expr = flattened_expr;
                        self.changes_made = true;
                    }
                }
            }
            _ => {}
        }

        additional_stmts
    }
}

impl HirPass for SimplifiedHirJsPass {
    fn name(&self) -> &'static str {
        "SimplifiedHirJsPass"
    }

    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.changes_made = false;
        self.visit_module(module, ctx);
        // Return false to prevent additional iterations by the optimizer
        // The pass should be idempotent - not create changes on subsequent runs
        false
    }
}

impl<'hir> Visitor<'hir> for SimplifiedHirJsPass {
    type Context = HirOptContext;

    fn visit_block(&mut self, block: &'hir mut hir::Block, ctx: &mut Self::Context) {
        let mut new_stmts = Vec::new();

        for stmt in &mut block.stmts {
            // Process the statement and get any additional statements needed
            let additional_stmts = self.process_stmt(stmt, ctx);

            // Add the additional statements first
            new_stmts.extend(additional_stmts);

            // Then add the (possibly modified) original statement
            new_stmts.push(stmt.clone());
        }

        // Handle completion expression
        if let Some(completion_expr) = &mut block.expr {
            // First, check if this is a loop expression - handle it before other special cases
            if let hir::ExprKind::Loop(..) = &completion_expr.kind {
                // For loop expressions in completion position, check if they can be rendered as statements
                if !expr_can_render_as_js_stmt(completion_expr) {
                    // Check if this loop has break statements with values
                    if self.loop_has_break_with_value(completion_expr) {
                        // If it already contains temp variables, it may have been partially processed
                        // but still needs to be converted to statement position
                        if !self.contains_temp_variables(completion_expr) {
                            // Loop expressions with break values need temp variables to capture the break value
                            let temp_name = self.generate_temp_var_name();
                            let span = completion_expr.span;

                            // Create temp variable declaration
                            let temp_declaration =
                                self.create_temp_var_declaration(ctx, &temp_name, span);
                            new_stmts.push(temp_declaration);

                            // Transform the loop body to assign to temp variable on break
                            let mut transformed_loop = completion_expr.clone();
                            if let hir::ExprKind::Loop(body) = &mut transformed_loop.kind {
                                self.transform_break_statements_in_block(body, &temp_name, ctx);
                            }

                            // Create loop statement (not assigned to anything)
                            let loop_stmt = hir::Stmt::new(
                                ctx.hir_id_allocator.next_id(),
                                hir::StmtKind::Expr(Box::new(transformed_loop)),
                                span,
                            );
                            new_stmts.push(loop_stmt);

                            // Replace completion expression with temp variable reference
                            *completion_expr = self.create_temp_var_path(ctx, &temp_name, span);
                            self.changes_made = true;
                        } else {
                            // Loop already contains temp variables but still needs loop completion variable
                            // This happens when the loop body was processed first and added temp variables
                            let temp_name = self.generate_temp_var_name();
                            let span = completion_expr.span;

                            // Create temp variable declaration for loop completion
                            let temp_declaration =
                                self.create_temp_var_declaration(ctx, &temp_name, span);
                            new_stmts.push(temp_declaration);

                            // Create loop statement (not assigned to anything)
                            let loop_stmt = hir::Stmt::new(
                                ctx.hir_id_allocator.next_id(),
                                hir::StmtKind::Expr(Box::new(completion_expr.clone())),
                                span,
                            );
                            new_stmts.push(loop_stmt);

                            // Replace completion expression with temp variable reference
                            *completion_expr = self.create_temp_var_path(ctx, &temp_name, span);
                            self.changes_made = true;
                        }
                    } else {
                        // Loop without break values - convert to statement and set completion to None
                        let span = completion_expr.span;

                        // Create loop statement (not assigned to anything)
                        let loop_stmt = hir::Stmt::new(
                            ctx.hir_id_allocator.next_id(),
                            hir::StmtKind::Expr(Box::new(completion_expr.clone())),
                            span,
                        );
                        new_stmts.push(loop_stmt);

                        // Set completion expression to None instead of wildcard
                        block.expr = None;
                        self.changes_made = true;
                        return; // Early return since we've removed the completion expression
                    }
                }
            // Special handling for assignment expressions with match on RHS (even if they contain temp vars)
            } else if let hir::ExprKind::Binary(hir::BinaryOpKind::Assign, lhs, rhs) =
                &completion_expr.kind
            {
                if let hir::ExprKind::Match(..) = &rhs.kind {
                    // Special handling for assignment with match: lhs = match ... ->
                    // temp_var = ...; match { arms assign to temp_var }; lhs = temp_var; completion = temp_var
                    let temp_name = self.generate_temp_var_name();
                    let span = completion_expr.span;

                    // Create temp variable declaration
                    let temp_declaration = self.create_temp_var_declaration(ctx, &temp_name, span);
                    new_stmts.push(temp_declaration);

                    // Transform match to assign to temp variable in each arm
                    let match_statements =
                        self.transform_match_to_statements((**rhs).clone(), &temp_name, ctx);
                    new_stmts.extend(match_statements);

                    // Create assignment: lhs = temp_var
                    let temp_ref = self.create_temp_var_path(ctx, &temp_name, span);
                    let final_assignment = hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Binary(
                            hir::BinaryOpKind::Assign,
                            Box::new((**lhs).clone()),
                            Box::new(temp_ref.clone()),
                        ),
                        span,
                    };
                    let final_assignment_stmt = hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Expr(Box::new(final_assignment)),
                        span,
                    );
                    new_stmts.push(final_assignment_stmt);

                    // Replace completion expression with temp variable reference
                    *completion_expr = temp_ref;
                    self.changes_made = true;
                }
            } else {
                // Use normal transformation for other completion expressions
                if matches!(completion_expr.kind, hir::ExprKind::Wildcard) {
                    // For wildcard expressions in completion position, use None instead of creating temp variables
                    *completion_expr = hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Wildcard,
                        span: completion_expr.span,
                    };
                    // Actually, let's remove the completion expression entirely by setting it to None
                    // But since we can't set it to None here, we'll mark it for removal
                    self.changes_made = true;
                } else if !self.can_render_as_js_expr(completion_expr)
                    && !self.can_render_as_js_stmt(completion_expr)
                    && !self.contains_temp_variables(completion_expr)
                {
                    let (flattened_expr, mut temp_stmts) =
                        self.flatten_expression_to_temp_var(completion_expr.clone(), ctx);
                    new_stmts.append(&mut temp_stmts);
                    *completion_expr = flattened_expr;
                    self.changes_made = true;
                }
            }
        }

        block.stmts = new_stmts;

        // Check if completion expression should be removed (use None instead of wildcard)
        if let Some(completion_expr) = &block.expr {
            // If completion expression is a wildcard and no temp variables or other logic depend on it,
            // remove it entirely as per @topaxi's suggestion to use None instead
            if matches!(completion_expr.kind, hir::ExprKind::Wildcard) {
                // Check if this wildcard was created by a previous transformation 
                // and no other logic depends on it
                let has_dependent_logic = block.stmts.iter().any(|_stmt| {
                    // Check if any statements reference this completion expression
                    // For now, we'll be conservative and only remove standalone wildcards
                    false
                });
                
                if !has_dependent_logic {
                    block.expr = None;
                    self.changes_made = true;
                }
            }
        }

        // Use the default walking behavior to visit nested structures
        for stmt in &mut block.stmts {
            match &mut stmt.kind {
                hir::StmtKind::FunctionDeclaration(decl) => {
                    // Visit function body recursively
                    self.visit_block(&mut decl.body, ctx);
                }
                hir::StmtKind::Expr(expr) => {
                    // Visit expressions that might contain blocks
                    self.visit_expr(expr, ctx);
                }
                hir::StmtKind::Let(_, expr, _) => {
                    // Visit let expressions that might contain blocks
                    self.visit_expr(expr, ctx);
                }
                hir::StmtKind::Return(Some(expr)) => {
                    // Visit return expressions that might contain blocks
                    self.visit_expr(expr, ctx);
                }
                _ => {}
            }
        }
    }

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut Self::Context) {
        match &mut expr.kind {
            hir::ExprKind::FunctionExpression(func_expr) => {
                // Visit function body recursively to handle nested loops
                self.visit_block(&mut func_expr.body, ctx);
            }
            hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                // Visit condition first
                self.visit_expr(condition, ctx);

                // Visit then branch
                self.visit_block(then_branch, ctx);

                // Visit else branches
                for else_clause in else_branches {
                    if let Some(condition) = &mut else_clause.condition {
                        self.visit_expr(condition, ctx);
                    }
                    self.visit_block(&mut else_clause.consequence, ctx);
                }
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                // Visit scrutinee
                self.visit_expr(scrutinee, ctx);

                // Visit each match arm
                for arm in arms {
                    // Visit guard if present
                    if let Some(guard) = &mut arm.guard {
                        self.visit_expr(guard, ctx);
                    }

                    // Visit arm body
                    self.visit_block(&mut arm.block, ctx);
                }
            }
            hir::ExprKind::Loop(loop_body) => {
                // Visit loop body to catch nested expressions that need transformation
                // This is necessary to handle break/continue expressions and ensure match
                // expressions are properly structured for JavaScript generation
                self.visit_block(loop_body, ctx);
            }
            hir::ExprKind::Block(block) => {
                // Visit block content
                self.visit_block(block, ctx);
            }
            hir::ExprKind::Break(_) | hir::ExprKind::Continue => {
                // Break and continue expressions will be handled by flatten_expression_to_temp_var
                // when they're encountered in expression contexts
                // No additional processing needed here
            }
            _ => {
                // Use default walking behavior for other expressions
                walk_expr(self, expr, ctx);
            }
        }
    }
}
