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
    iteration_count: usize,
}

impl SimplifiedHirJsPass {
    pub fn new() -> Self {
        Self {
            temp_var_counter: 0,
            changes_made: false,
            iteration_count: 0,
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

    /// Flatten expressions that cannot be represented in JavaScript to temp variables
    fn flatten_expression_to_temp_var(
        &mut self,
        expr: hir::Expr,
        ctx: &mut HirOptContext,
    ) -> (hir::Expr, Vec<hir::Stmt>) {
        let temp_name = self.generate_temp_var_name();
        let span = expr.span;

        // Create temp variable declaration
        let temp_declaration = self.create_temp_var_declaration(ctx, &temp_name, span);

        // Create temp variable reference
        let temp_ref = self.create_temp_var_path(ctx, &temp_name, span);

        // Handle complex expressions specially
        match &expr.kind {
            hir::ExprKind::Block(_block) => {
                // For block expressions, transform the block to assign to temp variable
                let statements = self.transform_block_to_statements(expr, &temp_name, ctx);
                let mut all_stmts = vec![temp_declaration];
                all_stmts.extend(statements);
                (temp_ref, all_stmts)
            }
            hir::ExprKind::Loop(..) => {
                // For loop expressions, use specialized loop transformation
                let (_, statements) = self.transform_loop_expression(expr, ctx);
                let mut all_stmts = vec![temp_declaration];
                all_stmts.extend(statements);
                (temp_ref, all_stmts)
            }
            hir::ExprKind::Match(_, arms) => {
                // Check if all arms have return statements - if so, no temp variable is needed
                let all_arms_have_returns = arms.iter().all(|arm| {
                    if let Some(last_stmt) = arm.block.stmts.last() {
                        matches!(last_stmt.kind, hir::StmtKind::Return(_))
                    } else {
                        false
                    }
                });

                if all_arms_have_returns {
                    // No temp variable needed, just return the match expression as statements
                    let statements = self.transform_match_to_statements(expr, &temp_name, ctx);
                    // Return a dummy temp ref (won't be used) and no temp declaration
                    let dummy_temp_ref = self.create_temp_var_path(ctx, &temp_name, span);
                    (dummy_temp_ref, statements)
                } else {
                    // For match expressions, transform to proper statements
                    let statements = self.transform_match_to_statements(expr, &temp_name, ctx);
                    let mut all_stmts = vec![temp_declaration];
                    all_stmts.extend(statements);
                    (temp_ref, all_stmts)
                }
            }
            hir::ExprKind::IfElse(..) => {
                // For if-else expressions, transform to proper statements
                let statements = self.transform_if_else_to_statements(expr, &temp_name, ctx);
                let mut all_stmts = vec![temp_declaration];
                all_stmts.extend(statements);
                (temp_ref, all_stmts)
            }
            hir::ExprKind::Break(Some(break_value)) => {
                // For break expressions with values, transform specially:
                // let result = break value; -> temp_var = value; break; let result = temp_var;
                
                // Extract the break value without extra wrapping
                let value_expr = *break_value.clone();
                let assignment_stmt = self.create_assignment_stmt(ctx, &temp_name, value_expr, span);
                
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
                
                (temp_ref, vec![temp_declaration, assignment_stmt, plain_break])
            }
            hir::ExprKind::Break(None) => {
                // For break expressions without values, just create the break statement
                let plain_break = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(expr)),
                    span,
                );
                
                // Return a dummy temp ref (won't be used) and no temp declaration
                let dummy_temp_ref = self.create_temp_var_path(ctx, &temp_name, span);
                (dummy_temp_ref, vec![plain_break])
            }
            hir::ExprKind::Continue => {
                // For continue expressions, just create the continue statement
                let plain_continue = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(expr)),
                    span,
                );
                
                // Return a dummy temp ref (won't be used) and no temp declaration
                let dummy_temp_ref = self.create_temp_var_path(ctx, &temp_name, span);
                (dummy_temp_ref, vec![plain_continue])
            }
            _ => {
                // For simple expressions, create assignment statement
                let assignment_stmt = self.create_assignment_stmt(ctx, &temp_name, expr, span);
                (temp_ref, vec![temp_declaration, assignment_stmt])
            }
        }
    }

    /// Check if an expression can be represented as a JavaScript expression
    fn can_render_as_js_expr(&self, expr: &hir::Expr) -> bool {
        expr_can_render_as_js_expr(expr)
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
            hir::ExprKind::Binary(_, lhs, rhs) => {
                // Process left operand
                if !self.can_render_as_js_expr(lhs) && !self.contains_temp_variables(lhs) {
                    let (flattened_expr, mut temp_stmts) =
                        self.flatten_expression_to_temp_var((**lhs).clone(), ctx);
                    additional_stmts.append(&mut temp_stmts);
                    **lhs = flattened_expr;
                    self.changes_made = true;
                }

                // Process right operand
                if !self.can_render_as_js_expr(rhs) && !self.contains_temp_variables(rhs) {
                    let (flattened_expr, mut temp_stmts) =
                        self.flatten_expression_to_temp_var((**rhs).clone(), ctx);
                    additional_stmts.append(&mut temp_stmts);
                    **rhs = flattened_expr;
                    self.changes_made = true;
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
                    let (flattened_expr, mut temp_stmts) =
                        self.flatten_expression_to_temp_var((**obj).clone(), ctx);
                    additional_stmts.append(&mut temp_stmts);
                    **obj = flattened_expr;
                    self.changes_made = true;
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
            _ => {
                // For other expressions, no sub-expression processing needed
            }
        }

        additional_stmts
    }

    /// Transform loop expressions to use temp variables
    fn transform_loop_expression(
        &mut self,
        loop_expr: hir::Expr,
        ctx: &mut HirOptContext,
    ) -> (hir::Expr, Vec<hir::Stmt>) {
        let temp_name = self.generate_temp_var_name();
        let span = loop_expr.span;

        // Create temp variable declaration
        let temp_declaration = self.create_temp_var_declaration(ctx, &temp_name, span);

        // Transform the loop body to assign to temp variable on break
        let mut transformed_loop = loop_expr.clone();
        if let hir::ExprKind::Loop(body) = &mut transformed_loop.kind {
            self.transform_break_statements_in_block(body, &temp_name, ctx);
        }

        // Create loop statement
        let loop_stmt = hir::Stmt::new(
            ctx.hir_id_allocator.next_id(),
            hir::StmtKind::Expr(Box::new(transformed_loop)),
            span,
        );

        // Create temp variable reference
        let temp_ref = self.create_temp_var_path(ctx, &temp_name, span);

        (temp_ref, vec![temp_declaration, loop_stmt])
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
                let assignment_stmt =
                    self.create_assignment_stmt(ctx, temp_name, completion_expr, span);
                block_stmts.push(assignment_stmt);
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
            let all_arms_have_returns = arms.iter().all(|arm| {
                if let Some(last_stmt) = arm.block.stmts.last() {
                    matches!(last_stmt.kind, hir::StmtKind::Return(_))
                } else {
                    false
                }
            });

            // If all arms have return statements, don't create assignment statements
            if all_arms_have_returns {
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
                    let assignment_stmt =
                        self.create_assignment_stmt(ctx, temp_name, completion_expr, span);
                    new_arm.block.stmts.push(assignment_stmt);
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
            // Fallback: create a simple assignment statement
            vec![self.create_assignment_stmt(ctx, temp_name, expr, span)]
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
                // Skip temp variables that we created
                if self.is_temp_variable(expr) || self.contains_temp_variables(expr) {
                    return additional_stmts;
                }

                // First, recursively process sub-expressions
                let mut temp_stmts = self.process_sub_expressions(expr, ctx);
                additional_stmts.append(&mut temp_stmts);

                // Then check if the expression itself needs flattening
                if !expr_can_render_as_assignment_rhs(expr) && !self.contains_temp_variables(expr) {
                    // For complex expressions in let statements, flatten them
                    if let hir::ExprKind::Loop(..) = &expr.kind {
                        // Special handling for loop expressions
                        let (flattened_expr, mut temp_stmts) =
                            self.transform_loop_expression((**expr).clone(), ctx);

                        additional_stmts.append(&mut temp_stmts);
                        **expr = flattened_expr;
                        self.changes_made = true;
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
            hir::StmtKind::Expr(expr) => {
                // Skip temp variables that we created
                if self.is_temp_variable(expr) || self.contains_temp_variables(expr) {
                    return additional_stmts;
                }

                // First, recursively process sub-expressions
                let mut temp_stmts = self.process_sub_expressions(expr, ctx);
                additional_stmts.append(&mut temp_stmts);

                // Handle complex expressions in expression statements
                // Use statement-specific logic for standalone expressions
                if !expr_can_render_as_js_stmt(expr) && !self.contains_temp_variables(expr) {
                    if let hir::ExprKind::Loop(..) = &expr.kind {
                        // Special handling for loop expressions
                        let (flattened_expr, mut temp_stmts) =
                            self.transform_loop_expression((**expr).clone(), ctx);

                        additional_stmts.append(&mut temp_stmts);
                        **expr = flattened_expr;
                        self.changes_made = true;
                    } else if let hir::ExprKind::Block(..) | hir::ExprKind::Match(..) = &expr.kind {
                        // These complex expressions need to be flattened
                        let (flattened_expr, mut temp_stmts) =
                            self.flatten_expression_to_temp_var((**expr).clone(), ctx);

                        additional_stmts.append(&mut temp_stmts);
                        **expr = flattened_expr;
                        self.changes_made = true;
                    }
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
                if !self.can_render_as_js_expr(expr) && !self.contains_temp_variables(expr) {
                    // Skip TailCall expressions in return statements - they're handled by codegen
                    if let hir::ExprKind::TailCall(..) = &expr.kind {
                        return additional_stmts;
                    }

                    if let hir::ExprKind::Loop(..) = &expr.kind {
                        // Special handling for loop expressions
                        let (flattened_expr, mut temp_stmts) =
                            self.transform_loop_expression((**expr).clone(), ctx);

                        additional_stmts.append(&mut temp_stmts);
                        **expr = flattened_expr;
                        self.changes_made = true;
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
        // Only run once - this pass should be idempotent and complete its work in one pass
        if self.iteration_count > 0 {
            return false;
        }

        self.iteration_count += 1;
        self.changes_made = false;
        self.visit_module(module, ctx);
        self.changes_made
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
        if let Some(completion_expr) = &mut block.expr
            && !self.can_render_as_js_expr(completion_expr)
            && !self.contains_temp_variables(completion_expr)
        {
            if let hir::ExprKind::Loop(..) = &completion_expr.kind {
                // Special handling for loop expressions in block completion position
                // Loops should become statements, not be assigned to temp variables
                let loop_stmt = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(completion_expr.clone())),
                    completion_expr.span,
                );
                new_stmts.push(loop_stmt);
                block.expr = None; // Remove the completion expression since it's now a statement
                self.changes_made = true;
            } else {
                // General flattening for other complex expressions
                let (flattened_expr, mut temp_stmts) =
                    self.flatten_expression_to_temp_var(completion_expr.clone(), ctx);

                new_stmts.append(&mut temp_stmts);
                *completion_expr = flattened_expr;
                self.changes_made = true;
            }
        }

        block.stmts = new_stmts;

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
                // Visit loop body to catch nested expressions
                self.visit_block(loop_body, ctx);
            }
            hir::ExprKind::Block(block) => {
                // Visit block content
                self.visit_block(block, ctx);
            }
            _ => {
                // Use default walking behavior for other expressions
                walk_expr(self, expr, ctx);
            }
        }
    }
}
