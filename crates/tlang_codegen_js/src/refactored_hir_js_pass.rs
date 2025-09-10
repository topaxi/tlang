use tlang_hir::{Visitor, hir, visit::walk_expr};
use tlang_hir_opt::{HirOptContext, HirPass};

use crate::expression_transformer::{ExpressionAnalyzer, ExpressionTransformer};
use crate::transformation_strategies::{
    BinaryExpressionStrategy, BlockExpressionStrategy, BreakContinueStrategy, 
    IfElseExpressionStrategy, LoopExpressionStrategy, MatchExpressionStrategy,
};
use crate::js_expr_utils::{expr_can_render_as_assignment_rhs, expr_can_render_as_js_stmt};

/// A refactored HIR JS pass that uses the generic expression transformation framework
/// This pass focuses on flattening expressions that can't be represented in JavaScript
/// This pass assumes that return statements have already been handled by ReturnStatementPass
#[derive(Debug)]
pub struct RefactoredHirJsPass {
    transformer: ExpressionTransformer,
    changes_made: bool,
}

impl RefactoredHirJsPass {
    pub fn new() -> Self {
        let mut transformer = ExpressionTransformer::new();

        // Add transformation strategies in order of priority
        transformer.add_strategy(Box::new(BreakContinueStrategy));
        transformer.add_strategy(Box::new(MatchExpressionStrategy));
        transformer.add_strategy(Box::new(IfElseExpressionStrategy));
        transformer.add_strategy(Box::new(BlockExpressionStrategy));
        transformer.add_strategy(Box::new(BinaryExpressionStrategy));
        transformer.add_strategy(Box::new(LoopExpressionStrategy));

        Self {
            transformer,
            changes_made: false,
        }
    }

    /// Check if an expression contains loops that need transformation
    fn expression_contains_loop_needing_transformation(expr: &hir::Expr) -> bool {
        eprintln!("DEBUG: Checking if expression contains loop needing transformation: {:?} (HIR ID: {:?})", 
            std::mem::discriminant(&expr.kind), expr.hir_id);
        
        let result = match &expr.kind {
            hir::ExprKind::Loop(body) => {
                eprintln!("DEBUG: Found loop expression (HIR ID: {:?})", expr.hir_id);
                // Check if this loop needs transformation
                let needs_transformation = ExpressionAnalyzer::contains_break_with_value(expr);
                eprintln!("DEBUG: Loop needs transformation: {}", needs_transformation);
                needs_transformation
            }
            _ => {
                // Comprehensive recursive search for loops in all possible expression types
                Self::deep_search_for_loops(expr)
            }
        };
        
        eprintln!("DEBUG: Expression contains loop needing transformation: {} (HIR ID: {:?})", result, expr.hir_id);
        result
    }

    /// Deep recursive search for loops in any expression structure
    fn deep_search_for_loops(expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Loop(body) => {
                // Found a loop - check if it needs transformation
                ExpressionAnalyzer::contains_break_with_value(expr)
            }
            hir::ExprKind::Cast(inner_expr, _) => {
                eprintln!("DEBUG: Found cast expression, checking inner: {:?}", std::mem::discriminant(&inner_expr.kind));
                Self::deep_search_for_loops(inner_expr)
            }
            hir::ExprKind::Binary(_, lhs, rhs) => {
                eprintln!("DEBUG: Found binary expression, checking operands (LHS: {:?}, RHS: {:?})", 
                    lhs.hir_id, rhs.hir_id);
                Self::deep_search_for_loops(lhs) || Self::deep_search_for_loops(rhs)
            }
            hir::ExprKind::Unary(_, operand) => {
                Self::deep_search_for_loops(operand)
            }
            hir::ExprKind::Call(call) => {
                // Check callee and arguments
                Self::deep_search_for_loops(&call.callee) ||
                call.arguments.iter().any(|arg| Self::deep_search_for_loops(arg))
            }
            hir::ExprKind::Block(block) => {
                eprintln!("DEBUG: Found block expression, checking statements and completion");
                block.stmts.iter().any(|stmt| {
                    match &stmt.kind {
                        hir::StmtKind::Let(_, expr, _) | hir::StmtKind::Expr(expr) | hir::StmtKind::Return(Some(expr)) => {
                            Self::deep_search_for_loops(expr)
                        }
                        _ => false,
                    }
                }) || block.expr.as_ref().map_or(false, |expr| Self::deep_search_for_loops(expr))
            }
            hir::ExprKind::FunctionExpression(func_expr) => {
                eprintln!("DEBUG: Found function expression, checking body");
                func_expr.body.stmts.iter().any(|stmt| {
                    match &stmt.kind {
                        hir::StmtKind::Let(_, expr, _) | hir::StmtKind::Expr(expr) | hir::StmtKind::Return(Some(expr)) => {
                            Self::deep_search_for_loops(expr)
                        }
                        _ => false,
                    }
                }) || func_expr.body.expr.as_ref().map_or(false, |expr| Self::deep_search_for_loops(expr))
            }
            hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                eprintln!("DEBUG: Found if-else expression, checking branches");
                Self::deep_search_for_loops(condition) ||
                then_branch.stmts.iter().any(|stmt| {
                    match &stmt.kind {
                        hir::StmtKind::Let(_, expr, _) | hir::StmtKind::Expr(expr) | hir::StmtKind::Return(Some(expr)) => {
                            Self::deep_search_for_loops(expr)
                        }
                        _ => false,
                    }
                }) ||
                then_branch.expr.as_ref().map_or(false, |expr| Self::deep_search_for_loops(expr)) ||
                else_branches.iter().any(|else_clause| {
                    else_clause.condition.as_ref().map_or(false, |cond| Self::deep_search_for_loops(cond)) ||
                    else_clause.consequence.stmts.iter().any(|stmt| {
                        match &stmt.kind {
                            hir::StmtKind::Let(_, expr, _) | hir::StmtKind::Expr(expr) | hir::StmtKind::Return(Some(expr)) => {
                                Self::deep_search_for_loops(expr)
                            }
                            _ => false,
                        }
                    }) ||
                    else_clause.consequence.expr.as_ref().map_or(false, |expr| Self::deep_search_for_loops(expr))
                })
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                eprintln!("DEBUG: Found match expression, checking arms");
                Self::deep_search_for_loops(scrutinee) ||
                arms.iter().any(|arm| {
                    arm.guard.as_ref().map_or(false, |guard| Self::deep_search_for_loops(guard)) ||
                    arm.block.stmts.iter().any(|stmt| {
                        match &stmt.kind {
                            hir::StmtKind::Let(_, expr, _) | hir::StmtKind::Expr(expr) | hir::StmtKind::Return(Some(expr)) => {
                                Self::deep_search_for_loops(expr)
                            }
                            _ => false,
                        }
                    }) ||
                    arm.block.expr.as_ref().map_or(false, |expr| Self::deep_search_for_loops(expr))
                })
            }
            hir::ExprKind::List(elements) => {
                eprintln!("DEBUG: Found list expression, checking elements");
                elements.iter().any(|element| Self::deep_search_for_loops(element))
            }
            hir::ExprKind::FieldAccess(obj, _) => {
                eprintln!("DEBUG: Found field access expression, checking object");
                Self::deep_search_for_loops(obj)
            }
            hir::ExprKind::IndexAccess(obj, index) => {
                eprintln!("DEBUG: Found index access expression, checking object and index");
                Self::deep_search_for_loops(obj) || Self::deep_search_for_loops(index)
            }
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
                        if (!ExpressionAnalyzer::can_render_as_js_expr(lhs)
                            && !ExpressionAnalyzer::contains_temp_variables(
                                lhs,
                                self.transformer.temp_var_manager(),
                            ))
                            || (!ExpressionAnalyzer::can_render_as_js_expr(rhs)
                                && !ExpressionAnalyzer::contains_temp_variables(
                                    rhs,
                                    self.transformer.temp_var_manager(),
                                ))
                        {
                            // We have complex operands, so transform the entire expression to if-else
                            return additional_stmts;
                        }

                        // Process operands normally if both are simple
                        if !ExpressionAnalyzer::can_render_as_js_expr(lhs)
                            && !ExpressionAnalyzer::contains_temp_variables(
                                lhs,
                                self.transformer.temp_var_manager(),
                            )
                        {
                            let result = self.transformer.transform_expression((**lhs).clone(), ctx);
                            additional_stmts.extend(result.statements);
                            **lhs = result.expr;
                            self.changes_made = true;
                        }

                        if !ExpressionAnalyzer::can_render_as_js_expr(rhs)
                            && !ExpressionAnalyzer::contains_temp_variables(
                                rhs,
                                self.transformer.temp_var_manager(),
                            )
                        {
                            let result = self.transformer.transform_expression((**rhs).clone(), ctx);
                            additional_stmts.extend(result.statements);
                            **rhs = result.expr;
                            self.changes_made = true;
                        }
                    }
                    _ => {
                        // Regular binary operators - process operands normally
                        if !ExpressionAnalyzer::can_render_as_js_expr(lhs)
                            && !ExpressionAnalyzer::contains_temp_variables(
                                lhs,
                                self.transformer.temp_var_manager(),
                            )
                        {
                            let result = self.transformer.transform_expression((**lhs).clone(), ctx);
                            additional_stmts.extend(result.statements);
                            **lhs = result.expr;
                            self.changes_made = true;
                        }

                        if !ExpressionAnalyzer::can_render_as_js_expr(rhs)
                            && !ExpressionAnalyzer::contains_temp_variables(
                                rhs,
                                self.transformer.temp_var_manager(),
                            )
                        {
                            let result = self.transformer.transform_expression((**rhs).clone(), ctx);
                            additional_stmts.extend(result.statements);
                            **rhs = result.expr;
                            self.changes_made = true;
                        }
                    }
                }
            }
            hir::ExprKind::Unary(_, operand) => {
                // Process operand
                if !ExpressionAnalyzer::can_render_as_js_expr(operand)
                    && !ExpressionAnalyzer::contains_temp_variables(
                        operand,
                        self.transformer.temp_var_manager(),
                    )
                {
                    let result = self.transformer.transform_expression((**operand).clone(), ctx);
                    additional_stmts.extend(result.statements);
                    **operand = result.expr;
                    self.changes_made = true;
                }
            }
            hir::ExprKind::Call(call) => {
                // Process function call arguments
                for arg in &mut call.arguments {
                    if !ExpressionAnalyzer::can_render_as_js_expr(arg)
                        && !ExpressionAnalyzer::contains_temp_variables(
                            arg,
                            self.transformer.temp_var_manager(),
                        )
                    {
                        let result = self.transformer.transform_expression(arg.clone(), ctx);
                        additional_stmts.extend(result.statements);
                        *arg = result.expr;
                        self.changes_made = true;
                    }
                }
            }
            hir::ExprKind::List(elements) => {
                // Process list elements
                for element in elements {
                    if !ExpressionAnalyzer::can_render_as_js_expr(element)
                        && !ExpressionAnalyzer::contains_temp_variables(
                            element,
                            self.transformer.temp_var_manager(),
                        )
                    {
                        let result = self.transformer.transform_expression(element.clone(), ctx);
                        additional_stmts.extend(result.statements);
                        *element = result.expr;
                        self.changes_made = true;
                    }
                }
            }
            hir::ExprKind::FieldAccess(obj, _) => {
                // Process object expression
                if !ExpressionAnalyzer::can_render_as_js_expr(obj)
                    && !ExpressionAnalyzer::contains_temp_variables(
                        obj,
                        self.transformer.temp_var_manager(),
                    )
                {
                    // Skip break/continue expressions - they should not be flattened to temp variables
                    match &obj.kind {
                        hir::ExprKind::Break(..) | hir::ExprKind::Continue => {
                            // Leave break/continue expressions as-is
                        }
                        _ => {
                            let result =
                                self.transformer.transform_expression((**obj).clone(), ctx);
                            additional_stmts.extend(result.statements);
                            **obj = result.expr;
                            self.changes_made = true;
                        }
                    }
                }
            }
            hir::ExprKind::IndexAccess(obj, index) => {
                // Process object expression
                if !ExpressionAnalyzer::can_render_as_js_expr(obj)
                    && !ExpressionAnalyzer::contains_temp_variables(
                        obj,
                        self.transformer.temp_var_manager(),
                    )
                {
                    let result = self.transformer.transform_expression((**obj).clone(), ctx);
                    additional_stmts.extend(result.statements);
                    **obj = result.expr;
                    self.changes_made = true;
                }

                // Process index expression
                if !ExpressionAnalyzer::can_render_as_js_expr(index)
                    && !ExpressionAnalyzer::contains_temp_variables(
                        index,
                        self.transformer.temp_var_manager(),
                    )
                {
                    let result = self.transformer.transform_expression((**index).clone(), ctx);
                    additional_stmts.extend(result.statements);
                    **index = result.expr;
                    self.changes_made = true;
                }
            }
            hir::ExprKind::IfElse(condition, _then_branch, _else_branches) => {
                // Process condition first (it might contain complex expressions like loops)
                if !ExpressionAnalyzer::can_render_as_js_expr(condition)
                    && !ExpressionAnalyzer::contains_temp_variables(
                        condition,
                        self.transformer.temp_var_manager(),
                    )
                {
                    let result = self.transformer.transform_expression((**condition).clone(), ctx);
                    additional_stmts.extend(result.statements);
                    **condition = result.expr;
                    self.changes_made = true;
                }
            }
            hir::ExprKind::Match(scrutinee, _arms) => {
                // Process scrutinee first (it might contain complex expressions like loops)
                if !ExpressionAnalyzer::can_render_as_js_expr(scrutinee)
                    && !ExpressionAnalyzer::contains_temp_variables(
                        scrutinee,
                        self.transformer.temp_var_manager(),
                    )
                {
                    let result = self.transformer.transform_expression((**scrutinee).clone(), ctx);
                    additional_stmts.extend(result.statements);
                    **scrutinee = result.expr;
                    self.changes_made = true;
                }
            }
            _ => {
                // For other expressions, no sub-expression processing needed
            }
        }

        additional_stmts
    }

    /// Process statements and flatten complex expressions in let statements
    fn process_stmt(&mut self, stmt: &mut hir::Stmt, ctx: &mut HirOptContext) -> Vec<hir::Stmt> {
        let mut additional_stmts = Vec::new();
        eprintln!("DEBUG: Processing statement type: {:?} ID: {:?}", 
            std::mem::discriminant(&stmt.kind), stmt.hir_id);

        match &mut stmt.kind {
            hir::StmtKind::Let(_, expr, _) => {
                eprintln!("DEBUG: Processing let statement: {:?}, expr: {:?} (kind: {:?})", 
                    stmt.hir_id, expr.hir_id, std::mem::discriminant(&expr.kind));
                // First, recursively process sub-expressions
                let mut temp_stmts = self.process_sub_expressions(expr, ctx);
                additional_stmts.append(&mut temp_stmts);

                // Then check if the expression itself needs flattening
                eprintln!("DEBUG: Checking if expr {:?} can render as assignment RHS", expr.hir_id);
                eprintln!("DEBUG: Expression kind: {:?}", expr.kind);
                eprintln!("DEBUG: First checking if expr can render as JS expr...");
                let can_render_js = ExpressionAnalyzer::can_render_as_js_expr(expr);
                eprintln!("DEBUG: expr_can_render_as_js_expr: {}", can_render_js);
                let can_render = expr_can_render_as_assignment_rhs(expr);
                eprintln!("DEBUG: expr_can_render_as_assignment_rhs: {}", can_render);
                
                if !can_render {
                    // For complex expressions in let statements, flatten them
                    if let hir::ExprKind::Loop(body) = &expr.kind {
                        eprintln!("DEBUG: Found loop in let statement: {:?}", expr.hir_id);
                        // Check if this loop needs transformation 
                        // (has break values)
                        let needs_transformation = ExpressionAnalyzer::contains_break_with_value(expr);
                            
                        eprintln!("DEBUG: Loop needs transformation: {}", needs_transformation);
                        if needs_transformation && !ExpressionAnalyzer::contains_any_temp_variables(expr) {
                            eprintln!("DEBUG: Transforming loop in let statement");
                            // Transform using the appropriate strategy
                            let result =
                                self.transformer.transform_expression((**expr).clone(), ctx);
                            additional_stmts.extend(result.statements);
                            **expr = result.expr;
                            self.changes_made = true;
                        } else {
                            // Loop without break values or already processed - can be converted to statement directly
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
                        if !(self.transformer.temp_var_manager().is_temp_var(expr)
                            || (ExpressionAnalyzer::contains_temp_variables(
                                expr,
                                self.transformer.temp_var_manager(),
                            ) && !matches!(&expr.kind, hir::ExprKind::Block(_))))
                        {
                            let result =
                                self.transformer.transform_expression((**expr).clone(), ctx);
                            additional_stmts.extend(result.statements);
                            **expr = result.expr;
                            self.changes_made = true;
                        }
                    }
                }
            }
            hir::StmtKind::Expr(expr) => {
                eprintln!("DEBUG: Processing expression statement: {:?}", expr.hir_id);
                // Skip temp variables that we created
                if self.transformer.temp_var_manager().is_temp_var(expr)
                    || ExpressionAnalyzer::contains_temp_variables(
                        expr,
                        self.transformer.temp_var_manager(),
                    )
                {
                    return additional_stmts;
                }

                // First, recursively process sub-expressions
                let mut temp_stmts = self.process_sub_expressions(expr, ctx);
                additional_stmts.append(&mut temp_stmts);

                if !expr_can_render_as_js_stmt(expr)
                    && !ExpressionAnalyzer::contains_temp_variables(
                        expr,
                        self.transformer.temp_var_manager(),
                    )
                    && matches!(
                        &expr.kind,
                        hir::ExprKind::Block(..) | hir::ExprKind::Match(..)
                    )
                {
                    // These complex expressions need to be flattened
                    let result = self.transformer.transform_expression((**expr).clone(), ctx);
                    additional_stmts.extend(result.statements);
                    **expr = result.expr;
                    self.changes_made = true;
                }

                // Always process loop expressions in expression statements
                if let hir::ExprKind::Loop(body) = &expr.kind
                    && !ExpressionAnalyzer::contains_any_temp_variables(expr)
                {
                    // Check if the loop needs transformation
                    let needs_transformation = ExpressionAnalyzer::contains_break_with_value(expr);
                        
                    if needs_transformation {
                        // Transform the loop to capture break values or accumulator
                        let result = self.transformer.transform_expression((**expr).clone(), ctx);
                        additional_stmts.extend(result.statements);
                        **expr = result.expr;
                        self.changes_made = true;
                    } else {
                        // Visit the loop body to transform any break/continue expressions
                        self.visit_expr(expr, ctx);
                    }
                }
            }
            hir::StmtKind::Return(Some(expr)) => {
                // Skip temp variables that we created
                if self.transformer.temp_var_manager().is_temp_var(expr)
                    || ExpressionAnalyzer::contains_temp_variables(
                        expr,
                        self.transformer.temp_var_manager(),
                    )
                {
                    return additional_stmts;
                }

                // First, recursively process sub-expressions
                let mut temp_stmts = self.process_sub_expressions(expr, ctx);
                additional_stmts.append(&mut temp_stmts);

                // Handle complex expressions in return statements
                if !ExpressionAnalyzer::can_render_as_js_expr(expr) {
                    // Skip TailCall expressions in return statements - they're handled by codegen
                    if let hir::ExprKind::TailCall(..) = &expr.kind {
                        return additional_stmts;
                    }

                    if let hir::ExprKind::Loop(body) = &expr.kind {
                        // Transform loop expressions in return context
                        let needs_transformation = ExpressionAnalyzer::contains_break_with_value(expr);
                            
                        if needs_transformation
                            && !ExpressionAnalyzer::contains_temp_variables(
                                expr,
                                self.transformer.temp_var_manager(),
                            )
                        {
                            let result =
                                self.transformer.transform_expression((**expr).clone(), ctx);
                            additional_stmts.extend(result.statements);
                            **expr = result.expr;
                            self.changes_made = true;
                        } else {
                            // Loop without break values - return undefined after the loop
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
                        let result = self.transformer.transform_expression((**expr).clone(), ctx);
                        additional_stmts.extend(result.statements);
                        **expr = result.expr;
                        self.changes_made = true;
                    }
                }
            }
            _ => {}
        }

        additional_stmts
    }
}

impl Default for RefactoredHirJsPass {
    fn default() -> Self {
        Self::new()
    }
}

impl HirPass for RefactoredHirJsPass {
    fn name(&self) -> &'static str {
        "RefactoredHirJsPass"
    }

    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.changes_made = false;
        self.visit_module(module, ctx);
        
        eprintln!("DEBUG: RefactoredHirJsPass completed, changes_made: {}", self.changes_made);
        
        // Return false to prevent infinite loops - let a single pass handle all transformations
        false
    }
}

impl<'hir> Visitor<'hir> for RefactoredHirJsPass {
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
            eprintln!("DEBUG: Processing block completion expression: {:?}, kind: {:?}", completion_expr.hir_id, std::mem::discriminant(&completion_expr.kind));
            
            // Check if the expression contains loops that need transformation
            if Self::expression_contains_loop_needing_transformation(completion_expr) {
                eprintln!("DEBUG: Completion expression contains loop needing transformation");
                let result = self
                    .transformer
                    .transform_expression(completion_expr.clone(), ctx);
                new_stmts.extend(result.statements);
                *completion_expr = result.expr;
                self.changes_made = true;
            } else {
                // Check for other expressions that need transformation
                if !ExpressionAnalyzer::can_render_as_js_expr(completion_expr)
                    && !ExpressionAnalyzer::can_render_as_js_stmt(completion_expr)
                    && !ExpressionAnalyzer::contains_any_temp_variables(completion_expr)
                    && !matches!(completion_expr.kind, hir::ExprKind::Break(_))
                {
                    eprintln!("DEBUG: Block completion expression needs transformation: {:?}", completion_expr.hir_id);
                    // Transform before visiting nested structures
                    let result = self
                        .transformer
                        .transform_expression(completion_expr.clone(), ctx);
                    new_stmts.extend(result.statements);
                    *completion_expr = result.expr;
                    self.changes_made = true;
                } else {
                    eprintln!("DEBUG: Block completion expression not transformed: {:?} (js_expr: {}, js_stmt: {}, temp_vars: {}, is_break: {})",
                        completion_expr.hir_id,
                        ExpressionAnalyzer::can_render_as_js_expr(completion_expr),
                        ExpressionAnalyzer::can_render_as_js_stmt(completion_expr),
                        ExpressionAnalyzer::contains_any_temp_variables(completion_expr),
                        matches!(completion_expr.kind, hir::ExprKind::Break(_))
                    );
                }
            }
        }

        block.stmts = new_stmts;

        // Process nested structures AFTER all transformations are complete
        // This ensures that loops are transformed before being visited
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

        // Visit completion expression AFTER all statements have been processed
        if let Some(completion_expr) = &mut block.expr {
            self.visit_expr(completion_expr, ctx);
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

                    // Only move break/continue statements to statement position automatically
                    if let Some(completion_expr) = &mut arm.block.expr {
                        match &completion_expr.kind {
                            hir::ExprKind::Break(None) | hir::ExprKind::Continue => {
                                let control_flow_stmt = hir::Stmt::new(
                                    ctx.hir_id_allocator.next_id(),
                                    hir::StmtKind::Expr(Box::new(completion_expr.clone())),
                                    completion_expr.span,
                                );
                                arm.block.stmts.push(control_flow_stmt);
                                arm.block.expr = None;
                                self.changes_made = true;
                            }
                            _ => {
                                // Don't automatically move to statement position
                            }
                        }
                    }
                }
            }
            hir::ExprKind::Loop(loop_body) => {
                // Check if this loop needs transformation
                // This can happen when loops are generated during the transformation process
                let hir_id = expr.hir_id;
                let has_breaks = ExpressionAnalyzer::block_contains_break_with_value(loop_body);
                let needs_transformation = has_breaks;
                    
                eprintln!("DEBUG: Found loop in visit_expr (HIR ID: {:?}), needs transformation: {} (breaks: {})", 
                    hir_id, needs_transformation, has_breaks);
                    
                if needs_transformation {
                    // Transform the loop expression right here
                    eprintln!("DEBUG: Transforming loop in visit_expr (HIR ID: {:?})", hir_id);
                    
                    // We can't transform in place during visiting, so we need to mark this as an error
                    // The real fix is to make sure the transformation happens earlier
                    eprintln!("WARNING: Loop expression should have been transformed earlier. HIR ID: {:?}", hir_id);
                    
                    // For now, visit the loop body but mark that this is an issue
                    self.visit_block(loop_body, ctx);
                    self.changes_made = true; // Mark that we found an issue
                } else {
                    // Visit loop body for control flow handling only
                    self.visit_block(loop_body, ctx);
                }
            }
            hir::ExprKind::Block(block) => {
                // Visit block content
                self.visit_block(block, ctx);
            }
            hir::ExprKind::Break(_) | hir::ExprKind::Continue => {
                // Break and continue expressions will be handled by transformation strategies
            }
            _ => {
                // Use default walking behavior for other expressions
                walk_expr(self, expr, ctx);
            }
        }
    }
}