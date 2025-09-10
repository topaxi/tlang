use tlang_hir::hir;
use tlang_hir_opt::HirOptContext;

use crate::expression_transformer::{
    ExpressionAnalyzer, StatementBuilder, TransformResult, TransformationStrategy,
};

/// Strategy for transforming match expressions to statements
#[derive(Debug)]
pub struct MatchExpressionStrategy;

impl TransformationStrategy for MatchExpressionStrategy {
    fn should_transform(&self, expr: &hir::Expr) -> bool {
        matches!(expr.kind, hir::ExprKind::Match(..))
    }

    fn transform(
        &mut self,
        expr: hir::Expr,
        ctx: &mut HirOptContext,
        stmt_builder: &mut StatementBuilder,
    ) -> TransformResult {
        let span = expr.span;
        
        if let hir::ExprKind::Match(scrutinee, arms) = expr.kind {
            let temp_var_manager = stmt_builder.temp_var_manager();
            let temp_name = temp_var_manager.generate_name();
            let temp_declaration = temp_var_manager.create_declaration(ctx, &temp_name, span);
            let temp_ref = temp_var_manager.create_path(ctx, &temp_name, span);

            // Check if all arms have return statements
            let all_arms_have_returns = arms.iter().all(|arm| {
                if let Some(last_stmt) = arm.block.stmts.last() {
                    matches!(last_stmt.kind, hir::StmtKind::Return(_))
                } else {
                    false
                }
            });

            let statements = if all_arms_have_returns {
                // Don't assign to temp variable since all arms return
                let match_statements = Self::transform_match_to_statements(
                    hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Match(scrutinee, arms),
                        span,
                    },
                    "",
                    ctx,
                    stmt_builder,
                );
                let mut all_stmts = vec![temp_declaration];
                all_stmts.extend(match_statements);
                all_stmts
            } else {
                let match_statements = Self::transform_match_to_statements(
                    hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Match(scrutinee, arms),
                        span,
                    },
                    &temp_name,
                    ctx,
                    stmt_builder,
                );
                let mut all_stmts = vec![temp_declaration];
                all_stmts.extend(match_statements);
                all_stmts
            };

            TransformResult {
                expr: temp_ref,
                statements,
            }
        } else {
            // Should not happen if should_transform is correct
            TransformResult {
                expr,
                statements: Vec::new(),
            }
        }
    }
}

impl MatchExpressionStrategy {
    /// Check if an expression needs transformation (especially loops)
    fn expression_needs_transformation(expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Loop(_body) => {
                // Check if this loop needs transformation
                ExpressionAnalyzer::contains_break_with_value(expr)
            }
            hir::ExprKind::Match(..) => true,
            hir::ExprKind::IfElse(..) => true,
            hir::ExprKind::Block(..) => true,
            hir::ExprKind::Binary(..) => {
                // Check if any operands need transformation
                if let hir::ExprKind::Binary(_, lhs, rhs) = &expr.kind {
                    !ExpressionAnalyzer::can_render_as_js_expr(lhs) 
                        || !ExpressionAnalyzer::can_render_as_js_expr(rhs)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn transform_match_to_statements(
        expr: hir::Expr,
        temp_name: &str,
        ctx: &mut HirOptContext,
        stmt_builder: &mut StatementBuilder,
    ) -> Vec<hir::Stmt> {
        let span = expr.span;
        if let hir::ExprKind::Match(scrutinee, arms) = expr.kind {
            let mut transformed_arms = Vec::new();

            for arm in arms {
                let mut new_arm = arm;

                // Transform the completion expression in each arm
                if let Some(completion_expr) = new_arm.block.expr.take() {
                    match &completion_expr.kind {
                        hir::ExprKind::Break(Some(break_value)) => {
                            // For break with value: temp_var = value; break;
                            if !temp_name.is_empty() {
                                let assignment_stmt = stmt_builder.create_assignment(
                                    ctx,
                                    temp_name,
                                    (**break_value).clone(),
                                    span,
                                );
                                new_arm.block.stmts.push(assignment_stmt);
                            }

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
                            // For other expressions, check if they need transformation first
                            if !temp_name.is_empty() {
                                // Check if the completion expression needs transformation
                                let processed_expr = if Self::expression_needs_transformation(&completion_expr) {
                                    // Create a temporary transformer to handle nested expressions
                                    let mut temp_transformer = crate::expression_transformer::ExpressionTransformer::new();
                                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BreakContinueStrategy));
                                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::MatchExpressionStrategy));
                                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::IfElseExpressionStrategy));
                                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BlockExpressionStrategy));
                                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BinaryExpressionStrategy));
                                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::LoopExpressionStrategy));
                                    
                                    let result = temp_transformer.transform_expression(completion_expr, ctx);
                                    // Add any statements from the nested transformation
                                    new_arm.block.stmts.extend(result.statements);
                                    result.expr
                                } else {
                                    completion_expr
                                };
                                
                                let assignment_stmt = stmt_builder.create_assignment(
                                    ctx,
                                    temp_name,
                                    processed_expr,
                                    span,
                                );
                                new_arm.block.stmts.push(assignment_stmt);
                            } else {
                                // If no temp_name, check if transformation is needed
                                let processed_expr = if Self::expression_needs_transformation(&completion_expr) {
                                    // Create a temporary transformer to handle nested expressions
                                    let mut temp_transformer = crate::expression_transformer::ExpressionTransformer::new();
                                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BreakContinueStrategy));
                                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::MatchExpressionStrategy));
                                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::IfElseExpressionStrategy));
                                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BlockExpressionStrategy));
                                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BinaryExpressionStrategy));
                                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::LoopExpressionStrategy));
                                    
                                    let result = temp_transformer.transform_expression(completion_expr, ctx);
                                    // Add any statements from the nested transformation
                                    new_arm.block.stmts.extend(result.statements);
                                    result.expr
                                } else {
                                    completion_expr
                                };
                                
                                // Add the expression as a statement
                                let expr_stmt = hir::Stmt::new(
                                    ctx.hir_id_allocator.next_id(),
                                    hir::StmtKind::Expr(Box::new(processed_expr)),
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
                vec![stmt_builder.create_assignment(ctx, temp_name, expr, span)]
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
}

/// Strategy for transforming if-else expressions to statements
#[derive(Debug)]
pub struct IfElseExpressionStrategy;

impl TransformationStrategy for IfElseExpressionStrategy {
    fn should_transform(&self, expr: &hir::Expr) -> bool {
        matches!(expr.kind, hir::ExprKind::IfElse(..))
            && !ExpressionAnalyzer::can_render_as_js_expr(expr)
    }

    fn transform(
        &mut self,
        expr: hir::Expr,
        ctx: &mut HirOptContext,
        stmt_builder: &mut StatementBuilder,
    ) -> TransformResult {
        let span = expr.span;
        let temp_var_manager = stmt_builder.temp_var_manager();
        let temp_name = temp_var_manager.generate_name();
        let temp_declaration = temp_var_manager.create_declaration(ctx, &temp_name, span);
        let temp_ref = temp_var_manager.create_path(ctx, &temp_name, span);

        let statements = Self::transform_if_else_to_statements(expr, &temp_name, ctx, stmt_builder);
        let mut all_stmts = vec![temp_declaration];
        all_stmts.extend(statements);

        TransformResult {
            expr: temp_ref,
            statements: all_stmts,
        }
    }
}

impl IfElseExpressionStrategy {
    fn transform_if_else_to_statements(
        expr: hir::Expr,
        temp_name: &str,
        ctx: &mut HirOptContext,
        stmt_builder: &mut StatementBuilder,
    ) -> Vec<hir::Stmt> {
        let span = expr.span;
        if let hir::ExprKind::IfElse(condition, then_branch, else_branches) = expr.kind {
            let mut hoisted_declarations = Vec::new();
            
            // Transform branches to assign to temp variable
            let mut new_then_branch = *then_branch;
            if let Some(completion_expr) = new_then_branch.expr.take() {
                // Check if completion expression needs transformation and hoist declarations
                if BlockExpressionStrategy::expression_needs_transformation(&completion_expr) {
                    let mut temp_transformer = crate::expression_transformer::ExpressionTransformer::new();
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BreakContinueStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::MatchExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::IfElseExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BlockExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BinaryExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::LoopExpressionStrategy));
                    
                    let result = temp_transformer.transform_expression(completion_expr, ctx);
                    
                    // Extract temp variable declarations
                    for stmt in &result.statements {
                        if BlockExpressionStrategy::is_temp_var_declaration(stmt) {
                            hoisted_declarations.push(stmt.clone());
                        } else {
                            new_then_branch.stmts.push(stmt.clone());
                        }
                    }
                    
                    if !BlockExpressionStrategy::is_temp_var_reference(&result.expr, temp_name) {
                        let assignment_stmt =
                            stmt_builder.create_assignment(ctx, temp_name, result.expr, span);
                        new_then_branch.stmts.push(assignment_stmt);
                    }
                } else {
                    let assignment_stmt =
                        stmt_builder.create_assignment(ctx, temp_name, completion_expr, span);
                    new_then_branch.stmts.push(assignment_stmt);
                }
            }

            let mut new_else_branches = Vec::new();
            for else_branch in else_branches {
                let mut new_else_branch = else_branch;
                if let Some(completion_expr) = new_else_branch.consequence.expr.take() {
                    // Check if completion expression needs transformation and hoist declarations
                    if BlockExpressionStrategy::expression_needs_transformation(&completion_expr) {
                        let mut temp_transformer = crate::expression_transformer::ExpressionTransformer::new();
                        temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BreakContinueStrategy));
                        temp_transformer.add_strategy(Box::new(crate::transformation_strategies::MatchExpressionStrategy));
                        temp_transformer.add_strategy(Box::new(crate::transformation_strategies::IfElseExpressionStrategy));
                        temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BlockExpressionStrategy));
                        temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BinaryExpressionStrategy));
                        temp_transformer.add_strategy(Box::new(crate::transformation_strategies::LoopExpressionStrategy));
                        
                        let result = temp_transformer.transform_expression(completion_expr, ctx);
                        
                        // Extract temp variable declarations
                        for stmt in &result.statements {
                            if BlockExpressionStrategy::is_temp_var_declaration(stmt) {
                                hoisted_declarations.push(stmt.clone());
                            } else {
                                new_else_branch.consequence.stmts.push(stmt.clone());
                            }
                        }
                        
                        if !BlockExpressionStrategy::is_temp_var_reference(&result.expr, temp_name) {
                            let assignment_stmt =
                                stmt_builder.create_assignment(ctx, temp_name, result.expr, span);
                            new_else_branch.consequence.stmts.push(assignment_stmt);
                        }
                    } else {
                        let assignment_stmt =
                            stmt_builder.create_assignment(ctx, temp_name, completion_expr, span);
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

            // Return hoisted declarations first, then the if-else statement
            let mut all_stmts = hoisted_declarations;
            all_stmts.push(if_else_stmt);
            all_stmts
        } else {
            // Fallback: create a simple assignment statement
            vec![stmt_builder.create_assignment(ctx, temp_name, expr, span)]
        }
    }
}

/// Strategy for transforming block expressions
#[derive(Debug)]
pub struct BlockExpressionStrategy;

impl TransformationStrategy for BlockExpressionStrategy {
    fn should_transform(&self, expr: &hir::Expr) -> bool {
        matches!(expr.kind, hir::ExprKind::Block(..))
            && !ExpressionAnalyzer::can_render_as_js_expr(expr)
    }

    fn transform(
        &mut self,
        expr: hir::Expr,
        ctx: &mut HirOptContext,
        stmt_builder: &mut StatementBuilder,
    ) -> TransformResult {
        let span = expr.span;
        let temp_var_manager = stmt_builder.temp_var_manager();
        let temp_name = temp_var_manager.generate_name();
        let temp_declaration = temp_var_manager.create_declaration(ctx, &temp_name, span);
        let temp_ref = temp_var_manager.create_path(ctx, &temp_name, span);

        let statements = Self::transform_block_to_statements(expr, &temp_name, ctx, stmt_builder);
        let mut all_stmts = vec![temp_declaration];
        all_stmts.extend(statements);

        TransformResult {
            expr: temp_ref,
            statements: all_stmts,
        }
    }
}

impl BlockExpressionStrategy {
    /// Check if an expression is a reference to a specific temp variable
    fn is_temp_var_reference(expr: &hir::Expr, temp_name: &str) -> bool {
        match &expr.kind {
            hir::ExprKind::Path(path) => {
                if path.segments.len() == 1 {
                    path.segments[0].ident.as_str() == temp_name
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Check if an expression is an undefined value (indicating no meaningful completion)
    fn is_undefined_value(expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Path(path) => {
                if path.segments.len() == 1 {
                    path.segments[0].ident.as_str() == "undefined"
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Check if a statement is a temp variable declaration (starts with $hir$)
    fn is_temp_var_declaration(stmt: &hir::Stmt) -> bool {
        match &stmt.kind {
            hir::StmtKind::Let(pattern, _, _) => {
                if let Some(ident) = pattern.ident() {
                    ident.as_str().starts_with("$hir$")
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Recursively extract temp variable declarations from nested structures
    fn extract_nested_temp_declarations(stmt: hir::Stmt) -> (Vec<hir::Stmt>, hir::Stmt) {
        match &stmt.kind {
            hir::StmtKind::Expr(expr) => {
                match &expr.kind {
                    hir::ExprKind::Block(block) => {
                        // Extract temp declarations from nested block
                        let mut hoisted_declarations = Vec::new();
                        let mut new_block_stmts = Vec::new();
                        
                        for nested_stmt in &block.stmts {
                            if Self::is_temp_var_declaration(nested_stmt) {
                                hoisted_declarations.push(nested_stmt.clone());
                            } else {
                                let (nested_hoisted, final_stmt) = Self::extract_nested_temp_declarations(nested_stmt.clone());
                                hoisted_declarations.extend(nested_hoisted);
                                new_block_stmts.push(final_stmt);
                            }
                        }
                        
                        // Create new block with remaining statements
                        let new_block = hir::Block::new(
                            block.hir_id,
                            new_block_stmts,
                            block.expr.clone(),
                            block.span,
                        );
                        
                        let new_expr = hir::Expr {
                            hir_id: expr.hir_id,
                            kind: hir::ExprKind::Block(Box::new(new_block)),
                            span: expr.span,
                        };
                        
                        let new_stmt = hir::Stmt::new(
                            stmt.hir_id,
                            hir::StmtKind::Expr(Box::new(new_expr)),
                            stmt.span,
                        );
                        
                        (hoisted_declarations, new_stmt)
                    }
                    _ => (Vec::new(), stmt)
                }
            }
            _ => (Vec::new(), stmt)
        }
    }

    /// Check if an expression needs transformation (especially loops)
    fn expression_needs_transformation(expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Loop(_body) => {
                // Check if this loop needs transformation
                ExpressionAnalyzer::contains_break_with_value(expr)
            }
            hir::ExprKind::Match(..) => true,
            hir::ExprKind::IfElse(..) => true,
            hir::ExprKind::Block(..) => true,
            hir::ExprKind::Binary(..) => {
                // Check if any operands need transformation
                if let hir::ExprKind::Binary(_, lhs, rhs) = &expr.kind {
                    !ExpressionAnalyzer::can_render_as_js_expr(lhs) 
                        || !ExpressionAnalyzer::can_render_as_js_expr(rhs)
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn transform_block_to_statements(
        expr: hir::Expr,
        temp_name: &str,
        ctx: &mut HirOptContext,
        stmt_builder: &mut StatementBuilder,
    ) -> Vec<hir::Stmt> {
        let span = expr.span;
        if let hir::ExprKind::Block(block) = expr.kind {
            let mut block_stmts = block.stmts;

            // Handle completion expression by adding assignment within the block
            if let Some(completion_expr) = block.expr {
                // Check if the completion expression needs transformation first
                let (processed_completion_expr, hoisted_declarations) = if Self::expression_needs_transformation(&completion_expr) {
                    // Create a temporary transformer to handle nested expressions
                    // Use the current counter to avoid temp variable name collisions
                    let current_counter = stmt_builder.temp_var_manager().current_counter();
                    let mut temp_transformer = crate::expression_transformer::ExpressionTransformer::new_with_counter(current_counter);
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BreakContinueStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::MatchExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::IfElseExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BlockExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BinaryExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::LoopExpressionStrategy));
                    
                    let result = temp_transformer.transform_expression(completion_expr, ctx);
                    
                    // Synchronize the counter to avoid future collisions
                    stmt_builder.temp_var_manager().set_counter(temp_transformer.current_counter());
                    
                    // Extract temp variable declarations that need to be hoisted
                    let mut hoisted_declarations = Vec::new();
                    let mut remaining_statements = Vec::new();
                    
                    for stmt in result.statements {
                        if Self::is_temp_var_declaration(&stmt) {
                            hoisted_declarations.push(stmt);
                        } else {
                            remaining_statements.push(stmt);
                        }
                    }
                    
                    // Also check for nested temp variable declarations in remaining statements
                    let mut additional_hoisted = Vec::new();
                    let mut final_statements = Vec::new();
                    
                    for stmt in remaining_statements {
                        let (nested_hoisted, final_stmt) = Self::extract_nested_temp_declarations(stmt);
                        additional_hoisted.extend(nested_hoisted);
                        final_statements.push(final_stmt);
                    }
                    
                    hoisted_declarations.extend(additional_hoisted);
                    
                    // Add final statements to the block
                    block_stmts.extend(final_statements);
                    (result.expr, hoisted_declarations)
                } else {
                    (completion_expr, Vec::new())
                };
                
                // Check if the block has return statements that would make the completion assignment unreachable
                let block_ref = hir::Block::new(
                    ctx.hir_id_allocator.next_id(),
                    block_stmts.clone(),
                    Some(processed_completion_expr.clone()),
                    span,
                );

                let has_return_statements = ExpressionAnalyzer::block_contains_return_statements(&block_ref);

                if has_return_statements {
                    // Block has return statements, so completion expression assignment would be stray
                    let expr_stmt = hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Expr(Box::new(processed_completion_expr)),
                        span,
                    );
                    block_stmts.push(expr_stmt);
                } else {
                    // Only create assignment if the completion expression is not already the outer temp variable
                    // This prevents self-assignments like "$hir$0 = $hir$0"
                    if !Self::is_temp_var_reference(&processed_completion_expr, temp_name) {
                        let assignment_stmt =
                            stmt_builder.create_assignment(ctx, temp_name, processed_completion_expr, span);
                        block_stmts.push(assignment_stmt);
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

                // Return hoisted declarations first, then the block statement
                let mut all_stmts = hoisted_declarations;
                all_stmts.push(block_expr_stmt);
                all_stmts
            } else {
                // No completion expression, just create the block statement
                let modified_block = hir::Block::new(
                    ctx.hir_id_allocator.next_id(),
                    block_stmts,
                    None,
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
            }
        } else {
            // Fallback: create a simple assignment statement
            vec![stmt_builder.create_assignment(ctx, temp_name, expr, span)]
        }
    }
}

/// Strategy for transforming loop expressions
#[derive(Debug)]
pub struct LoopExpressionStrategy;

impl TransformationStrategy for LoopExpressionStrategy {
    fn should_transform(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Loop(body) => {
                // Transform loops that have break statements with values
                ExpressionAnalyzer::block_contains_break_with_value(body)
            }
            _ => false,
        }
    }

    fn transform(
        &mut self,
        expr: hir::Expr,
        ctx: &mut HirOptContext,
        stmt_builder: &mut StatementBuilder,
    ) -> TransformResult {
        let span = expr.span;
        let temp_var_manager = stmt_builder.temp_var_manager();
        let temp_name = temp_var_manager.generate_name();
        let temp_declaration = temp_var_manager.create_declaration(ctx, &temp_name, span);
        let temp_ref = temp_var_manager.create_path(ctx, &temp_name, span);

        if let hir::ExprKind::Loop(body) = &expr.kind {
            // Only handle loops with explicit break statements with values
            if ExpressionAnalyzer::block_contains_break_with_value(body) {
                let mut transformed_loop = expr.clone();
                transformed_loop.hir_id = ctx.hir_id_allocator.next_id();
                if let hir::ExprKind::Loop(body) = &mut transformed_loop.kind {
                    Self::transform_break_statements_in_block(body, &temp_name, ctx, stmt_builder);
                }

                // Create loop statement (not assigned to anything)
                let loop_stmt = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(transformed_loop)),
                    span,
                );

                let all_stmts = vec![temp_declaration, loop_stmt];

                TransformResult {
                    expr: temp_ref,
                    statements: all_stmts,
                }
            } else {
                // Loop doesn't need transformation
                TransformResult {
                    expr,
                    statements: Vec::new(),
                }
            }
        } else {
            // Should not happen if should_transform is correct
            TransformResult {
                expr,
                statements: Vec::new(),
            }
        }
    }
}

impl LoopExpressionStrategy {

    fn transform_break_statements_in_block(
        block: &mut hir::Block,
        temp_name: &str,
        ctx: &mut HirOptContext,
        stmt_builder: &mut StatementBuilder,
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
                        let assignment_stmt = stmt_builder.create_assignment(
                            ctx,
                            temp_name,
                            *break_expr.clone(),
                            span,
                        );
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
                        Self::transform_break_statements_in_expr(expr, temp_name, ctx, stmt_builder);
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
                let assignment_stmt = stmt_builder.create_assignment(
                    ctx,
                    temp_name,
                    *break_expr.clone(),
                    expr.span,
                );
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
                Self::transform_break_statements_in_expr(expr, temp_name, ctx, stmt_builder);
            }
        }
    }

    fn transform_break_statements_in_expr(
        expr: &mut hir::Expr,
        temp_name: &str,
        ctx: &mut HirOptContext,
        stmt_builder: &mut StatementBuilder,
    ) {
        match &mut expr.kind {
            hir::ExprKind::Block(block) => {
                Self::transform_break_statements_in_block(block, temp_name, ctx, stmt_builder);
            }
            hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                Self::transform_break_statements_in_expr(condition, temp_name, ctx, stmt_builder);
                Self::transform_break_statements_in_block(then_branch, temp_name, ctx, stmt_builder);
                for else_branch in else_branches {
                    if let Some(ref mut cond) = else_branch.condition {
                        Self::transform_break_statements_in_expr(cond, temp_name, ctx, stmt_builder);
                    }
                    Self::transform_break_statements_in_block(
                        &mut else_branch.consequence,
                        temp_name,
                        ctx,
                        stmt_builder,
                    );
                }
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                // Transform break statements in match scrutinee
                Self::transform_break_statements_in_expr(scrutinee, temp_name, ctx, stmt_builder);

                // Transform break statements in each match arm
                for arm in arms {
                    if let Some(ref mut guard) = arm.guard {
                        Self::transform_break_statements_in_expr(guard, temp_name, ctx, stmt_builder);
                    }
                    Self::transform_break_statements_in_block(&mut arm.block, temp_name, ctx, stmt_builder);
                }
            }
            _ => {
                // For other expression types, use the visitor to walk through them
                // Note: This is a simplified approach - a full implementation would need
                // a proper visitor that handles HIR transformation context
            }
        }
    }
}

/// Strategy for transforming break and continue expressions
#[derive(Debug)]
pub struct BreakContinueStrategy;

impl TransformationStrategy for BreakContinueStrategy {
    fn should_transform(&self, expr: &hir::Expr) -> bool {
        matches!(
            expr.kind,
            hir::ExprKind::Break(_) | hir::ExprKind::Continue
        )
    }

    fn transform(
        &mut self,
        expr: hir::Expr,
        ctx: &mut HirOptContext,
        stmt_builder: &mut StatementBuilder,
    ) -> TransformResult {
        let span = expr.span;

        match &expr.kind {
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

                TransformResult {
                    expr: wildcard_expr,
                    statements: vec![plain_break],
                }
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

                TransformResult {
                    expr: wildcard_expr,
                    statements: vec![plain_continue],
                }
            }
            hir::ExprKind::Break(Some(break_value)) => {
                // For break expressions with values, transform to:
                // let result = break value; -> temp_var = value; break; let result = temp_var;
                let temp_var_manager = stmt_builder.temp_var_manager();
                let temp_name = temp_var_manager.generate_name();
                let temp_declaration = temp_var_manager.create_declaration(ctx, &temp_name, span);
                let temp_ref = temp_var_manager.create_path(ctx, &temp_name, span);

                // Extract the break value
                let value_expr = *break_value.clone();
                let assignment_stmt =
                    stmt_builder.create_assignment(ctx, &temp_name, value_expr, span);

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

                TransformResult {
                    expr: temp_ref,
                    statements: vec![temp_declaration, assignment_stmt, plain_break],
                }
            }
            _ => {
                // Should not happen if should_transform is correct
                TransformResult {
                    expr,
                    statements: Vec::new(),
                }
            }
        }
    }
}

/// Strategy for transforming complex binary expressions that contain non-JS expressions
#[derive(Debug)]
pub struct BinaryExpressionStrategy;

impl TransformationStrategy for BinaryExpressionStrategy {
    fn should_transform(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Binary(_, lhs, rhs) => {
                // Transform binary expressions if either operand contains complex expressions
                // that can't be rendered as JS
                !ExpressionAnalyzer::can_render_as_js_expr(lhs) 
                    || !ExpressionAnalyzer::can_render_as_js_expr(rhs)
                    || ExpressionAnalyzer::contains_break_with_value(lhs)
                    || ExpressionAnalyzer::contains_break_with_value(rhs)
            }
            _ => false,
        }
    }

    fn transform(
        &mut self,
        expr: hir::Expr,
        ctx: &mut HirOptContext,
        stmt_builder: &mut StatementBuilder,
    ) -> TransformResult {
        let span = expr.span;
        
        if let hir::ExprKind::Binary(op_kind, lhs, rhs) = expr.kind {
            let temp_var_manager = stmt_builder.temp_var_manager();
            let temp_name = temp_var_manager.generate_name();
            let temp_declaration = temp_var_manager.create_declaration(ctx, &temp_name, span);
            let temp_ref = temp_var_manager.create_path(ctx, &temp_name, span);

            let statements = Self::transform_binary_to_statements(
                *lhs, 
                op_kind, 
                *rhs, 
                &temp_name, 
                ctx, 
                stmt_builder, 
                span
            );
            let mut all_stmts = vec![temp_declaration];
            all_stmts.extend(statements);

            TransformResult {
                expr: temp_ref,
                statements: all_stmts,
            }
        } else {
            // Should not happen if should_transform is correct
            TransformResult {
                expr,
                statements: Vec::new(),
            }
        }
    }
}

impl BinaryExpressionStrategy {
    fn transform_binary_to_statements(
        lhs: hir::Expr,
        op_kind: hir::BinaryOpKind,
        rhs: hir::Expr,
        temp_name: &str,
        ctx: &mut HirOptContext,
        stmt_builder: &mut StatementBuilder,
        span: tlang_span::Span,
    ) -> Vec<hir::Stmt> {
        // For assignment operations, we need special handling
        match op_kind {
            hir::BinaryOpKind::Assign => {
                // This is an assignment: lhs = rhs
                // If RHS contains complex expressions, we need to flatten them first
                if !ExpressionAnalyzer::can_render_as_js_expr(&rhs) {
                    // Create a temporary transformer to handle the RHS
                    let mut temp_transformer = crate::expression_transformer::ExpressionTransformer::new();
                    
                    // Add strategies for the nested expressions
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BreakContinueStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::MatchExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::IfElseExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BlockExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::LoopExpressionStrategy));
                    
                    // Transform the RHS
                    let rhs_result = temp_transformer.transform_expression(rhs, ctx);
                    
                    // Create the assignment with the transformed RHS
                    let assignment_expr = hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Binary(
                            hir::BinaryOpKind::Assign,
                            Box::new(lhs.clone()),
                            Box::new(rhs_result.expr),
                        ),
                        span,
                    };
                    
                    let assignment_stmt = hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Expr(Box::new(assignment_expr)),
                        span,
                    );
                    
                    // Add final assignment to temp variable
                    let final_assignment = stmt_builder.create_assignment(
                        ctx,
                        temp_name,
                        lhs, // The result of the assignment is the LHS value
                        span,
                    );
                    
                    let mut all_stmts = rhs_result.statements;
                    all_stmts.push(assignment_stmt);
                    all_stmts.push(final_assignment);
                    all_stmts
                } else {
                    // Simple assignment - create the statement directly
                    let assignment_expr = hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Binary(
                            hir::BinaryOpKind::Assign,
                            Box::new(lhs.clone()),
                            Box::new(rhs),
                        ),
                        span,
                    };
                    
                    let assignment_stmt = hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Expr(Box::new(assignment_expr)),
                        span,
                    );
                    
                    // Add final assignment to temp variable  
                    let final_assignment = stmt_builder.create_assignment(
                        ctx,
                        temp_name,
                        lhs, // The result of the assignment is the LHS value
                        span,
                    );
                    
                    vec![assignment_stmt, final_assignment]
                }
            }
            _ => {
                // For other binary operations, we need to transform complex operands first
                let mut statements = Vec::new();
                let mut final_lhs = lhs;
                let mut final_rhs = rhs;
                
                // Transform LHS if it contains complex expressions
                if !ExpressionAnalyzer::can_render_as_js_expr(&final_lhs) {
                    let mut temp_transformer = crate::expression_transformer::ExpressionTransformer::new();
                    
                    // Add strategies for the nested expressions
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BreakContinueStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::MatchExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::IfElseExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BlockExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::LoopExpressionStrategy));
                    
                    let lhs_result = temp_transformer.transform_expression(final_lhs, ctx);
                    statements.extend(lhs_result.statements);
                    final_lhs = lhs_result.expr;
                }
                
                // Transform RHS if it contains complex expressions
                if !ExpressionAnalyzer::can_render_as_js_expr(&final_rhs) {
                    let mut temp_transformer = crate::expression_transformer::ExpressionTransformer::new();
                    
                    // Add strategies for the nested expressions
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BreakContinueStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::MatchExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::IfElseExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::BlockExpressionStrategy));
                    temp_transformer.add_strategy(Box::new(crate::transformation_strategies::LoopExpressionStrategy));
                    
                    let rhs_result = temp_transformer.transform_expression(final_rhs, ctx);
                    statements.extend(rhs_result.statements);
                    final_rhs = rhs_result.expr;
                }
                
                // Create the final binary expression with transformed operands
                let binary_expr = hir::Expr {
                    hir_id: ctx.hir_id_allocator.next_id(),
                    kind: hir::ExprKind::Binary(op_kind, Box::new(final_lhs), Box::new(final_rhs)),
                    span,
                };
                
                statements.push(stmt_builder.create_assignment(ctx, temp_name, binary_expr, span));
                statements
            }
        }
    }
}