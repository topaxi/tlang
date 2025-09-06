use tlang_hir::{Visitor, hir, visit::{walk_stmt, walk_expr, walk_block}};
use tlang_hir_opt::{HirOptContext, HirPass};
use tlang_span::HirId;

/// A pass that transforms function completion expressions into explicit return statements
/// This simplifies the main HIR JS pass by ensuring all function completions use return statements
#[derive(Debug, Default)]
pub struct ReturnStatementPass {
    changes_made: bool,
    /// Track which functions we're currently inside
    function_stack: Vec<HirId>,
    /// Track which blocks are function body blocks (should have completion transformed)
    function_body_blocks: std::collections::HashSet<HirId>,
}

impl ReturnStatementPass {
    pub fn new() -> Self {
        Self {
            changes_made: false,
            function_stack: Vec::new(),
            function_body_blocks: std::collections::HashSet::new(),
        }
    }

    /// Check if we're currently inside a function
    fn is_in_function_context(&self) -> bool {
        !self.function_stack.is_empty()
    }

    /// Transform a block completion expression to use return statements if this is a function body block
    fn transform_completion_to_return(
        &mut self,
        block: &mut hir::Block,
        ctx: &mut HirOptContext,
    ) {
        // Only transform completion expressions in function body blocks, not nested blocks like loops
        if !self.function_body_blocks.contains(&block.hir_id) {
            return;
        }

        if let Some(completion_expr) = &mut block.expr {
            match &completion_expr.kind {
                hir::ExprKind::IfElse(_, _, _else_branches) => {
                    // For simple if-else expressions that were not flattened by SimplifiedHirJsPass,
                    // they can remain as completion expressions and be rendered as ternaries
                    // Only transform to return statement
                    let return_stmt = hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Return(Some(Box::new(completion_expr.clone()))),
                        completion_expr.span,
                    );
                    block.stmts.push(return_stmt);
                    block.expr = None;
                    self.changes_made = true;
                }
                hir::ExprKind::Match(..) => {
                    // Always transform match expressions to use return statements in each arm
                    // Match expressions cannot be rendered as simple JavaScript expressions
                    self.transform_match_to_returns(completion_expr, ctx);
                    
                    // Move the transformed match to statements and clear completion
                    let return_stmt = hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Expr(Box::new(completion_expr.clone())),
                        completion_expr.span,
                    );
                    block.stmts.push(return_stmt);
                    block.expr = None;
                    self.changes_made = true;
                }
                hir::ExprKind::TailCall(..) => {
                    // Transform tail calls to return statements
                    let return_stmt = hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Return(Some(Box::new(completion_expr.clone()))),
                        completion_expr.span,
                    );
                    block.stmts.push(return_stmt);
                    block.expr = None;
                    self.changes_made = true;
                }
                hir::ExprKind::Block(..) => {
                    // Transform block expressions that cannot be rendered as JavaScript expressions
                    let return_stmt = hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Return(Some(Box::new(completion_expr.clone()))),
                        completion_expr.span,
                    );
                    block.stmts.push(return_stmt);
                    block.expr = None;
                    self.changes_made = true;
                }
                _ => {
                    // For other expressions, always convert simple completion expressions to return statements
                    // This ensures function completion expressions become return statements
                    let return_stmt = hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Return(Some(Box::new(completion_expr.clone()))),
                        completion_expr.span,
                    );
                    block.stmts.push(return_stmt);
                    block.expr = None;
                    self.changes_made = true;
                }
            }
        }
    }

    /// Check if an if-else expression should be transformed to use return statements
    /// Only transform complex if-else expressions that cannot be ternaries
    fn should_transform_if_else_to_statements(&self, expr: &hir::Expr) -> bool {
        if let hir::ExprKind::IfElse(condition, then_branch, else_branches) = &expr.kind {
            // Transform if any of these conditions are true:
            // 1. Multiple else-if branches (cannot be ternary)
            if else_branches.len() > 1 {
                return true;
            }
            
            // 2. Any branch contains statements (not just simple expressions)
            if !then_branch.stmts.is_empty() {
                return true;
            }
            
            if let Some(else_branch) = else_branches.first() {
                if !else_branch.consequence.stmts.is_empty() {
                    return true;
                }
            }
            
            // 3. Condition is complex and cannot be easily rendered
            if !self.is_simple_expression(condition) {
                return true;
            }
            
            // 4. Any branch expression is complex
            if let Some(then_expr) = &then_branch.expr {
                if !self.is_simple_expression(then_expr) {
                    return true;
                }
            }
            
            if let Some(else_branch) = else_branches.first() {
                if let Some(else_expr) = &else_branch.consequence.expr {
                    if !self.is_simple_expression(else_expr) {
                        return true;
                    }
                }
            }
            
            // If all branches are simple, keep as completion expression for ternary generation
            false
        } else {
            false
        }
    }

    /// Check if an expression is simple enough to be rendered directly in JavaScript
    fn is_simple_expression(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            // These are simple and can be rendered directly
            hir::ExprKind::Literal(..) |
            hir::ExprKind::Path(..) |
            hir::ExprKind::Wildcard => true,
            
            // Simple binary operations with simple operands
            hir::ExprKind::Binary(_, lhs, rhs) => {
                self.is_simple_expression(lhs) && self.is_simple_expression(rhs)
            }
            
            // Simple unary operations
            hir::ExprKind::Unary(_, operand) => self.is_simple_expression(operand),
            
            // Simple function calls with simple arguments
            hir::ExprKind::Call(call) => {
                call.arguments.iter().all(|arg| self.is_simple_expression(arg))
            }
            
            // These expressions are complex and need statement processing
            hir::ExprKind::Block(..) |
            hir::ExprKind::Loop(..) |
            hir::ExprKind::Match(..) |
            hir::ExprKind::Break(..) |
            hir::ExprKind::Continue |
            hir::ExprKind::TailCall(..) |
            hir::ExprKind::FunctionExpression(..) => false,
            
            // Nested if-else expressions are complex
            hir::ExprKind::IfElse(..) => false,
            
            // Other cases default to false for safety
            _ => false,
        }
    }

    /// Check if a complex expression should be converted to a return statement
    fn should_convert_complex_expr_to_return(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            // Break and continue statements should not be wrapped
            hir::ExprKind::Break(..) | hir::ExprKind::Continue => false,
            // Blocks should be handled separately
            hir::ExprKind::Block(..) => false,
            // Simple expressions should be left as completion expressions
            _ if self.is_simple_expression(expr) => false,
            // Complex expressions should be converted to return statements
            _ => true,
        }
    }

    /// Transform if-else expressions to use return statements in each branch
    fn transform_if_else_to_returns(
        &mut self,
        expr: &mut hir::Expr,
        ctx: &mut HirOptContext,
    ) {
        if let hir::ExprKind::IfElse(_, then_branch, else_branches) = &mut expr.kind {
            // Transform the then branch
            self.transform_completion_to_return(then_branch, ctx);
            
            // Transform all else branches
            for else_branch in else_branches {
                self.transform_completion_to_return(&mut else_branch.consequence, ctx);
            }
        }
    }

    /// Transform match expressions to use return statements in each arm
    fn transform_match_to_returns(
        &mut self,
        expr: &mut hir::Expr,
        ctx: &mut HirOptContext,
    ) {
        if let hir::ExprKind::Match(_, arms) = &mut expr.kind {
            for arm in arms {
                self.transform_completion_to_return(&mut arm.block, ctx);
            }
        }
    }
}

impl HirPass for ReturnStatementPass {
    fn name(&self) -> &'static str {
        "ReturnStatementPass"
    }

    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.changes_made = false;
        self.function_stack.clear();
        self.function_body_blocks.clear();
        
        self.visit_module(module, ctx);
        
        self.changes_made
    }
}

impl<'hir> Visitor<'hir> for ReturnStatementPass {
    type Context = HirOptContext;

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, ctx: &mut Self::Context) {
        match &mut stmt.kind {
            hir::StmtKind::FunctionDeclaration(func) => {
                // Enter function context
                self.function_stack.push(func.hir_id);
                self.function_body_blocks.insert(func.body.hir_id);
                
                // Visit the function body
                self.visit_block(&mut func.body, ctx);
                
                // Exit function context
                self.function_stack.pop();
                self.function_body_blocks.remove(&func.body.hir_id);
            }
            hir::StmtKind::DynFunctionDeclaration(_func) => {
                // DynFunctionDeclaration doesn't have a body to visit
                // It just contains metadata about function variants
                walk_stmt(self, stmt, ctx);
            }
            _ => {
                walk_stmt(self, stmt, ctx);
            }
        }
    }

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut Self::Context) {
        match &mut expr.kind {
            hir::ExprKind::FunctionExpression(func) => {
                // Enter function context
                self.function_stack.push(func.hir_id);
                self.function_body_blocks.insert(func.body.hir_id);
                
                // Visit the function body
                self.visit_block(&mut func.body, ctx);
                
                // Exit function context
                self.function_stack.pop();
                self.function_body_blocks.remove(&func.body.hir_id);
            }
            _ => {
                walk_expr(self, expr, ctx);
            }
        }
    }

    fn visit_block(
        &mut self,
        block: &'hir mut hir::Block,
        ctx: &mut Self::Context,
    ) {
        // First visit all nested blocks
        walk_block(self, block, ctx);
        
        // Then transform the completion expression if in function context
        self.transform_completion_to_return(block, ctx);
    }
}