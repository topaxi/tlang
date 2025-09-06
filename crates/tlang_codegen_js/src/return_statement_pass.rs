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
}

impl ReturnStatementPass {
    pub fn new() -> Self {
        Self {
            changes_made: false,
            function_stack: Vec::new(),
        }
    }

    /// Check if we're currently inside a function
    fn is_in_function_context(&self) -> bool {
        !self.function_stack.is_empty()
    }

    /// Transform a block completion expression to use return statements if in function context
    fn transform_completion_to_return(
        &mut self,
        block: &mut hir::Block,
        ctx: &mut HirOptContext,
    ) {
        if !self.is_in_function_context() {
            return;
        }

        if let Some(completion_expr) = &mut block.expr {
            match &completion_expr.kind {
                hir::ExprKind::IfElse(_, _, _) => {
                    // Transform if-else expressions in function completion position to use return statements
                    self.transform_if_else_to_returns(completion_expr, ctx);
                    
                    // Move the transformed if-else to statements and clear completion
                    let return_stmt = hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Expr(Box::new(completion_expr.clone())),
                        completion_expr.span,
                    );
                    block.stmts.push(return_stmt);
                    block.expr = None;
                    self.changes_made = true;
                }
                hir::ExprKind::Match(..) => {
                    // Transform match expressions to use return statements in each arm
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
                _ => {
                    // For simple expressions, convert to return statement
                    if self.should_convert_to_return(completion_expr) {
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
    }

    /// Check if a completion expression should be converted to a return statement
    fn should_convert_to_return(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            // Break and continue statements should not be wrapped
            hir::ExprKind::Break(..) | hir::ExprKind::Continue => false,
            // Blocks should be handled recursively, not wrapped
            hir::ExprKind::Block(..) => false,
            // All other expressions should be converted to returns in function context
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
                
                // Visit the function body
                self.visit_block(&mut func.body, ctx);
                
                // Exit function context
                self.function_stack.pop();
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
                
                // Visit the function body
                self.visit_block(&mut func.body, ctx);
                
                // Exit function context
                self.function_stack.pop();
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