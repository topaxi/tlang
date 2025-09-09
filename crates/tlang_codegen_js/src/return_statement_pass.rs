use tlang_hir::{
    Visitor, hir,
    visit::{walk_block, walk_expr, walk_stmt},
};
use tlang_hir_opt::{HirOptContext, HirPass};
use tlang_span::HirId;

use crate::expr_can_render_as_js_expr;

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

    /// Transform a block completion expression to use return statements if this is a function body block
    fn transform_completion_to_return(&mut self, block: &mut hir::Block, ctx: &mut HirOptContext) {
        // Only transform completion expressions in function body blocks, not nested blocks like loops
        if !self.function_body_blocks.contains(&block.hir_id) {
            return;
        }

        if let Some(completion_expr) = &mut block.expr {
            match &completion_expr.kind {
                hir::ExprKind::IfElse(_condition, then_branch, else_branches) => {
                    // Check if any branch contains tail calls - if so, transform to use return statements
                    let has_tail_calls =
                        self.if_else_contains_tail_calls(then_branch, else_branches);

                    if has_tail_calls {
                        // Transform the if-else to use return statements in each branch
                        self.transform_if_else_to_returns(completion_expr, ctx);

                        // Move the transformed if-else to statements and clear completion
                        let if_else_stmt = hir::Stmt::new(
                            ctx.hir_id_allocator.next_id(),
                            hir::StmtKind::Expr(Box::new(completion_expr.clone())),
                            completion_expr.span,
                        );
                        block.stmts.push(if_else_stmt);
                        block.expr = None;
                        self.changes_made = true;
                    } else {
                        // For simple if-else expressions without tail calls, wrap in return statement
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

    /// Check if an if-else expression contains tail calls in any branch
    fn if_else_contains_tail_calls(
        &self,
        then_branch: &hir::Block,
        else_branches: &[hir::ElseClause],
    ) -> bool {
        // Check then branch for tail calls
        if self.block_contains_tail_calls(then_branch) {
            return true;
        }

        // Check else branches for tail calls
        for else_clause in else_branches {
            if self.block_contains_tail_calls(&else_clause.consequence) {
                return true;
            }
        }

        false
    }

    /// Check if a block contains tail calls in its completion expression
    fn block_contains_tail_calls(&self, block: &hir::Block) -> bool {
        if let Some(completion_expr) = &block.expr {
            self.expr_contains_tail_calls(completion_expr)
        } else {
            false
        }
    }

    /// Check if an expression contains tail calls (recursively)
    fn expr_contains_tail_calls(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::TailCall(..) => true,
            hir::ExprKind::IfElse(_, then_branch, else_branches) => {
                self.if_else_contains_tail_calls(then_branch, else_branches)
            }
            hir::ExprKind::Match(_, arms) => arms
                .iter()
                .any(|arm| self.block_contains_tail_calls(&arm.block)),
            hir::ExprKind::Block(block) => self.block_contains_tail_calls(block),
            _ => false,
        }
    }

    /// Transform if-else expression to use return statements in branches that contain tail calls
    fn transform_if_else_to_returns(&mut self, expr: &mut hir::Expr, ctx: &mut HirOptContext) {
        if let hir::ExprKind::IfElse(_, then_branch, else_branches) = &mut expr.kind {
            // Transform then branch
            self.transform_branch_to_return(then_branch, ctx);

            // Transform else branches
            for else_clause in else_branches {
                self.transform_branch_to_return(&mut else_clause.consequence, ctx);
            }
        }
    }

    /// Transform a branch block to use return statements for completion expressions
    fn transform_branch_to_return(&mut self, block: &mut hir::Block, ctx: &mut HirOptContext) {
        if let Some(completion_expr) = &mut block.expr {
            // If the completion expression contains tail calls, transform it
            if self.expr_contains_tail_calls(completion_expr) {
                // Create return statement for completion expression
                let return_stmt = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Return(Some(Box::new(completion_expr.clone()))),
                    completion_expr.span,
                );
                block.stmts.push(return_stmt);
                block.expr = None;
                self.changes_made = true;
            } else {
                // For non-tail-call expressions, still create return statements
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

    /// Check if an if-else expression should be transformed to use return statements
    /// Transform match expressions to use return statements in each arm
    fn transform_match_to_returns(&mut self, expr: &mut hir::Expr, ctx: &mut HirOptContext) {
        if let hir::ExprKind::Match(_, arms) = &mut expr.kind {
            for arm in arms {
                // Transform the completion expression in each arm to a return statement
                if let Some(completion_expr) = &mut arm.block.expr {
                    match &completion_expr.kind {
                        // Break and continue statements should be moved to statement position, not wrapped in return
                        hir::ExprKind::Break(..) | hir::ExprKind::Continue => {
                            let control_flow_stmt = hir::Stmt::new(
                                ctx.hir_id_allocator.next_id(),
                                hir::StmtKind::Expr(Box::new(completion_expr.clone())),
                                completion_expr.span,
                            );
                            arm.block.stmts.push(control_flow_stmt);
                            arm.block.expr = None;
                            self.changes_made = true;
                        }
                        // Match expressions need recursive transformation
                        hir::ExprKind::Match(..) => {
                            // Recursively transform nested match expressions
                            self.transform_match_to_returns(completion_expr, ctx);

                            // After transformation, move the match to statement position
                            let match_stmt = hir::Stmt::new(
                                ctx.hir_id_allocator.next_id(),
                                hir::StmtKind::Expr(Box::new(completion_expr.clone())),
                                completion_expr.span,
                            );
                            arm.block.stmts.push(match_stmt);
                            arm.block.expr = None;
                            self.changes_made = true;
                        }
                        // All other expressions get wrapped in return statements
                        _ => {
                            let return_stmt = hir::Stmt::new(
                                ctx.hir_id_allocator.next_id(),
                                hir::StmtKind::Return(Some(Box::new(completion_expr.clone()))),
                                completion_expr.span,
                            );
                            arm.block.stmts.push(return_stmt);
                            arm.block.expr = None;
                            self.changes_made = true;
                        }
                    }
                }

                // Also apply standard completion transformation for nested complex expressions
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
                if func.body.stmts.is_empty()
                    && let Some(expr) = &func.body.expr
                    && expr_can_render_as_js_expr(expr)
                {
                    // Single expression within arrow function which can be rendered as JS expression
                    return;
                }

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

    fn visit_block(&mut self, block: &'hir mut hir::Block, ctx: &mut Self::Context) {
        // First visit all nested blocks
        walk_block(self, block, ctx);

        // Then transform the completion expression if in function context
        self.transform_completion_to_return(block, ctx);
    }
}
