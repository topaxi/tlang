//! ANF-like expression transformer for JavaScript code generation
//!
//! This module implements a simplified Administrative Normal Form (ANF) transformation
//! that converts HIR expressions which cannot be directly represented in JavaScript
//! into statements with temporary variables.

use tlang_hir::hir;
use tlang_hir_opt::{HirOptContext, HirPass};
use tlang_span::Span;
use tlang_ast::node::Ident;

/// ANF-like transformer that converts complex expressions to statements with temp variables
pub struct AnfTransformer {
    counter: usize,
    /// Track which blocks are function body blocks (should have completion transformed to return)
    function_body_blocks: std::collections::HashSet<tlang_span::HirId>,
}

impl AnfTransformer {
    pub fn new() -> Self {
        Self {
            counter: 0,
            function_body_blocks: std::collections::HashSet::new(),
        }
    }

    /// Generate a new temp variable name
    fn generate_temp_name(&mut self) -> String {
        let name = format!("$hir${}", self.counter);
        self.counter += 1;
        name
    }

    /// Create a temp variable declaration
    fn create_temp_declaration(&self, ctx: &mut HirOptContext, name: &str, span: Span) -> hir::Stmt {
        let hir_id = ctx.hir_id_allocator.next_id();
        let ident = Ident::new(name, span);
        let pattern = hir::Pat {
            kind: hir::PatKind::Identifier(ctx.hir_id_allocator.next_id(), Box::new(ident)),
            span,
        };

        let wildcard_expr = hir::Expr {
            hir_id: ctx.hir_id_allocator.next_id(),
            kind: hir::ExprKind::Wildcard,
            span,
        };

        let default_ty = hir::Ty {
            kind: hir::TyKind::Unknown,
            span,
        };

        hir::Stmt::new(
            hir_id,
            hir::StmtKind::Let(Box::new(pattern), Box::new(wildcard_expr), Box::new(default_ty)),
            span,
        )
    }

    /// Create a temp variable path expression
    fn create_temp_path(&self, ctx: &mut HirOptContext, name: &str, span: Span) -> hir::Expr {
        let hir_id = ctx.hir_id_allocator.next_id();
        let ident = Ident::new(name, span);
        let path_segment = hir::PathSegment { ident };
        let path = hir::Path::new(vec![path_segment], span);

        hir::Expr {
            hir_id,
            kind: hir::ExprKind::Path(Box::new(path)),
            span,
        }
    }

    /// Create an assignment expression: (temp_var = value)
    fn create_assignment_expression(&self, temp_var: hir::Expr, value: hir::Expr) -> hir::Expr {
        hir::Expr {
            hir_id: temp_var.hir_id,
            span: value.span,
            kind: hir::ExprKind::Binary(
                hir::BinaryOpKind::Assign,
                Box::new(temp_var),
                Box::new(value),
            ),
        }
    }

    /// Transform expressions in a block
    fn transform_block(&mut self, block: &mut hir::Block, ctx: &mut HirOptContext) {
        let mut temp_declarations = Vec::new();
        
        // Transform all statements
        for stmt in &mut block.stmts {
            self.transform_statement(stmt, ctx, &mut temp_declarations);
        }

        // Transform completion expression if present
        if let Some(completion_expr) = &mut block.expr {
            // Check if this is a function body block that should have return statements
            if self.function_body_blocks.contains(&block.hir_id) {
                // Transform to return statement
                let transformed_expr = self.transform_expression(completion_expr.clone(), ctx, &mut temp_declarations);
                
                // Add temp declarations first
                if !temp_declarations.is_empty() {
                    temp_declarations.extend_from_slice(&block.stmts);
                    block.stmts = temp_declarations;
                }
                
                // Create return statement and add to the end of the block
                let return_stmt = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Return(Some(Box::new(transformed_expr))),
                    completion_expr.span,
                );
                
                block.stmts.push(return_stmt);
                block.expr = None; // Remove completion expression
                return; // Early return to avoid adding temp_declarations again
            } else {
                // Normal completion expression transformation
                *completion_expr = self.transform_expression(completion_expr.clone(), ctx, &mut temp_declarations);
            }
        }
        
        // Add temp declarations at the beginning of the block
        if !temp_declarations.is_empty() {
            temp_declarations.extend_from_slice(&block.stmts);
            block.stmts = temp_declarations;
        }
    }

    /// Transform a statement
    fn transform_statement(&mut self, stmt: &mut hir::Stmt, ctx: &mut HirOptContext, temp_declarations: &mut Vec<hir::Stmt>) {
        match &mut stmt.kind {
            hir::StmtKind::Let(_pattern, init, _ty) => {
                *init = Box::new(self.transform_expression(*init.clone(), ctx, temp_declarations));
            }
            hir::StmtKind::Expr(expr) => {
                *expr = Box::new(self.transform_expression(*expr.clone(), ctx, temp_declarations));
            }
            hir::StmtKind::FunctionDeclaration(func_decl) => {
                // Mark the function body as a function body block (for return statement transformation)
                self.function_body_blocks.insert(func_decl.body.hir_id);
                
                // Transform function body
                self.transform_block(&mut func_decl.body, ctx);
            }
            _ => {
                // Other statement types don't need transformation
            }
        }
    }

    /// Check if an expression needs transformation
    fn needs_transformation(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Block(_) => true, // All blocks need statement form
            hir::ExprKind::Loop(_) => true,
            hir::ExprKind::Match(_, _) => true,
            hir::ExprKind::IfElse(_, _, _) => true,
            _ => false,
        }
    }

    /// Transform an expression
    fn transform_expression(&mut self, expr: hir::Expr, ctx: &mut HirOptContext, temp_declarations: &mut Vec<hir::Stmt>) -> hir::Expr {
        // Check if this expression needs transformation to statement form
        if self.needs_transformation(&expr) {
            return self.transform_complex_expression(expr, ctx, temp_declarations);
        }

        // First, recursively transform children (post-order)
        let mut expr = expr;
        match &mut expr.kind {
            hir::ExprKind::Binary(_, left, right) => {
                *left = Box::new(self.transform_expression(*left.clone(), ctx, temp_declarations));
                *right = Box::new(self.transform_expression(*right.clone(), ctx, temp_declarations));
            }
            hir::ExprKind::Call(call) => {
                for arg in &mut call.arguments {
                    *arg = self.transform_expression(arg.clone(), ctx, temp_declarations);
                }
            }
            _ => {
                // Other expression types don't need recursive transformation
            }
        }

        expr
    }

    /// Transform a complex expression that needs to be extracted
    fn transform_complex_expression(&mut self, expr: hir::Expr, ctx: &mut HirOptContext, temp_declarations: &mut Vec<hir::Stmt>) -> hir::Expr {
        // Generate temp variable for the result
        let temp_name = self.generate_temp_name();
        let temp_path = self.create_temp_path(ctx, &temp_name, expr.span);
        let temp_declaration = self.create_temp_declaration(ctx, &temp_name, expr.span);
        
        // Add temp declaration
        temp_declarations.push(temp_declaration);
        
        // Transform the expression based on its type
        match expr.kind {
            hir::ExprKind::Block(mut block) => {
                // Transform the block to assign its completion to temp variable
                self.transform_block_to_assign_to_temp(&mut block, &temp_name, ctx);
                
                // Create the block statement
                let block_stmt = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: expr.hir_id,
                        kind: hir::ExprKind::Block(Box::new(*block)),
                        span: expr.span,
                    })),
                    expr.span,
                );
                
                temp_declarations.push(block_stmt);
                temp_path
            }
            hir::ExprKind::Loop(mut body) => {
                // Transform loop body to handle break statements with temp variables
                self.transform_loop_body_for_temp_variable(&mut body, &temp_name, ctx);
                
                // Create the loop statement
                let loop_stmt = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: expr.hir_id,
                        kind: hir::ExprKind::Loop(body),
                        span: expr.span,
                    })),
                    expr.span,
                );
                
                temp_declarations.push(loop_stmt);
                temp_path
            }
            hir::ExprKind::Match(scrutinee, mut arms) => {
                // Transform each arm's block to assign to temp variable
                for arm in &mut arms {
                    self.transform_block_to_assign_to_temp(&mut arm.block, &temp_name, ctx);
                }
                
                // Create the match statement
                let match_stmt = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: expr.hir_id,
                        kind: hir::ExprKind::Match(scrutinee, arms),
                        span: expr.span,
                    })),
                    expr.span,
                );
                
                temp_declarations.push(match_stmt);
                temp_path
            }
            hir::ExprKind::IfElse(cond, mut then_block, mut else_clauses) => {
                // Transform then block to assign to temp variable
                self.transform_block_to_assign_to_temp(&mut then_block, &temp_name, ctx);
                
                // Transform else clauses to assign to temp variable
                for else_clause in &mut else_clauses {
                    self.transform_block_to_assign_to_temp(&mut else_clause.consequence, &temp_name, ctx);
                }
                
                // Create the if statement
                let if_stmt = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(hir::Expr {
                        hir_id: expr.hir_id,
                        kind: hir::ExprKind::IfElse(cond, Box::new(*then_block), else_clauses),
                        span: expr.span,
                    })),
                    expr.span,
                );
                
                temp_declarations.push(if_stmt);
                temp_path
            }
            _ => {
                // For other expression types, just return the expression as-is
                expr
            }
        }
    }
    
    /// Transform loop body to handle break statements with temp variables
    fn transform_loop_body_for_temp_variable(&mut self, body: &mut hir::Block, temp_name: &str, ctx: &mut HirOptContext) {
        // Transform all statements, looking for break statements
        for stmt in &mut body.stmts {
            self.transform_statement_for_loop_temp(stmt, temp_name, ctx);
        }
        
        // Transform completion expression if present
        if let Some(completion_expr) = &mut body.expr {
            self.transform_expression_for_loop_temp(completion_expr, temp_name, ctx);
        }
    }
    
    /// Transform statements in loop context, handling break statements
    fn transform_statement_for_loop_temp(&mut self, stmt: &mut hir::Stmt, temp_name: &str, ctx: &mut HirOptContext) {
        match &mut stmt.kind {
            hir::StmtKind::Expr(expr) => {
                self.transform_expression_for_loop_temp(expr, temp_name, ctx);
            }
            _ => {
                // Regular transformation for other statement types
                let mut temp_declarations = Vec::new();
                self.transform_statement(stmt, ctx, &mut temp_declarations);
                // Note: We don't handle temp_declarations here as loops should be simple
            }
        }
    }
    
    /// Transform expressions in loop context, handling break statements
    fn transform_expression_for_loop_temp(&mut self, expr: &mut hir::Expr, temp_name: &str, ctx: &mut HirOptContext) {
        match &mut expr.kind {
            hir::ExprKind::Break(Some(break_expr)) => {
                // Transform the break expression and ensure it assigns to temp variable
                let mut temp_declarations = Vec::new();
                let transformed_break_expr = self.transform_expression(*break_expr.clone(), ctx, &mut temp_declarations);
                let temp_path = self.create_temp_path(ctx, temp_name, break_expr.span);
                let assignment = self.create_assignment_expression(temp_path, transformed_break_expr);
                
                // Create a new break with the assignment
                *expr = hir::Expr {
                    hir_id: expr.hir_id,
                    kind: hir::ExprKind::Break(Some(Box::new(assignment))),
                    span: expr.span,
                };
            }
            _ => {
                // Regular transformation
                let mut temp_declarations = Vec::new();
                *expr = self.transform_expression(expr.clone(), ctx, &mut temp_declarations);
            }
        }
    }

    /// Transform a block to assign its completion expression to a temp variable
    fn transform_block_to_assign_to_temp(&mut self, block: &mut hir::Block, temp_name: &str, ctx: &mut HirOptContext) {
        // Transform all statements first
        let mut temp_declarations = Vec::new();
        for stmt in &mut block.stmts {
            self.transform_statement(stmt, ctx, &mut temp_declarations);
        }
        
        // Add any temp declarations to the beginning of the block
        if !temp_declarations.is_empty() {
            temp_declarations.extend_from_slice(&block.stmts);
            block.stmts = temp_declarations;
        }
        
        // Transform completion expression if present
        if let Some(completion_expr) = block.expr.take() {
            let span = completion_expr.span;
            
            // Transform the completion expression
            let mut completion_temp_declarations = Vec::new();
            let transformed_completion = self.transform_expression(completion_expr, ctx, &mut completion_temp_declarations);
            
            // Add any temp declarations from completion expression
            block.stmts.extend(completion_temp_declarations);
            
            // Create temp variable path
            let temp_path = self.create_temp_path(ctx, temp_name, span);
            
            // Create assignment expression: temp_var = completion_expr
            let assignment = self.create_assignment_expression(temp_path, transformed_completion);
            
            // Add assignment as a statement to the block
            let assignment_stmt = hir::Stmt::new(
                ctx.hir_id_allocator.next_id(),
                hir::StmtKind::Expr(Box::new(assignment)),
                span,
            );
            block.stmts.push(assignment_stmt);
            
            // Remove completion expression (it's now a statement)
            block.expr = None;
        }
        // If no completion expression, no need to assign anything
    }
}

impl HirPass for AnfTransformer {
    fn name(&self) -> &'static str {
        "AnfTransformer"
    }

    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        // Reset state
        self.counter = 0;
        self.function_body_blocks.clear();
        
        // Transform the module's block
        self.transform_block(&mut module.block, ctx);
        
        false
    }
}