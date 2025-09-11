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
    temp_declarations: Vec<hir::Stmt>,
}

impl AnfTransformer {
    pub fn new() -> Self {
        Self {
            counter: 0,
            temp_declarations: Vec::new(),
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
        // Transform all statements
        for stmt in &mut block.stmts {
            self.transform_statement(stmt, ctx);
        }

        // Transform completion expression if present
        if let Some(completion_expr) = &mut block.expr {
            *completion_expr = self.transform_expression(completion_expr.clone(), ctx);
        }
    }

    /// Transform a statement
    fn transform_statement(&mut self, stmt: &mut hir::Stmt, ctx: &mut HirOptContext) {
        match &mut stmt.kind {
            hir::StmtKind::Let(_pattern, init, _ty) => {
                // Check if the init expression needs transformation
                if self.needs_transformation(&**init) {
                    // Extract the complex expression and transform it
                    let complex_expr = *init.clone();
                    let transformed_expr = self.transform_complex_expression(complex_expr, ctx);
                    *init = Box::new(transformed_expr);
                } else {
                    // Normal transformation for simple expressions
                    *init = Box::new(self.transform_expression(*init.clone(), ctx));
                }
            }
            hir::StmtKind::Expr(expr) => {
                *expr = Box::new(self.transform_expression(*expr.clone(), ctx));
            }
            hir::StmtKind::FunctionDeclaration(func_decl) => {
                // Reset counter for each function
                let old_counter = self.counter;
                let old_declarations = self.temp_declarations.clone();
                self.counter = 0;
                self.temp_declarations.clear();

                // Transform function body
                self.transform_block(&mut func_decl.body, ctx);

                // Add temp declarations at the beginning of function
                if !self.temp_declarations.is_empty() {
                    let mut new_stmts = self.temp_declarations.clone();
                    new_stmts.extend_from_slice(&func_decl.body.stmts);
                    func_decl.body.stmts = new_stmts;
                }

                // Restore state
                self.counter = old_counter;
                self.temp_declarations = old_declarations;
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

    /// Transform a complex expression that needs to be extracted
    fn transform_complex_expression(&mut self, expr: hir::Expr, ctx: &mut HirOptContext) -> hir::Expr {
        match expr.kind.clone() {
            hir::ExprKind::Block(block) => {
                self.transform_block_to_temp_variable(expr, &block, ctx)
            }
            hir::ExprKind::Loop(_) => {
                self.transform_loop_to_temp_variable(expr, ctx)
            }
            hir::ExprKind::Match(_, _) => {
                self.transform_match_to_temp_variable(expr, ctx)
            }
            hir::ExprKind::IfElse(_, _, _) => {
                self.transform_if_to_temp_variable(expr, ctx)
            }
            _ => self.transform_expression(expr, ctx),
        }
    }

    /// Transform a block expression to use temp variables
    fn transform_block_to_temp_variable(&mut self, expr: hir::Expr, _block: &hir::Block, ctx: &mut HirOptContext) -> hir::Expr {
        let mut expr_copy = expr.clone();
        if let hir::ExprKind::Block(ref mut block) = expr_copy.kind {
            // Create temp variable for the result
            let temp_name = self.generate_temp_name();
            let temp_path = self.create_temp_path(ctx, &temp_name, expr.span);
            let temp_declaration = self.create_temp_declaration(ctx, &temp_name, expr.span);
            
            // Transform the block to assign its completion to temp variable
            self.transform_block_to_assign_to_temp(block, &temp_name, ctx);
            
            // Create the block statement and add to current function
            let block_stmt = hir::Stmt::new(
                ctx.hir_id_allocator.next_id(),
                hir::StmtKind::Expr(Box::new(hir::Expr {
                    hir_id: expr.hir_id,
                    kind: hir::ExprKind::Block(Box::new(*block.clone())),
                    span: expr.span,
                })),
                expr.span,
            );
            
            // Add temp declaration and block statement to the function
            self.temp_declarations.push(temp_declaration);
            self.temp_declarations.push(block_stmt);
            
            // Return the temp variable reference
            return temp_path;
        }
        
        // Fallback to normal transformation
        self.transform_expression(expr, ctx)
    }

    /// Transform a loop expression to use temp variables
    fn transform_loop_to_temp_variable(&mut self, expr: hir::Expr, ctx: &mut HirOptContext) -> hir::Expr {
        // Create temp variable for the result
        let temp_name = self.generate_temp_name();
        let temp_path = self.create_temp_path(ctx, &temp_name, expr.span);
        let temp_declaration = self.create_temp_declaration(ctx, &temp_name, expr.span);
        
        // Transform the loop body to ensure break statements assign to temp variable
        let mut transformed_loop = expr.clone();
        if let hir::ExprKind::Loop(body) = &mut transformed_loop.kind {
            self.transform_loop_body_for_temp_variable(body, &temp_name, ctx);
        }
        
        // Create the loop statement
        let loop_stmt = hir::Stmt::new(
            ctx.hir_id_allocator.next_id(),
            hir::StmtKind::Expr(Box::new(transformed_loop)),
            expr.span,
        );
        
        // Add temp declaration and loop statement to the function
        self.temp_declarations.push(temp_declaration);
        self.temp_declarations.push(loop_stmt);
        
        // Return the temp variable reference
        temp_path
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
                self.transform_statement(stmt, ctx);
            }
        }
    }
    
    /// Transform expressions in loop context, handling break statements
    fn transform_expression_for_loop_temp(&mut self, expr: &mut hir::Expr, temp_name: &str, ctx: &mut HirOptContext) {
        match &mut expr.kind {
            hir::ExprKind::Break(Some(break_expr)) => {
                // Transform the break expression and ensure it assigns to temp variable
                let transformed_break_expr = self.transform_expression(*break_expr.clone(), ctx);
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
                *expr = self.transform_expression(expr.clone(), ctx);
            }
        }
    }

    /// Transform a match expression to use temp variables
    fn transform_match_to_temp_variable(&mut self, expr: hir::Expr, ctx: &mut HirOptContext) -> hir::Expr {
        // Create temp variable for the result
        let temp_name = self.generate_temp_name();
        let temp_path = self.create_temp_path(ctx, &temp_name, expr.span);
        let temp_declaration = self.create_temp_declaration(ctx, &temp_name, expr.span);
        
        // Transform the match expression by modifying its arms to assign to temp variable
        let mut transformed_match = expr.clone();
        
        if let hir::ExprKind::Match(scrutinee, arms) = &mut transformed_match.kind {
            // Transform scrutinee
            *scrutinee = Box::new(self.transform_expression(*scrutinee.clone(), ctx));
            
            // Transform each arm's block to assign to temp variable
            for arm in arms {
                // Transform the guard if present
                if let Some(guard) = &mut arm.guard {
                    *guard = self.transform_expression(guard.clone(), ctx);
                }
                
                // Transform the block to assign its completion to temp variable
                self.transform_block_to_assign_to_temp(&mut arm.block, &temp_name, ctx);
            }
        }
        
        // Create the match statement
        let match_stmt = hir::Stmt::new(
            ctx.hir_id_allocator.next_id(),
            hir::StmtKind::Expr(Box::new(transformed_match)),
            expr.span,
        );
        
        // Add temp declaration and match statement to the function
        self.temp_declarations.push(temp_declaration);
        self.temp_declarations.push(match_stmt);
        
        // Return the temp variable reference
        temp_path
    }

    /// Transform an if expression to use temp variables
    fn transform_if_to_temp_variable(&mut self, expr: hir::Expr, ctx: &mut HirOptContext) -> hir::Expr {
        // Create temp variable for the result
        let temp_name = self.generate_temp_name();
        let temp_path = self.create_temp_path(ctx, &temp_name, expr.span);
        let temp_declaration = self.create_temp_declaration(ctx, &temp_name, expr.span);
        
        // Transform the if expression by modifying its branches to assign to temp variable
        let mut transformed_if = expr.clone();
        
        if let hir::ExprKind::IfElse(cond, then_block, else_clauses) = &mut transformed_if.kind {
            // Transform condition
            *cond = Box::new(self.transform_expression(*cond.clone(), ctx));
            
            // Transform then block to assign to temp variable
            self.transform_block_to_assign_to_temp(then_block, &temp_name, ctx);
            
            // Transform else clauses to assign to temp variable
            for else_clause in else_clauses {
                if let Some(condition) = &mut else_clause.condition {
                    *condition = self.transform_expression(condition.clone(), ctx);
                }
                self.transform_block_to_assign_to_temp(&mut else_clause.consequence, &temp_name, ctx);
            }
        }
        
        // Create the if statement
        let if_stmt = hir::Stmt::new(
            ctx.hir_id_allocator.next_id(),
            hir::StmtKind::Expr(Box::new(transformed_if)),
            expr.span,
        );
        
        // Add temp declaration and if statement to the function
        self.temp_declarations.push(temp_declaration);
        self.temp_declarations.push(if_stmt);
        
        // Return the temp variable reference
        temp_path
    }
    
    /// Transform a block to assign its completion expression to a temp variable
    fn transform_block_to_assign_to_temp(&mut self, block: &mut hir::Block, temp_name: &str, ctx: &mut HirOptContext) {
        // Transform all statements
        for stmt in &mut block.stmts {
            self.transform_statement(stmt, ctx);
        }
        
        // Transform completion expression if present
        if let Some(completion_expr) = block.expr.take() {
            let span = completion_expr.span;
            
            // Transform the completion expression
            let transformed_completion = self.transform_expression(completion_expr, ctx);
            
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
        } else {
            // If no completion expression, assign wildcard/unit to temp variable
            let temp_path = self.create_temp_path(ctx, temp_name, block.span);
            let wildcard = hir::Expr {
                hir_id: ctx.hir_id_allocator.next_id(),
                kind: hir::ExprKind::Wildcard,
                span: block.span,
            };
            let assignment = self.create_assignment_expression(temp_path, wildcard);
            
            // Add assignment as a statement to the block
            let assignment_stmt = hir::Stmt::new(
                ctx.hir_id_allocator.next_id(),
                hir::StmtKind::Expr(Box::new(assignment)),
                block.span,
            );
            block.stmts.push(assignment_stmt);
            
            // Ensure no completion expression
            block.expr = None;
        }
    }

    /// Transform an expression
    fn transform_expression(&mut self, mut expr: hir::Expr, ctx: &mut HirOptContext) -> hir::Expr {
        // Check if this expression needs transformation to statement form
        if self.needs_transformation(&expr) {
            return self.transform_complex_expression(expr, ctx);
        }

        // First, recursively transform children (post-order)
        match &mut expr.kind {
            hir::ExprKind::Block(block) => {
                self.transform_block(block, ctx);
            }
            hir::ExprKind::Binary(_, left, right) => {
                *left = Box::new(self.transform_expression(*left.clone(), ctx));
                *right = Box::new(self.transform_expression(*right.clone(), ctx));
            }
            hir::ExprKind::Call(call) => {
                for arg in &mut call.arguments {
                    *arg = self.transform_expression(arg.clone(), ctx);
                }
            }
            hir::ExprKind::IfElse(cond, then_block, else_clauses) => {
                *cond = Box::new(self.transform_expression(*cond.clone(), ctx));
                self.transform_block(then_block, ctx);
                for else_clause in else_clauses {
                    if let Some(condition) = &mut else_clause.condition {
                        *condition = self.transform_expression(condition.clone(), ctx);
                    }
                    self.transform_block(&mut else_clause.consequence, ctx);
                }
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                *scrutinee = Box::new(self.transform_expression(*scrutinee.clone(), ctx));
                for arm in arms {
                    if let Some(guard) = &mut arm.guard {
                        *guard = self.transform_expression(guard.clone(), ctx);
                    }
                    self.transform_block(&mut arm.block, ctx);
                }
            }
            hir::ExprKind::Loop(body) => {
                self.transform_block(body, ctx);
            }
            _ => {
                // Other expression types don't need recursive transformation
            }
        }

        expr
    }

    /// Transform an expression in place
    fn transform_expression_in_place(&mut self, expr: &mut hir::Expr, ctx: &mut HirOptContext) {
        *expr = self.transform_expression(expr.clone(), ctx);
    }
}

impl HirPass for AnfTransformer {
    fn name(&self) -> &'static str {
        "AnfTransformer"
    }

    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        // Reset state
        self.counter = 0;
        self.temp_declarations.clear();
        
        // Transform the module's block
        self.transform_block(&mut module.block, ctx);
        
        false
    }
}