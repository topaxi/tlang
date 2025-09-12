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
        let mut transformed_statements = Vec::new();
        
        // Transform all statements in order, collecting temp declarations appropriately
        for stmt in &mut block.stmts {
            let mut stmt_temp_declarations = Vec::new();
            self.transform_statement(stmt, ctx, &mut stmt_temp_declarations);
            
            // Add temp declarations immediately before the statement that needs them
            transformed_statements.extend(stmt_temp_declarations);
            transformed_statements.push(stmt.clone());
        }

        // Transform completion expression if present
        if let Some(completion_expr) = &mut block.expr {
            // Check if this is a function body block that should have return statements
            if self.function_body_blocks.contains(&block.hir_id) {
                // Transform to return statement
                let mut expr_temp_declarations = Vec::new();
                let transformed_expr = self.transform_expression(completion_expr.clone(), ctx, &mut expr_temp_declarations);
                
                // Add temp declarations before the return statement
                transformed_statements.extend(expr_temp_declarations);
                
                // Create return statement and add to the end of the block
                let return_stmt = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Return(Some(Box::new(transformed_expr))),
                    completion_expr.span,
                );
                
                transformed_statements.push(return_stmt);
                block.stmts = transformed_statements;
                block.expr = None; // Remove completion expression
                return; // Early return
            } else {
                // Normal completion expression transformation
                let mut expr_temp_declarations = Vec::new();
                *completion_expr = self.transform_expression(completion_expr.clone(), ctx, &mut expr_temp_declarations);
                
                // Add temp declarations at the end of the existing statements
                transformed_statements.extend(expr_temp_declarations);
            }
        }
        
        // Update block statements with transformed ones
        block.stmts = transformed_statements;
    }

    /// Transform a statement
    fn transform_statement(&mut self, stmt: &mut hir::Stmt, ctx: &mut HirOptContext, temp_declarations: &mut Vec<hir::Stmt>) {
        match &mut stmt.kind {
            hir::StmtKind::Let(_pattern, init, _ty) => {
                // Special handling for break expressions in let statements
                if let hir::ExprKind::Break(Some(_)) = &init.kind {
                    // Convert break expression to assignment + break statements
                    self.transform_break_in_let_statement(stmt, ctx, temp_declarations);
                } else {
                    *init = Box::new(self.transform_expression(*init.clone(), ctx, temp_declarations));
                }
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
        let mut new_statements = Vec::new();
        
        // Transform all statements, looking for break statements
        for stmt in &mut body.stmts {
            match &mut stmt.kind {
                hir::StmtKind::Expr(expr) => {
                    // Check for break expressions that need special handling
                    if let hir::ExprKind::Break(Some(break_value)) = &expr.kind {
                        // Convert `break value` to assignment + plain break
                        let span = break_value.span;
                        
                        // Transform the break value
                        let mut temp_declarations = Vec::new();
                        let transformed_break_value = self.transform_expression(*break_value.clone(), ctx, &mut temp_declarations);
                        new_statements.extend(temp_declarations);
                        
                        // Create assignment statement: temp_name = break_value
                        let temp_path = self.create_temp_path(ctx, temp_name, span);
                        let assignment = self.create_assignment_expression(temp_path, transformed_break_value);
                        let assignment_stmt = hir::Stmt::new(
                            ctx.hir_id_allocator.next_id(),
                            hir::StmtKind::Expr(Box::new(assignment)),
                            span,
                        );
                        new_statements.push(assignment_stmt);
                        
                        // Create plain break statement
                        let plain_break_stmt = hir::Stmt::new(
                            ctx.hir_id_allocator.next_id(),
                            hir::StmtKind::Expr(Box::new(hir::Expr {
                                hir_id: expr.hir_id,
                                kind: hir::ExprKind::Break(None),
                                span: expr.span,
                            })),
                            expr.span,
                        );
                        new_statements.push(plain_break_stmt);
                    } else {
                        self.transform_expression_for_loop_temp(expr, temp_name, ctx);
                        new_statements.push(stmt.clone());
                    }
                }
                _ => {
                    // Regular transformation for other statement types
                    let mut temp_declarations = Vec::new();
                    self.transform_statement(stmt, ctx, &mut temp_declarations);
                    new_statements.extend(temp_declarations);
                    new_statements.push(stmt.clone());
                }
            }
        }
        
        // Transform completion expression if present
        if let Some(completion_expr) = &mut body.expr {
            self.transform_expression_for_loop_temp(completion_expr, temp_name, ctx);
        }
        
        // Replace the body statements with the transformed ones
        body.stmts = new_statements;
    }
    
    /// Transform expressions in loop context, handling break statements
    fn transform_expression_for_loop_temp(&mut self, expr: &mut hir::Expr, temp_name: &str, ctx: &mut HirOptContext) {
        match &mut expr.kind {
            hir::ExprKind::Match(_, _) | hir::ExprKind::Block(_) | hir::ExprKind::Loop(_) | hir::ExprKind::IfElse(_, _, _) => {
                // Complex expressions in loop context - transform recursively
                let mut temp_declarations = Vec::new();
                *expr = self.transform_expression(expr.clone(), ctx, &mut temp_declarations);
            }
            _ => {
                // Simple expressions - leave as is but recurse into children
                match &mut expr.kind {
                    hir::ExprKind::Binary(_, left, right) => {
                        self.transform_expression_for_loop_temp(left, temp_name, ctx);
                        self.transform_expression_for_loop_temp(right, temp_name, ctx);
                    }
                    hir::ExprKind::Call(call) => {
                        for arg in &mut call.arguments {
                            self.transform_expression_for_loop_temp(arg, temp_name, ctx);
                        }
                    }
                    _ => {}
                }
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

    /// Transform break expressions in let statements
    /// Converts `let x = break value;` into `temp = value; break; let x = temp;`
    fn transform_break_in_let_statement(&mut self, stmt: &mut hir::Stmt, ctx: &mut HirOptContext, temp_declarations: &mut Vec<hir::Stmt>) {
        if let hir::StmtKind::Let(pattern, init_expr, ty) = &mut stmt.kind {
            if let hir::ExprKind::Break(Some(break_value)) = &init_expr.kind {
                // Generate temp variable for the break value
                let temp_name = self.generate_temp_name();
                let temp_declaration = self.create_temp_declaration(ctx, &temp_name, init_expr.span);
                temp_declarations.push(temp_declaration);
                
                // Transform the break value
                let mut break_temp_declarations = Vec::new();
                let transformed_break_value = self.transform_expression(*break_value.clone(), ctx, &mut break_temp_declarations);
                temp_declarations.extend(break_temp_declarations);
                
                // Create assignment statement: temp = break_value
                let temp_path = self.create_temp_path(ctx, &temp_name, break_value.span);
                let assignment = self.create_assignment_expression(temp_path.clone(), transformed_break_value);
                let assignment_stmt = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(assignment)),
                    break_value.span,
                );
                temp_declarations.push(assignment_stmt);
                
                // Create plain break statement
                let plain_break = hir::Expr {
                    hir_id: ctx.hir_id_allocator.next_id(),
                    kind: hir::ExprKind::Break(None),
                    span: init_expr.span,
                };
                let break_stmt = hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(plain_break)),
                    init_expr.span,
                );
                temp_declarations.push(break_stmt);
                
                // Update the let statement to use the temp variable
                *init_expr = Box::new(temp_path);
            }
        }
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