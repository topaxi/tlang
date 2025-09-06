use tlang_ast::node::Ident;
use tlang_hir::{Visitor, hir, visit::{walk_block, walk_expr}};
use tlang_hir_opt::{HirOptContext, HirPass};
use tlang_span::Span;

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
        
        // Create assignment statement
        let assignment_stmt = self.create_assignment_stmt(ctx, &temp_name, expr, span);
        
        // Create temp variable reference
        let temp_ref = self.create_temp_var_path(ctx, &temp_name, span);

        (temp_ref, vec![temp_declaration, assignment_stmt])
    }

    /// Check if an expression can be represented as a JavaScript expression
    fn can_render_as_js_expr(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            // Simple expressions that can be rendered directly
            hir::ExprKind::Literal(..) |
            hir::ExprKind::Path(..) |
            hir::ExprKind::Wildcard => true,
            
            // Binary operations can usually be rendered
            hir::ExprKind::Binary(_, lhs, rhs) => {
                self.can_render_as_js_expr(lhs) && self.can_render_as_js_expr(rhs)
            }
            
            // Unary operations can usually be rendered
            hir::ExprKind::Unary(_, operand) => self.can_render_as_js_expr(operand),
            
            // Function calls can be rendered if arguments can be rendered
            hir::ExprKind::Call(call) => {
                call.arguments.iter().all(|arg| self.can_render_as_js_expr(arg))
            }
            
            // If-else can be rendered as ternary if conditions are met
            hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                self.can_render_if_else_as_ternary(condition, then_branch, else_branches)
            }
            
            // These expressions cannot be rendered as JavaScript expressions
            hir::ExprKind::Block(..) |
            hir::ExprKind::Loop(..) |
            hir::ExprKind::Match(..) |
            hir::ExprKind::Break(..) |
            hir::ExprKind::Continue |
            hir::ExprKind::TailCall(..) |
            hir::ExprKind::FunctionExpression(..) => false,
            
            // List literals can be rendered if all elements can be rendered
            hir::ExprKind::List(elements) => {
                elements.iter().all(|elem| self.can_render_as_js_expr(elem))
            }
            
            // Field access can be rendered if the object can be rendered
            hir::ExprKind::FieldAccess(obj, _) => self.can_render_as_js_expr(obj),
            
            // Other cases default to false for safety
            _ => false,
        }
    }

    /// Check if an if-else expression can be rendered as a ternary operator
    fn can_render_if_else_as_ternary(
        &self,
        condition: &hir::Expr,
        then_branch: &hir::Block,
        else_branches: &[hir::ElseClause],
    ) -> bool {
        // Must be a simple if-else (no else-if chains)
        if else_branches.len() != 1 {
            return false;
        }
        
        let else_branch = &else_branches[0];
        
        // Must not have an else-if condition
        if else_branch.condition.is_some() {
            return false;
        }
        
        // Condition must be simple
        if !self.can_render_as_js_expr(condition) {
            return false;
        }
        
        // Both branches must have no statements and simple completion expressions
        if !then_branch.stmts.is_empty() || !else_branch.consequence.stmts.is_empty() {
            return false;
        }
        
        // Both branches must have completion expressions that can be rendered
        let then_expr = match &then_branch.expr {
            Some(expr) => expr,
            None => return false,
        };
        let else_expr = match &else_branch.consequence.expr {
            Some(expr) => expr,
            None => return false,
        };
        
        self.can_render_as_js_expr(then_expr) && self.can_render_as_js_expr(else_expr)
    }

    /// Check if a block contains only a simple expression that can be rendered
    fn can_render_as_simple_block_expr(&self, block: &hir::Block) -> bool {
        block.stmts.is_empty() && 
        block.expr.as_ref().map_or(false, |expr| self.can_render_as_js_expr(expr))
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
                        let assignment_stmt = self.create_assignment_stmt(ctx, temp_name, *break_expr.clone(), span);
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
                let assignment_stmt = self.create_assignment_stmt(ctx, temp_name, *break_expr.clone(), expr.span);
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
                    self.transform_break_statements_in_block(&mut else_branch.consequence, temp_name, ctx);
                }
            }
            _ => {
                // For other expression types, use the visitor to walk through them
                walk_expr(self, expr, ctx);
            }
        }
    }

    /// Process statements and flatten complex expressions in let statements
    fn process_stmt(
        &mut self,
        stmt: &mut hir::Stmt,
        ctx: &mut HirOptContext,
    ) -> Vec<hir::Stmt> {
        let mut additional_stmts = Vec::new();

        match &mut stmt.kind {
            hir::StmtKind::Let(_, expr, _) => {
                if !self.can_render_as_js_expr(expr) {
                    // For complex expressions in let statements, flatten them
                    if let hir::ExprKind::Loop(..) = &expr.kind {
                        // Special handling for loop expressions
                        let (flattened_expr, mut temp_stmts) = 
                            self.transform_loop_expression(*expr.clone(), ctx);
                        
                        additional_stmts.append(&mut temp_stmts);
                        **expr = flattened_expr;
                        self.changes_made = true;
                    } else {
                        // General flattening for other complex expressions
                        let (flattened_expr, mut temp_stmts) = 
                            self.flatten_expression_to_temp_var(*expr.clone(), ctx);
                        
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
        self.changes_made
    }
}

impl<'hir> Visitor<'hir> for SimplifiedHirJsPass {
    type Context = HirOptContext;

    fn visit_block(
        &mut self,
        block: &'hir mut hir::Block,
        ctx: &mut Self::Context,
    ) {
        let mut new_stmts = Vec::new();

        for stmt in &mut block.stmts {
            // Process the statement and get any additional statements needed
            let additional_stmts = self.process_stmt(stmt, ctx);
            
            // Add the additional statements first
            new_stmts.extend(additional_stmts);
            
            // Then add the (possibly modified) original statement
            new_stmts.push(stmt.clone());
        }

        block.stmts = new_stmts;

        // Continue visiting nested structures
        walk_block(self, block, ctx);
    }
}