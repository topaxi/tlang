use tlang_ast::node::Ident;
use tlang_hir::{Visitor, hir, visit};
use tlang_hir_opt::{HirOptContext, HirPass};
use tlang_span::Span;

#[derive(Debug, Default)]
pub struct HirJsPass {
    temp_var_counter: usize,
    changes_made: bool,
}

impl HirJsPass {
    pub fn new() -> Self {
        Self {
            temp_var_counter: 0,
            changes_made: false,
        }
    }

    fn generate_temp_var_name(&mut self) -> String {
        let name = format!("$tmp${}", self.temp_var_counter);
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
        let temp_path = self.create_temp_var_path(ctx, &temp_name, span);

        // Create an assignment using a binary operator (this is a simplification)
        // In a more complete implementation, we might need a dedicated assignment statement type
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

    /// Generic function to flatten a complex expression to a temporary variable
    /// Returns (flattened_expr, statements_to_insert)
    fn flatten_expression_to_temp_var(
        &mut self,
        expr: hir::Expr,
        ctx: &mut HirOptContext,
    ) -> (hir::Expr, Vec<hir::Stmt>) {
        if !expr_can_render_as_js_expr(&expr) {
            // This expression needs flattening
            let temp_name = self.generate_temp_var_name();
            let span = expr.span;
            
            // Create temporary variable assignments for this expression
            let temp_stmts = self.create_temp_var_assignment_for_expr(
                ctx,
                &temp_name,
                expr,
                span,
            );
            
            // Return the temporary variable reference and the statements
            let temp_var_expr = self.create_temp_var_path(ctx, &temp_name, span);
            (temp_var_expr, temp_stmts)
        } else {
            // Expression can be rendered as JS, but we still need to flatten its subexpressions
            let (flattened_expr, stmts) = self.flatten_subexpressions_generically(expr, ctx);
            (flattened_expr, stmts)
        }
    }

    /// Recursively flatten subexpressions in all contexts
    fn flatten_subexpressions_generically(
        &mut self,
        mut expr: hir::Expr,
        ctx: &mut HirOptContext,
    ) -> (hir::Expr, Vec<hir::Stmt>) {
        let mut statements = Vec::new();

        match &mut expr.kind {
            hir::ExprKind::Call(call_expr) => {
                // Flatten function arguments
                for arg in &mut call_expr.arguments {
                    if !expr_can_render_as_js_expr(arg) {
                        let span = arg.span;
                        let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                        let expr_to_flatten = std::mem::replace(arg, placeholder);
                        let (flattened, mut stmts) = self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                        *arg = flattened;
                        statements.append(&mut stmts);
                        self.changes_made = true;
                    }
                }
            }
            hir::ExprKind::Binary(_, lhs, rhs) => {
                // Flatten left and right operands
                if !expr_can_render_as_js_expr(lhs) {
                    let span = lhs.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(lhs.as_mut(), placeholder);
                    let (flattened, mut stmts) = self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    **lhs = flattened;
                    statements.append(&mut stmts);
                    self.changes_made = true;
                }
                if !expr_can_render_as_js_expr(rhs) {
                    let span = rhs.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(rhs.as_mut(), placeholder);
                    let (flattened, mut stmts) = self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    **rhs = flattened;
                    statements.append(&mut stmts);
                    self.changes_made = true;
                }
            }
            hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                // Flatten condition if it's complex
                if !expr_can_render_as_js_expr(condition) {
                    let span = condition.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(condition.as_mut(), placeholder);
                    let (flattened, mut stmts) = self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    **condition = flattened;
                    statements.append(&mut stmts);
                    self.changes_made = true;
                }
                
                // Recursively handle then and else expressions
                if let Some(then_expr) = &mut then_branch.expr {
                    let span = then_expr.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(then_expr, placeholder);
                    let (flattened, mut stmts) = self.flatten_subexpressions_generically(expr_to_flatten, ctx);
                    *then_expr = flattened;
                    statements.append(&mut stmts);
                }
                
                for else_branch in else_branches {
                    if let Some(else_expr) = &mut else_branch.consequence.expr {
                        let span = else_expr.span;
                        let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                        let expr_to_flatten = std::mem::replace(else_expr, placeholder);
                        let (flattened, mut stmts) = self.flatten_subexpressions_generically(expr_to_flatten, ctx);
                        *else_expr = flattened;
                        statements.append(&mut stmts);
                    }
                }
            }
            hir::ExprKind::List(exprs) => {
                // Flatten list elements
                for expr_elem in exprs {
                    if !expr_can_render_as_js_expr(expr_elem) {
                        let span = expr_elem.span;
                        let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                        let expr_to_flatten = std::mem::replace(expr_elem, placeholder);
                        let (flattened, mut stmts) = self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                        *expr_elem = flattened;
                        statements.append(&mut stmts);
                        self.changes_made = true;
                    }
                }
            }
            hir::ExprKind::Dict(exprs) => {
                // Flatten dictionary key-value pairs
                for (key, value) in exprs {
                    if !expr_can_render_as_js_expr(key) {
                        let span = key.span;
                        let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                        let expr_to_flatten = std::mem::replace(key, placeholder);
                        let (flattened, mut stmts) = self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                        *key = flattened;
                        statements.append(&mut stmts);
                        self.changes_made = true;
                    }
                    if !expr_can_render_as_js_expr(value) {
                        let span = value.span;
                        let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                        let expr_to_flatten = std::mem::replace(value, placeholder);
                        let (flattened, mut stmts) = self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                        *value = flattened;
                        statements.append(&mut stmts);
                        self.changes_made = true;
                    }
                }
            }
            hir::ExprKind::IndexAccess(base, index) => {
                // Flatten base and index expressions
                if !expr_can_render_as_js_expr(base) {
                    let span = base.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(base.as_mut(), placeholder);
                    let (flattened, mut stmts) = self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    **base = flattened;
                    statements.append(&mut stmts);
                    self.changes_made = true;
                }
                if !expr_can_render_as_js_expr(index) {
                    let span = index.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(index.as_mut(), placeholder);
                    let (flattened, mut stmts) = self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    **index = flattened;
                    statements.append(&mut stmts);
                    self.changes_made = true;
                }
            }
            hir::ExprKind::FieldAccess(base, _) => {
                // Flatten base expression
                if !expr_can_render_as_js_expr(base) {
                    let span = base.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(base.as_mut(), placeholder);
                    let (flattened, mut stmts) = self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    **base = flattened;
                    statements.append(&mut stmts);
                    self.changes_made = true;
                }
            }
            hir::ExprKind::Unary(_, operand) => {
                // Flatten unary operand
                if !expr_can_render_as_js_expr(operand) {
                    let span = operand.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(operand.as_mut(), placeholder);
                    let (flattened, mut stmts) = self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    **operand = flattened;
                    statements.append(&mut stmts);
                    self.changes_made = true;
                }
            }
            hir::ExprKind::Cast(operand, _) => {
                // Flatten cast operand
                if !expr_can_render_as_js_expr(operand) {
                    let span = operand.span;
                    let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
                    let expr_to_flatten = std::mem::replace(operand.as_mut(), placeholder);
                    let (flattened, mut stmts) = self.flatten_expression_to_temp_var(expr_to_flatten, ctx);
                    **operand = flattened;
                    statements.append(&mut stmts);
                    self.changes_made = true;
                }
            }
            _ => {
                // For other expression types, no subexpressions to flatten
            }
        }

        (expr, statements)
    }



    fn create_temp_var_assignment_for_expr(
        &mut self,
        ctx: &mut HirOptContext,
        temp_name: &str,
        expr: hir::Expr,
        span: Span,
    ) -> Vec<hir::Stmt> {
        let mut statements = Vec::new();
        
        match &expr.kind {
            hir::ExprKind::Block(block) => {
                if block.has_completion() {
                    // Transform block expression (existing logic adapted)
                    let mut new_block = block.as_ref().clone();

                    // Move the completion expression to an assignment statement
                    if let Some(completion_expr) = new_block.expr.take() {
                        let assignment_stmt = self.create_assignment_stmt(
                            ctx,
                            temp_name,
                            completion_expr,
                            span,
                        );
                        new_block.stmts.push(assignment_stmt);
                    }

                    // Create a combined statement with the block
                    let combined_expr = hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Call(Box::new(hir::CallExpression {
                            hir_id: ctx.hir_id_allocator.next_id(),
                            callee: hir::Expr {
                                hir_id: ctx.hir_id_allocator.next_id(),
                                kind: hir::ExprKind::Path(Box::new(hir::Path::new(
                                    vec![hir::PathSegment {
                                        ident: Ident::new("__TEMP_VAR_BLOCK__", span),
                                    }],
                                    span,
                                ))),
                                span,
                            },
                            arguments: vec![
                                // First argument: temp variable name
                                hir::Expr {
                                    hir_id: ctx.hir_id_allocator.next_id(),
                                    kind: hir::ExprKind::Literal(Box::new(
                                        tlang_ast::token::Literal::String(temp_name.into()),
                                    )),
                                    span,
                                },
                                // Second argument: the block
                                hir::Expr {
                                    hir_id: ctx.hir_id_allocator.next_id(),
                                    kind: hir::ExprKind::Block(Box::new(new_block)),
                                    span,
                                },
                            ],
                        })),
                        span,
                    };

                    statements.push(hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Expr(Box::new(combined_expr)),
                        span,
                    ));
                }
            }
            hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                // Check if this if/else can be rendered as a ternary operator
                if !if_else_can_render_as_ternary(condition, then_branch, else_branches) {
                    // Transform if/else expression (existing logic adapted)
                    let mut new_then_branch = then_branch.as_ref().clone();
                    if let Some(completion_expr) = new_then_branch.expr.take() {
                        let assignment_stmt = self.create_assignment_stmt(
                            ctx,
                            temp_name,
                            completion_expr,
                            span,
                        );
                        new_then_branch.stmts.push(assignment_stmt);
                    }

                    let mut new_else_branches = Vec::new();
                    for else_branch in else_branches {
                        let mut new_else_consequence = else_branch.consequence.clone();
                        if let Some(completion_expr) = new_else_consequence.expr.take() {
                            let assignment_stmt = self.create_assignment_stmt(
                                ctx,
                                temp_name,
                                completion_expr,
                                span,
                            );
                            new_else_consequence.stmts.push(assignment_stmt);
                        }
                        new_else_branches.push(hir::ElseClause {
                            condition: else_branch.condition.clone(),
                            consequence: new_else_consequence,
                        });
                    }

                    // Create a combined statement with the if/else
                    let combined_expr = hir::Expr {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        kind: hir::ExprKind::Call(Box::new(hir::CallExpression {
                            hir_id: ctx.hir_id_allocator.next_id(),
                            callee: hir::Expr {
                                hir_id: ctx.hir_id_allocator.next_id(),
                                kind: hir::ExprKind::Path(Box::new(hir::Path::new(
                                    vec![hir::PathSegment {
                                        ident: Ident::new("__TEMP_VAR_IF_ELSE__", span),
                                    }],
                                    span,
                                ))),
                                span,
                            },
                            arguments: vec![
                                // First argument: temp variable name
                                hir::Expr {
                                    hir_id: ctx.hir_id_allocator.next_id(),
                                    kind: hir::ExprKind::Literal(Box::new(
                                        tlang_ast::token::Literal::String(temp_name.into()),
                                    )),
                                    span,
                                },
                                // Second argument: the if/else expression
                                hir::Expr {
                                    hir_id: ctx.hir_id_allocator.next_id(),
                                    kind: hir::ExprKind::IfElse(
                                        condition.clone(),
                                        Box::new(new_then_branch),
                                        new_else_branches,
                                    ),
                                    span,
                                },
                            ],
                        })),
                        span,
                    };

                    statements.push(hir::Stmt::new(
                        ctx.hir_id_allocator.next_id(),
                        hir::StmtKind::Expr(Box::new(combined_expr)),
                        span,
                    ));
                }
            }
            hir::ExprKind::Match(..) => {
                // Transform match expression (existing logic adapted)
                let combined_expr = hir::Expr {
                    hir_id: ctx.hir_id_allocator.next_id(),
                    kind: hir::ExprKind::Call(Box::new(hir::CallExpression {
                        hir_id: ctx.hir_id_allocator.next_id(),
                        callee: hir::Expr {
                            hir_id: ctx.hir_id_allocator.next_id(),
                            kind: hir::ExprKind::Path(Box::new(hir::Path::new(
                                vec![hir::PathSegment {
                                    ident: Ident::new("__TEMP_VAR_MATCH__", span),
                                }],
                                span,
                            ))),
                            span,
                        },
                        arguments: vec![
                            // First argument: temp variable name
                            hir::Expr {
                                hir_id: ctx.hir_id_allocator.next_id(),
                                kind: hir::ExprKind::Literal(Box::new(
                                    tlang_ast::token::Literal::String(temp_name.into()),
                                )),
                                span,
                            },
                            // Second argument: the match expression
                            expr,
                        ],
                    })),
                    span,
                };

                statements.push(hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(combined_expr)),
                    span,
                ));
            }
            _ => {
                // For other expressions, just create a simple assignment
                statements.push(self.create_assignment_stmt(ctx, temp_name, expr, span));
            }
        }
        
        statements
    }
}

/// Check if an if-else expression can be rendered as a ternary operator in JavaScript
pub fn if_else_can_render_as_ternary(
    expr: &hir::Expr,
    then_branch: &hir::Block,
    else_branches: &[hir::ElseClause],
) -> bool {
    then_branch.stmts.is_empty()
        && expr_can_render_as_js_expr(expr)
        && expr_can_render_as_js_expr(then_branch.expr.as_ref().unwrap())
        && else_branches.iter().all(|else_branch| {
            else_branch.consequence.stmts.is_empty()
                && (else_branch.condition.is_none()
                    || expr_can_render_as_js_expr(else_branch.condition.as_ref().unwrap()))
                && expr_can_render_as_js_expr(else_branch.consequence.expr.as_ref().unwrap())
        })
}

/// Check if an expression can be rendered as a JavaScript expression (vs statement)
pub fn expr_can_render_as_js_expr(expr: &hir::Expr) -> bool {
    match &expr.kind {
        hir::ExprKind::Path(..) => true,
        hir::ExprKind::Let(..) => false,
        hir::ExprKind::Literal(..) => true,
        hir::ExprKind::Binary(_, lhs, rhs) => {
            expr_can_render_as_js_expr(lhs) && expr_can_render_as_js_expr(rhs)
        }
        hir::ExprKind::Unary(_, expr) => expr_can_render_as_js_expr(expr),
        hir::ExprKind::Block(..) => false,
        hir::ExprKind::Loop(..) => false,
        hir::ExprKind::Break(..) => false,
        hir::ExprKind::Continue => false,
        hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
            // If-else can be rendered as a JS expression if it can be a ternary operator
            if_else_can_render_as_ternary(condition, then_branch, else_branches)
        },
        hir::ExprKind::Match(..) => false,
        hir::ExprKind::Call(call_expr) => {
            call_expr.arguments.iter().all(expr_can_render_as_js_expr)
        }
        hir::ExprKind::Cast(expr, _) => expr_can_render_as_js_expr(expr),
        hir::ExprKind::FieldAccess(base, _) => expr_can_render_as_js_expr(base),
        hir::ExprKind::IndexAccess(base, index) => {
            expr_can_render_as_js_expr(base) && expr_can_render_as_js_expr(index)
        }
        hir::ExprKind::TailCall(_) => false,
        hir::ExprKind::List(exprs) => exprs.iter().all(expr_can_render_as_js_expr),
        hir::ExprKind::Dict(exprs) => exprs
            .iter()
            .all(|kv| expr_can_render_as_js_expr(&kv.0) && expr_can_render_as_js_expr(&kv.1)),
        hir::ExprKind::FunctionExpression(..) => true,
        hir::ExprKind::Range(..) => true,
        hir::ExprKind::Wildcard => true,
    }
}

impl HirPass for HirJsPass {
    fn optimize_hir(&mut self, module: &mut hir::Module, ctx: &mut HirOptContext) -> bool {
        self.changes_made = false;
        self.visit_module(module, ctx);
        self.changes_made
    }
}

impl<'hir> Visitor<'hir> for HirJsPass {
    type Context = HirOptContext;

    fn visit_expr(&mut self, expr: &'hir mut hir::Expr, ctx: &mut Self::Context) {
        // Don't recursively walk here - we'll handle recursion in our generic flattening
        
        // Apply generic expression flattening
        let span = expr.span;
        let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
        let expr_to_flatten = std::mem::replace(expr, placeholder);
        let (flattened_expr, statements) = self.flatten_subexpressions_generically(expr_to_flatten, ctx);
        *expr = flattened_expr;
        
        // If we generated statements, we need to handle them at the block level
        // For now, just note that changes were made
        if !statements.is_empty() {
            self.changes_made = true;
        }
    }

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, ctx: &mut Self::Context) {
        // Let visit_block handle the complex transformation logic
        // This is mainly for statements that aren't within blocks
        visit::walk_stmt(self, stmt, ctx);
    }

    fn visit_block(&mut self, block: &'hir mut hir::Block, ctx: &mut Self::Context) {
        // Transform all statements using the generic flattening approach
        let mut new_stmts = Vec::new();

        for stmt in &mut block.stmts {
            match &stmt.kind {
                // Handle let statements with complex expressions
                hir::StmtKind::Let(pat, expr, ty) => {
                    if !expr_can_render_as_js_expr(expr) {
                        // Use generic flattening for the expression
                        let (flattened_expr, mut temp_stmts) = self.flatten_expression_to_temp_var(
                            expr.as_ref().clone(),
                            ctx,
                        );
                        
                        // Add the temporary variable statements first
                        new_stmts.append(&mut temp_stmts);
                        
                        // Create the modified let statement using the flattened expression
                        let modified_let = hir::Stmt::new(
                            stmt.hir_id,
                            hir::StmtKind::Let(
                                pat.clone(),
                                Box::new(flattened_expr),
                                ty.clone(),
                            ),
                            stmt.span,
                        );
                        new_stmts.push(modified_let);
                        
                        self.changes_made = true;
                    } else {
                        // Expression can be rendered as JS, but check subexpressions
                        let (flattened_expr, mut temp_stmts) = self.flatten_subexpressions_generically(
                            expr.as_ref().clone(),
                            ctx,
                        );
                        
                        if !temp_stmts.is_empty() {
                            // Add the temporary variable statements first
                            new_stmts.append(&mut temp_stmts);
                            
                            // Create the modified let statement using the flattened expression
                            let modified_let = hir::Stmt::new(
                                stmt.hir_id,
                                hir::StmtKind::Let(
                                    pat.clone(),
                                    Box::new(flattened_expr),
                                    ty.clone(),
                                ),
                                stmt.span,
                            );
                            new_stmts.push(modified_let);
                            
                            self.changes_made = true;
                        } else {
                            // No changes needed
                            new_stmts.push(stmt.clone());
                        }
                    }
                }
                // Handle return statements with complex expressions
                hir::StmtKind::Return(Some(expr)) => {
                    if !expr_can_render_as_js_expr(expr) {
                        // Use generic flattening for the expression
                        let (flattened_expr, mut temp_stmts) = self.flatten_expression_to_temp_var(
                            expr.as_ref().clone(),
                            ctx,
                        );
                        
                        // Add the temporary variable statements first
                        new_stmts.append(&mut temp_stmts);
                        
                        // Create the modified return statement using the flattened expression
                        let modified_return = hir::Stmt::new(
                            stmt.hir_id,
                            hir::StmtKind::Return(Some(Box::new(flattened_expr))),
                            stmt.span,
                        );
                        new_stmts.push(modified_return);
                        
                        self.changes_made = true;
                    } else {
                        // Expression can be rendered as JS, but check subexpressions
                        let (flattened_expr, mut temp_stmts) = self.flatten_subexpressions_generically(
                            expr.as_ref().clone(),
                            ctx,
                        );
                        
                        if !temp_stmts.is_empty() {
                            // Add the temporary variable statements first
                            new_stmts.append(&mut temp_stmts);
                            
                            // Create the modified return statement using the flattened expression
                            let modified_return = hir::Stmt::new(
                                stmt.hir_id,
                                hir::StmtKind::Return(Some(Box::new(flattened_expr))),
                                stmt.span,
                            );
                            new_stmts.push(modified_return);
                            
                            self.changes_made = true;
                        } else {
                            // No changes needed
                            new_stmts.push(stmt.clone());
                        }
                    }
                }
                // Handle expression statements
                hir::StmtKind::Expr(expr) => {
                    // Apply generic flattening to expression statements as well
                    let (flattened_expr, mut temp_stmts) = self.flatten_subexpressions_generically(
                        expr.as_ref().clone(),
                        ctx,
                    );
                    
                    if !temp_stmts.is_empty() {
                        // Add the temporary variable statements first
                        new_stmts.append(&mut temp_stmts);
                        
                        // Create the modified expression statement
                        let modified_expr_stmt = hir::Stmt::new(
                            stmt.hir_id,
                            hir::StmtKind::Expr(Box::new(flattened_expr)),
                            stmt.span,
                        );
                        new_stmts.push(modified_expr_stmt);
                        
                        self.changes_made = true;
                    } else {
                        // No changes needed
                        new_stmts.push(stmt.clone());
                    }
                }
                _ => {
                    // For other statement types, just add them as-is
                    new_stmts.push(stmt.clone());
                }
            }
        }

        // Replace the statements if we made changes
        if !new_stmts.is_empty() {
            block.stmts = new_stmts;
        }

        // Visit the block expression if it exists
        if let Some(expr) = &mut block.expr {
            let span = expr.span;
            let placeholder = self.create_temp_var_path(ctx, "placeholder", span);
            let expr_to_flatten = std::mem::replace(expr, placeholder);
            let (flattened_expr, temp_stmts) = self.flatten_subexpressions_generically(expr_to_flatten, ctx);
            *expr = flattened_expr;
            
            // If we generated statements for the block expression, add them to the block
            if !temp_stmts.is_empty() {
                block.stmts.extend(temp_stmts);
                self.changes_made = true;
            }
        }
    }
}
