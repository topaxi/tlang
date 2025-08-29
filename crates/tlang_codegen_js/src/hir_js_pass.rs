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

    fn transform_nested_expressions_in_expr(&mut self, expr: &mut hir::Expr, _ctx: &mut HirOptContext) {
        // For now, let's focus on the block-level transformation 
        // and only handle simple cases in expression position.
        // Complex nested expressions in expression positions will be
        // handled by enhancing the block transformation logic.
        match &mut expr.kind {
            hir::ExprKind::Call(call_expr) => {
                // For function arguments, we'll only transform simple ternary-friendly cases
                // Complex cases should be handled at the block level by expanding the visitor
                for arg in &mut call_expr.arguments {
                    if let hir::ExprKind::IfElse(condition, then_branch, else_branches) = &arg.kind {
                        // Only transform if it can't be rendered as a ternary
                        if !if_else_can_render_as_ternary(condition, then_branch, else_branches) {
                            // Mark for transformation but don't transform here
                            // The block-level transformation should handle this
                            self.changes_made = true;
                        }
                    }
                }
            }
            _ => {
                // For other expression types, we'll rely on block-level transformation
            }
        }
    }

    fn needs_transformation(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Block(block) => block.has_completion(),
            hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                !if_else_can_render_as_ternary(condition, then_branch, else_branches)
            }
            hir::ExprKind::Match(..) => true, // Match expressions always need transformation in JS
            _ => false,
        }
    }

    fn transform_complex_expr_to_temp_var(
        &mut self, 
        expr: hir::Expr, 
        ctx: &mut HirOptContext
    ) -> hir::Expr {
        let temp_name = self.generate_temp_var_name();
        let span = expr.span;

        match &expr.kind {
            hir::ExprKind::Block(_block) => {
                // Create a special marker expression that the codegen can recognize
                hir::Expr {
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
                                    tlang_ast::token::Literal::String(temp_name.clone().into()),
                                )),
                                span,
                            },
                            // Second argument: the block expression
                            expr,
                        ],
                    })),
                    span,
                }
            }
            hir::ExprKind::IfElse(..) => {
                // Create a special marker expression for if/else
                hir::Expr {
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
                                    tlang_ast::token::Literal::String(temp_name.clone().into()),
                                )),
                                span,
                            },
                            // Second argument: the if/else expression
                            expr,
                        ],
                    })),
                    span,
                }
            }
            hir::ExprKind::Match(..) => {
                // Create a special marker expression for match
                hir::Expr {
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
                                    tlang_ast::token::Literal::String(temp_name.clone().into()),
                                )),
                                span,
                            },
                            // Second argument: the match expression
                            expr,
                        ],
                    })),
                    span,
                }
            }
            _ => expr, // Return the expression unchanged if it doesn't need transformation
        }
    }

    fn expr_needs_hoisting(&self, expr: &hir::Expr) -> bool {
        // Check if an expression contains complex nested expressions that need to be hoisted
        self.contains_complex_nested_expressions(expr)
    }

    fn contains_complex_nested_expressions(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Call(call_expr) => {
                // Check if any arguments contain complex expressions
                call_expr.arguments.iter().any(|arg| self.needs_transformation(arg))
                    || call_expr.arguments.iter().any(|arg| self.contains_complex_nested_expressions(arg))
            }
            hir::ExprKind::IfElse(condition, _then_branch, else_branches) => {
                // Check if the condition contains complex expressions
                self.needs_transformation(condition)
                    || self.contains_complex_nested_expressions(condition)
                    || else_branches.iter().any(|else_branch| {
                        if let Some(ref else_condition) = else_branch.condition {
                            self.needs_transformation(else_condition)
                                || self.contains_complex_nested_expressions(else_condition)
                        } else {
                            false
                        }
                    })
            }
            hir::ExprKind::Binary(_, lhs, rhs) => {
                self.needs_transformation(lhs) || self.contains_complex_nested_expressions(lhs)
                    || self.needs_transformation(rhs) || self.contains_complex_nested_expressions(rhs)
            }
            hir::ExprKind::Unary(_, inner) => {
                self.needs_transformation(inner) || self.contains_complex_nested_expressions(inner)
            }
            _ => false,
        }
    }

    fn hoist_complex_expressions(
        &mut self,
        expr: hir::Expr,
        ctx: &mut HirOptContext,
    ) -> Option<(Vec<hir::Stmt>, hir::Expr)> {
        let mut hoisted_stmts = Vec::new();
        let simplified_expr = self.simplify_expression_recursive(expr, ctx, &mut hoisted_stmts);
        
        if hoisted_stmts.is_empty() {
            None
        } else {
            Some((hoisted_stmts, simplified_expr))
        }
    }

    fn simplify_expression_recursive(
        &mut self,
        mut expr: hir::Expr,
        ctx: &mut HirOptContext,
        hoisted_stmts: &mut Vec<hir::Stmt>,
    ) -> hir::Expr {
        match &mut expr.kind {
            hir::ExprKind::Call(call_expr) => {
                // Transform complex arguments
                for arg in &mut call_expr.arguments {
                    if self.needs_transformation(arg) {
                        let temp_name = self.generate_temp_var_name();
                        let span = arg.span;
                        
                        // Create temp variable declaration and complex expression execution
                        let temp_decl_stmt = self.create_temp_var_declaration_stmt(ctx, &temp_name, span);
                        hoisted_stmts.push(temp_decl_stmt);
                        
                        let complex_stmt = self.create_complex_expr_execution_stmt(
                            ctx, &temp_name, arg.clone(), span
                        );
                        hoisted_stmts.push(complex_stmt);
                        
                        // Replace the argument with a reference to the temp variable
                        *arg = self.create_temp_var_path(ctx, &temp_name, span);
                    } else if self.contains_complex_nested_expressions(arg) {
                        // Recursively simplify nested expressions
                        *arg = self.simplify_expression_recursive(arg.clone(), ctx, hoisted_stmts);
                    }
                }
                expr
            }
            _ => expr,
        }
    }

    fn create_temp_var_declaration_stmt(
        &mut self,
        ctx: &mut HirOptContext,
        temp_name: &str,
        span: Span,
    ) -> hir::Stmt {
        // Create: let temp_var;
        let hir_id = ctx.hir_id_allocator.next_id();
        let pat = hir::Pat {
            kind: hir::PatKind::Identifier(
                ctx.hir_id_allocator.next_id(),
                Box::new(Ident::new(temp_name, span))
            ),
            span,
        };
        
        // Use undefined literal as initial value
        let undefined_expr = hir::Expr {
            hir_id: ctx.hir_id_allocator.next_id(),
            kind: hir::ExprKind::Path(Box::new(hir::Path::new(
                vec![hir::PathSegment {
                    ident: Ident::new("undefined", span),
                }],
                span,
            ))),
            span,
        };

        // Create a basic type (we'll use a generic type for now)
        let basic_ty = hir::Ty {
            kind: hir::TyKind::Path(hir::Path::new(
                vec![hir::PathSegment {
                    ident: Ident::new("any", span),
                }],
                span,
            )),
            span,
        };

        hir::Stmt::new(
            hir_id,
            hir::StmtKind::Let(Box::new(pat), Box::new(undefined_expr), Box::new(basic_ty)),
            span,
        )
    }

    fn create_complex_expr_execution_stmt(
        &mut self,
        ctx: &mut HirOptContext,
        temp_name: &str,
        complex_expr: hir::Expr,
        span: Span,
    ) -> hir::Stmt {
        match &complex_expr.kind {
            hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                // Create modified if/else that assigns to temp variable
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

                let if_else_expr = hir::Expr {
                    hir_id: ctx.hir_id_allocator.next_id(),
                    kind: hir::ExprKind::IfElse(
                        condition.clone(),
                        Box::new(new_then_branch),
                        new_else_branches,
                    ),
                    span,
                };

                hir::Stmt::new(
                    ctx.hir_id_allocator.next_id(),
                    hir::StmtKind::Expr(Box::new(if_else_expr)),
                    span,
                )
            }
            _ => {
                // For other complex expressions, just assign them directly
                self.create_assignment_stmt(ctx, temp_name, complex_expr, span)
            }
        }
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
        hir::ExprKind::IfElse(..) => false,
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
        // First, recursively visit children to handle deeply nested cases
        visit::walk_expr(self, expr, ctx);

        // Check if this expression contains nested complex expressions that need transformation
        self.transform_nested_expressions_in_expr(expr, ctx);
    }

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, ctx: &mut Self::Context) {
        // First, visit children to handle nested cases
        visit::walk_stmt(self, stmt, ctx);

        // Then check if this statement needs transformation
        if let hir::StmtKind::Let(_pat, expr, _ty) = &mut stmt.kind {
            match &expr.kind {
                hir::ExprKind::Block(block) => {
                    if block.has_completion() {
                        // This is a let statement with a block expression that has a completion value
                        // We need to transform this at a higher level since we need to insert statements
                        // For now, just mark that we found something to transform
                        self.changes_made = true;
                    }
                }
                hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                    // Check if this if/else can be rendered as a ternary operator
                    if !if_else_can_render_as_ternary(condition, then_branch, else_branches) {
                        // This is a let statement with an if/else expression that needs transformation
                        self.changes_made = true;
                    }
                }
                _ => {}
            }
        }
    }

    fn visit_block(&mut self, block: &'hir mut hir::Block, ctx: &mut Self::Context) {
        // Transform let statements with block expressions and other complex statements
        let mut new_stmts = Vec::new();

        for stmt in &mut block.stmts {
            match &stmt.kind {
                // Handle let statements with complex expressions
                hir::StmtKind::Let(pat, expr, ty) => {
                    match &expr.kind {
                        hir::ExprKind::Block(block_expr) => {
                            if block_expr.has_completion() {
                                // Transform block expression (existing logic)
                                let temp_name = self.generate_temp_var_name();
                                let span = stmt.span;

                                // Create the modified block with assignment statement
                                let mut new_block = block_expr.as_ref().clone();

                                // Move the completion expression to an assignment statement
                                if let Some(completion_expr) = new_block.expr.take() {
                                    let assignment_stmt = self.create_assignment_stmt(
                                        ctx,
                                        &temp_name,
                                        completion_expr,
                                        span,
                                    );
                                    new_block.stmts.push(assignment_stmt);
                                }

                                // Create a single statement that represents the temp declaration + block combination
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
                                                    tlang_ast::token::Literal::String(
                                                        temp_name.clone().into(),
                                                    ),
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

                                let combined_stmt = hir::Stmt::new(
                                    ctx.hir_id_allocator.next_id(),
                                    hir::StmtKind::Expr(Box::new(combined_expr)),
                                    span,
                                );
                                new_stmts.push(combined_stmt);

                                // Create the modified let statement using temp variable
                                let temp_path_expr = self.create_temp_var_path(ctx, &temp_name, span);
                                let modified_let = hir::Stmt::new(
                                    stmt.hir_id,
                                    hir::StmtKind::Let(
                                        pat.clone(),
                                        Box::new(temp_path_expr),
                                        ty.clone(),
                                    ),
                                    span,
                                );
                                new_stmts.push(modified_let);

                                self.changes_made = true;
                                continue;
                            }
                        }
                        hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                            // Check if this if/else can be rendered as a ternary operator
                            if !if_else_can_render_as_ternary(condition, then_branch, else_branches) {
                                // Transform if/else expression (existing logic)
                                let temp_name = self.generate_temp_var_name();
                                let span = stmt.span;

                                // Create modified if/else with assignment statements
                                let mut new_then_branch = then_branch.as_ref().clone();
                                if let Some(completion_expr) = new_then_branch.expr.take() {
                                    let assignment_stmt = self.create_assignment_stmt(
                                        ctx,
                                        &temp_name,
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
                                            &temp_name,
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

                                // Create a single statement that represents the temp declaration + if/else combination
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
                                                    tlang_ast::token::Literal::String(
                                                        temp_name.clone().into(),
                                                    ),
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

                                let combined_stmt = hir::Stmt::new(
                                    ctx.hir_id_allocator.next_id(),
                                    hir::StmtKind::Expr(Box::new(combined_expr)),
                                    span,
                                );
                                new_stmts.push(combined_stmt);

                                // Create the modified let statement using temp variable
                                let temp_path_expr = self.create_temp_var_path(ctx, &temp_name, span);
                                let modified_let = hir::Stmt::new(
                                    stmt.hir_id,
                                    hir::StmtKind::Let(
                                        pat.clone(),
                                        Box::new(temp_path_expr),
                                        ty.clone(),
                                    ),
                                    span,
                                );
                                new_stmts.push(modified_let);

                                self.changes_made = true;
                                continue;
                            }
                        }
                        _ => {}
                    }
                    
                    // If we didn't transform this let statement, just add it as-is
                    new_stmts.push(stmt.clone());
                }
                // Handle expression statements that might contain complex expressions
                hir::StmtKind::Expr(expr) => {
                    if self.expr_needs_hoisting(expr) {
                        // Transform expression statements with complex nested expressions
                        if let Some((hoisted_stmts, simplified_expr)) = 
                            self.hoist_complex_expressions(expr.as_ref().clone(), ctx) {
                            // Add hoisted statements first
                            new_stmts.extend(hoisted_stmts);
                            // Then add the simplified expression statement
                            let simplified_stmt = hir::Stmt::new(
                                stmt.hir_id,
                                hir::StmtKind::Expr(Box::new(simplified_expr)),
                                stmt.span,
                            );
                            new_stmts.push(simplified_stmt);
                            self.changes_made = true;
                            continue;
                        }
                    }
                    
                    // If we didn't transform this expression statement, just add it as-is
                    new_stmts.push(stmt.clone());
                }
                _ => {
                    // For other statement types, just add them as-is for now
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
            // If the block expression itself is complex, we may need to transform it too
            if self.expr_needs_hoisting(expr) {
                if let Some((hoisted_stmts, simplified_expr)) = 
                    self.hoist_complex_expressions((*expr).clone(), ctx) {
                    // Add hoisted statements to the block
                    block.stmts.extend(hoisted_stmts);
                    // Replace the block expression with the simplified one
                    *expr = Box::new(simplified_expr);
                    self.changes_made = true;
                }
            } else {
                self.visit_expr(expr, ctx);
            }
        }
    }
}
