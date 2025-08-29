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
        match &mut expr.kind {
            hir::ExprKind::Call(call_expr) => {
                // Transform function arguments with complex expressions
                for arg in &mut call_expr.arguments {
                    if self.needs_transformation(arg) {
                        // Mark that this argument needs transformation
                        self.changes_made = true;
                    }
                }
            }
            hir::ExprKind::IfElse(condition, _then_branch, _else_branches) => {
                // Check if condition has nested complex expressions
                if self.needs_transformation(condition) {
                    self.changes_made = true;
                }
            }
            _ => {
                // For other expression types, continue recursion
            }
        }
    }

    fn needs_transformation(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Block(block) => block.has_completion(),
            hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                // Check if this if/else contains statements or nested complex expressions
                !then_branch.stmts.is_empty()
                    || else_branches.iter().any(|else_branch| !else_branch.consequence.stmts.is_empty())
                    || self.needs_transformation(condition)
                    || (then_branch.expr.as_ref().map_or(false, |e| self.needs_transformation(e)))
                    || else_branches.iter().any(|else_branch| {
                        else_branch.condition.as_ref().map_or(false, |c| self.needs_transformation(c))
                            || else_branch.consequence.expr.as_ref().map_or(false, |e| self.needs_transformation(e))
                    })
            }
            hir::ExprKind::Match(..) => true, // Match expressions always need transformation in JS
            _ => false,
        }
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
        // First, recursively visit children to handle deeply nested cases
        visit::walk_expr(self, expr, ctx);

        // Check if this expression contains nested complex expressions that need transformation
        self.transform_nested_expressions_in_expr(expr, ctx);
    }

    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, ctx: &mut Self::Context) {
        // First, visit children to handle nested cases
        visit::walk_stmt(self, stmt, ctx);

        // Handle different statement types that may contain complex expressions
        match &mut stmt.kind {
            hir::StmtKind::Let(_pat, expr, _ty) => {
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
            hir::StmtKind::Return(Some(expr)) => {
                // Handle return statements with complex expressions
                if self.needs_transformation(expr) {
                    self.changes_made = true;
                }
            }
            hir::StmtKind::Expr(expr) => {
                // Handle expression statements with complex expressions
                if self.needs_transformation(expr) {
                    self.changes_made = true;
                }
            }
            _ => {}
        }
    }

    fn visit_block(&mut self, block: &'hir mut hir::Block, ctx: &mut Self::Context) {
        // Transform all statements that contain complex expressions
        let mut new_stmts = Vec::new();

        for stmt in &mut block.stmts {
            match &stmt.kind {
                // Handle let statements with complex expressions (existing logic)
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
                // Handle return statements with complex expressions
                hir::StmtKind::Return(Some(expr)) => {
                    match &expr.kind {
                        hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                            // Check if the condition contains nested complex expressions
                            if self.needs_transformation(condition) {
                                // Transform the condition to use a temporary variable
                                let temp_name = self.generate_temp_var_name();
                                let span = stmt.span;

                                // Create temporary variable assignments for the condition
                                let temp_assignments = self.create_temp_var_assignment_for_expr(
                                    ctx,
                                    &temp_name,
                                    condition.as_ref().clone(),
                                    span,
                                );
                                new_stmts.extend(temp_assignments);

                                // Create the modified return statement with temp variable as condition
                                let temp_path_expr = self.create_temp_var_path(ctx, &temp_name, span);
                                let modified_if_else = hir::Expr {
                                    hir_id: ctx.hir_id_allocator.next_id(),
                                    kind: hir::ExprKind::IfElse(
                                        Box::new(temp_path_expr),
                                        then_branch.clone(),
                                        else_branches.clone(),
                                    ),
                                    span,
                                };
                                let modified_return = hir::Stmt::new(
                                    stmt.hir_id,
                                    hir::StmtKind::Return(Some(Box::new(modified_if_else))),
                                    span,
                                );
                                new_stmts.push(modified_return);

                                self.changes_made = true;
                                continue;
                            }
                        }
                        _ => {
                            // For other return expressions, use the general transformation
                            if self.needs_transformation(expr) {
                                // Transform the complex expression in the return statement
                                let temp_name = self.generate_temp_var_name();
                                let span = stmt.span;

                                // Create the temp variable assignment for the complex expression
                                let temp_assignment = self.create_temp_var_assignment_for_expr(
                                    ctx, 
                                    &temp_name, 
                                    expr.as_ref().clone(), 
                                    span
                                );
                                new_stmts.extend(temp_assignment);

                                // Create the modified return statement using temp variable
                                let temp_path_expr = self.create_temp_var_path(ctx, &temp_name, span);
                                let modified_return = hir::Stmt::new(
                                    stmt.hir_id,
                                    hir::StmtKind::Return(Some(Box::new(temp_path_expr))),
                                    span,
                                );
                                new_stmts.push(modified_return);

                                self.changes_made = true;
                                continue;
                            }
                        }
                    }
                    
                    // If we didn't transform this return statement, just add it as-is
                    new_stmts.push(stmt.clone());
                }
                // Handle expression statements with complex expressions
                hir::StmtKind::Expr(_expr) => {
                    // Only transform expressions that are problematic in statement context
                    // Simple if-else statements should remain as if statements, not be transformed
                    // We only need to transform expressions that will be used in expression contexts
                    // For now, skip transformation of expression statements
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
            self.visit_expr(expr, ctx);
        }
    }
}
