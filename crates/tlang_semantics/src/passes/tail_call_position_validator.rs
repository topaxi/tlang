use crate::{
    analyzer::{SemanticAnalysisContext, SemanticAnalysisPass},
    diagnostic,
};
use tlang_ast::{
    node::{
        Block, Expr, ExprKind, FunctionDeclaration, Module, Stmt, StmtKind,
    },
    visit::{Visitor, walk_expr, walk_stmt},
};
use tlang_span::NodeId;

/// Pass for validating that recursive call expressions (marked with `rec`)
/// are only used in tail call positions.
///
/// Valid tail call positions include:
/// - The last expression in a function body
/// - The last expression in blocks that are in tail position
/// - Expressions in if-else branches that are in tail position
/// - Expressions in match arms that are in tail position
/// - After return statements
#[derive(Default)]
pub struct TailCallPositionValidator {
    /// Stack to track whether we're currently in a tail position
    /// true = in tail position, false = not in tail position
    tail_position_stack: Vec<bool>,
}

impl TailCallPositionValidator {
    pub fn new() -> Self {
        Self::default()
    }

    /// Check if we're currently in a tail position
    fn is_in_tail_position(&self) -> bool {
        self.tail_position_stack.last().copied().unwrap_or(false)
    }

    /// Enter a new context with the specified tail position status
    fn enter_tail_context(&mut self, in_tail_position: bool) {
        self.tail_position_stack.push(in_tail_position);
    }

    /// Exit the current tail position context
    fn exit_tail_context(&mut self) {
        self.tail_position_stack.pop();
    }

    /// Visit an expression in a non-tail position
    fn visit_expr_non_tail(&mut self, expr: &Expr, ctx: &mut SemanticAnalysisContext) {
        self.enter_tail_context(false);
        self.visit_expr(expr, ctx);
        self.exit_tail_context();
    }

    /// Visit a block where the final expression is in tail position
    fn visit_block_tail(&mut self, block: &Block, ctx: &mut SemanticAnalysisContext) {
        self.enter_scope(block.id, ctx);

        // All statements are not in tail position
        for stmt in &block.statements {
            self.enter_tail_context(false);
            self.visit_stmt(stmt, ctx);
            self.exit_tail_context();
        }

        // The block's final expression inherits tail position from current context
        if let Some(expr) = &block.expression {
            self.visit_expr(expr, ctx);
        }

        self.leave_scope(block.id, ctx);
    }
}

impl SemanticAnalysisPass for TailCallPositionValidator {
    fn analyze(&mut self, module: &Module, ctx: &mut SemanticAnalysisContext, _is_root: bool) {
        // Reset state
        self.tail_position_stack.clear();

        // Visit the module (module level is not in tail position)
        self.enter_tail_context(false);
        self.visit_module(module, ctx);
        self.exit_tail_context();
    }
}

impl<'ast> Visitor<'ast> for TailCallPositionValidator {
    type Context = SemanticAnalysisContext;

    fn enter_scope(&mut self, _node_id: NodeId, _ctx: &mut Self::Context) {
        // We don't need to track symbol tables for this pass
    }

    fn leave_scope(&mut self, _node_id: NodeId, _ctx: &mut Self::Context) {
        // We don't need to track symbol tables for this pass
    }

    fn visit_stmt(&mut self, stmt: &'ast Stmt, ctx: &mut Self::Context) {
        match &stmt.kind {
            StmtKind::Return(Some(expr)) => {
                // Expression after return is in tail position
                self.enter_tail_context(true);
                self.visit_expr(expr, ctx);
                self.exit_tail_context();
            }
            StmtKind::Let(let_decl) => {
                // Let binding expressions are not in tail position
                self.enter_tail_context(false);
                self.visit_pat(&let_decl.pattern, ctx);
                self.visit_expr(&let_decl.expression, ctx);
                self.exit_tail_context();
            }
            StmtKind::Expr(expr) => {
                // Expressions in statements are not in tail position
                self.enter_tail_context(false);
                self.visit_expr(expr, ctx);
                self.exit_tail_context();
            }
            _ => {
                // All other statements are not in tail position
                self.enter_tail_context(false);
                walk_stmt(self, stmt, ctx);
                self.exit_tail_context();
            }
        }
    }

    fn visit_fn_decl(&mut self, decl: &'ast FunctionDeclaration, ctx: &mut Self::Context) {
        // Don't visit the function name expression
        self.enter_scope(decl.id, ctx);

        // Function parameters are not in tail position
        self.enter_tail_context(false);
        for parameter in &decl.parameters {
            self.visit_fn_param(parameter, ctx);
        }

        if let Some(guard) = &decl.guard {
            self.visit_expr(guard, ctx);
        }

        if let Some(return_type_annotation) = &decl.return_type_annotation {
            self.visit_fn_ret_ty(return_type_annotation, ctx);
        }
        self.exit_tail_context();

        // Function body is in tail position
        self.enter_tail_context(true);
        self.visit_fn_body(&decl.body, ctx);
        self.exit_tail_context();

        self.leave_scope(decl.id, ctx);
    }

    fn visit_expr(&mut self, expr: &'ast Expr, ctx: &mut Self::Context) {
        match &expr.kind {
            ExprKind::RecursiveCall(_) => {
                if !self.is_in_tail_position() {
                    ctx.add_diagnostic(diagnostic::error_at!(
                        expr.span,
                        "Recursive call (marked with 'rec') is not in tail position. Recursive calls can only be used as the final expression in a function, block, if-else branch, or match arm.",
                    ));
                }
                // Continue with normal call processing
                walk_expr(self, expr, ctx);
            }
            ExprKind::Block(block) => {
                // Block inherits tail position from current context
                self.visit_block_tail(block, ctx);
            }
            ExprKind::IfElse(if_else_expr) => {
                // Condition is not in tail position
                self.visit_expr_non_tail(&if_else_expr.condition, ctx);

                // Then branch inherits tail position from current context
                self.visit_block_tail(&if_else_expr.then_branch, ctx);

                // Else branches inherit tail position from current context
                for else_clause in &if_else_expr.else_branches {
                    if let Some(condition) = &else_clause.condition {
                        self.visit_expr_non_tail(condition, ctx);
                    }
                    self.visit_block_tail(&else_clause.consequence, ctx);
                }
            }
            ExprKind::Match(match_expr) => {
                // Match expression is not in tail position
                self.visit_expr_non_tail(&match_expr.expression, ctx);

                // Each match arm inherits tail position from current context
                for arm in &match_expr.arms {
                    self.enter_scope(arm.id, ctx);

                    // Pattern and guard are not in tail position
                    self.enter_tail_context(false);
                    self.visit_pat(&arm.pattern, ctx);
                    if let Some(guard) = &arm.guard {
                        self.visit_expr(guard, ctx);
                    }
                    self.exit_tail_context();

                    // Arm expression inherits tail position from current context
                    self.visit_expr(&arm.expression, ctx);

                    self.leave_scope(arm.id, ctx);
                }
            }
            ExprKind::FunctionExpression(decl) => {
                // Function expressions create their own tail context
                self.visit_fn_decl(decl, ctx);
            }
            // For all other expression types, sub-expressions are not in tail position
            ExprKind::BinaryOp(binary_expr) => {
                self.visit_expr_non_tail(&binary_expr.lhs, ctx);
                self.visit_expr_non_tail(&binary_expr.rhs, ctx);
            }
            ExprKind::UnaryOp(_, expr) => {
                self.visit_expr_non_tail(expr, ctx);
            }
            ExprKind::Call(call_expr) => {
                self.visit_expr_non_tail(&call_expr.callee, ctx);
                for arg in &call_expr.arguments {
                    self.visit_expr_non_tail(arg, ctx);
                }
            }
            ExprKind::FieldExpression(field_expr) => {
                self.visit_expr_non_tail(&field_expr.base, ctx);
            }
            ExprKind::IndexExpression(index_expr) => {
                self.visit_expr_non_tail(&index_expr.base, ctx);
                self.visit_expr_non_tail(&index_expr.index, ctx);
            }
            ExprKind::Let(pattern, expr) => {
                self.visit_pat(pattern, ctx);
                self.visit_expr_non_tail(expr, ctx);
            }
            ExprKind::List(exprs) => {
                for expr in exprs {
                    self.visit_expr_non_tail(expr, ctx);
                }
            }
            ExprKind::Dict(kvs) => {
                for (key, value) in kvs {
                    self.visit_expr_non_tail(key, ctx);
                    self.visit_expr_non_tail(value, ctx);
                }
            }
            ExprKind::Cast(expr, ty) => {
                self.visit_expr_non_tail(expr, ctx);
                self.visit_ty(ty, ctx);
            }
            _ => {
                // For other expressions, use default behavior
                walk_expr(self, expr, ctx);
            }
        }
    }
}