use tlang_ast::node::Ident;
use tlang_hir::hir;
use tlang_hir_opt::HirOptContext;
use tlang_span::Span;

use crate::js_expr_utils::{
    expr_can_render_as_assignment_rhs, expr_can_render_as_js_expr, expr_can_render_as_js_stmt,
};

/// Result of transforming an expression
pub struct TransformResult {
    /// The resulting expression (often a temp variable reference)
    pub expr: hir::Expr,
    /// Additional statements that need to be executed before the expression
    pub statements: Vec<hir::Stmt>,
}

/// Shared utilities for temp variable management
#[derive(Debug)]
pub struct TempVarManager {
    counter: usize,
}

impl TempVarManager {
    pub fn new() -> Self {
        Self { counter: 0 }
    }

    pub fn new_with_counter(starting_counter: usize) -> Self {
        Self { counter: starting_counter }
    }

    pub fn current_counter(&self) -> usize {
        self.counter
    }

    pub fn set_counter(&mut self, counter: usize) {
        self.counter = counter;
    }

    pub fn generate_name(&mut self) -> String {
        let name = format!("$hir${}", self.counter);
        self.counter += 1;
        name
    }

    pub fn create_path(&self, ctx: &mut HirOptContext, name: &str, span: Span) -> hir::Expr {
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

    pub fn create_declaration(&self, ctx: &mut HirOptContext, name: &str, span: Span) -> hir::Stmt {
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
            hir::StmtKind::Let(
                Box::new(pattern),
                Box::new(wildcard_expr),
                Box::new(default_ty),
            ),
            span,
        )
    }

    pub fn is_temp_var(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Path(path) => {
                if let Some(first_segment) = path.segments.first() {
                    first_segment.ident.as_str().starts_with("$hir$")
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    /// Check if any variable name in the expression starts with $hir$ (global temp var detection)
    pub fn is_any_temp_var(expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Path(path) => {
                if let Some(first_segment) = path.segments.first() {
                    first_segment.ident.as_str().starts_with("$hir$")
                } else {
                    false
                }
            }
            _ => false,
        }
    }
}

/// Shared utilities for building statements
#[derive(Debug)]
pub struct StatementBuilder {
    temp_var_manager: TempVarManager,
}

impl StatementBuilder {
    pub fn new() -> Self {
        Self {
            temp_var_manager: TempVarManager::new(),
        }
    }

    pub fn new_with_counter(starting_counter: usize) -> Self {
        Self {
            temp_var_manager: TempVarManager::new_with_counter(starting_counter),
        }
    }

    pub fn create_assignment(
        &self,
        ctx: &mut HirOptContext,
        var_name: &str,
        value_expr: hir::Expr,
        span: Span,
    ) -> hir::Stmt {
        // Check if this would create a self-assignment (e.g., $hir$0 = $hir$0)
        // If so, create a no-op statement instead
        if Self::is_self_assignment(var_name, &value_expr) {
            // Return a no-op expression statement instead of a self-assignment
            let noop_expr = hir::Expr {
                hir_id: ctx.hir_id_allocator.next_id(),
                kind: hir::ExprKind::Path(Box::new(hir::Path::new(
                    vec![hir::PathSegment { 
                        ident: tlang_ast::node::Ident::new("undefined", span) 
                    }],
                    span,
                ))),
                span,
            };
            
            return hir::Stmt::new(
                ctx.hir_id_allocator.next_id(),
                hir::StmtKind::Expr(Box::new(noop_expr)),
                span,
            );
        }

        let hir_id = ctx.hir_id_allocator.next_id();
        let temp_path = self.temp_var_manager.create_path(ctx, var_name, span);

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

    /// Check if an assignment would be a self-assignment (var = var)
    fn is_self_assignment(var_name: &str, value_expr: &hir::Expr) -> bool {
        match &value_expr.kind {
            hir::ExprKind::Path(path) => {
                if path.segments.len() == 1 {
                    path.segments[0].ident.as_str() == var_name
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    pub fn temp_var_manager(&mut self) -> &mut TempVarManager {
        &mut self.temp_var_manager
    }
}

/// Shared utilities for analyzing expressions
pub struct ExpressionAnalyzer;

impl ExpressionAnalyzer {
    pub fn can_render_as_js_expr(expr: &hir::Expr) -> bool {
        expr_can_render_as_js_expr(expr)
    }

    pub fn can_render_as_js_stmt(expr: &hir::Expr) -> bool {
        expr_can_render_as_js_stmt(expr)
    }

    pub fn can_render_as_assignment_rhs(expr: &hir::Expr) -> bool {
        expr_can_render_as_assignment_rhs(expr)
    }

    pub fn contains_break_with_value(expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Break(Some(_)) => true,
            hir::ExprKind::Break(None) | hir::ExprKind::Continue => false,
            hir::ExprKind::Block(block) => Self::block_contains_break_with_value(block),
            hir::ExprKind::IfElse(cond, then_branch, else_branches) => {
                Self::contains_break_with_value(cond)
                    || Self::block_contains_break_with_value(then_branch)
                    || else_branches
                        .iter()
                        .any(|clause| Self::block_contains_break_with_value(&clause.consequence))
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                Self::contains_break_with_value(scrutinee)
                    || arms
                        .iter()
                        .any(|arm| Self::block_contains_break_with_value(&arm.block))
            }
            hir::ExprKind::Binary(_, lhs, rhs) => {
                Self::contains_break_with_value(lhs) || Self::contains_break_with_value(rhs)
            }
            hir::ExprKind::Unary(_, operand) => Self::contains_break_with_value(operand),
            hir::ExprKind::Call(call) => call
                .arguments
                .iter()
                .any(|arg| Self::contains_break_with_value(arg)),
            hir::ExprKind::List(elements) => elements
                .iter()
                .any(|elem| Self::contains_break_with_value(elem)),
            hir::ExprKind::FieldAccess(obj, _) => Self::contains_break_with_value(obj),
            hir::ExprKind::IndexAccess(obj, index) => {
                Self::contains_break_with_value(obj) || Self::contains_break_with_value(index)
            }
            hir::ExprKind::Loop(body) => Self::block_contains_break_with_value(body),
            _ => false,
        }
    }

    pub fn block_contains_break_with_value(block: &hir::Block) -> bool {
        for stmt in &block.stmts {
            if Self::stmt_contains_break_with_value(stmt) {
                return true;
            }
        }

        if let Some(expr) = &block.expr {
            if Self::contains_break_with_value(expr) {
                return true;
            }
        }

        false
    }

    pub fn stmt_contains_break_with_value(stmt: &hir::Stmt) -> bool {
        match &stmt.kind {
            hir::StmtKind::Expr(expr) => Self::contains_break_with_value(expr),
            hir::StmtKind::Let(_, expr, _) => Self::contains_break_with_value(expr),
            hir::StmtKind::Return(Some(expr)) => Self::contains_break_with_value(expr),
            _ => false,
        }
    }

    pub fn contains_return_statements(expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::Block(block) => Self::block_contains_return_statements(block),
            hir::ExprKind::IfElse(cond, then_branch, else_branches) => {
                Self::contains_return_statements(cond)
                    || Self::block_contains_return_statements(then_branch)
                    || else_branches
                        .iter()
                        .any(|clause| Self::block_contains_return_statements(&clause.consequence))
            }
            hir::ExprKind::Match(scrutinee, arms) => {
                Self::contains_return_statements(scrutinee)
                    || arms
                        .iter()
                        .any(|arm| Self::block_contains_return_statements(&arm.block))
            }
            hir::ExprKind::Binary(_, lhs, rhs) => {
                Self::contains_return_statements(lhs) || Self::contains_return_statements(rhs)
            }
            hir::ExprKind::Unary(_, operand) => Self::contains_return_statements(operand),
            hir::ExprKind::Call(call) => call
                .arguments
                .iter()
                .any(|arg| Self::contains_return_statements(arg)),
            hir::ExprKind::List(elements) => elements
                .iter()
                .any(|elem| Self::contains_return_statements(elem)),
            hir::ExprKind::FieldAccess(obj, _) => Self::contains_return_statements(obj),
            hir::ExprKind::IndexAccess(obj, index) => {
                Self::contains_return_statements(obj) || Self::contains_return_statements(index)
            }
            _ => false,
        }
    }

    pub fn block_contains_return_statements(block: &hir::Block) -> bool {
        for stmt in &block.stmts {
            if Self::stmt_contains_return_statements(stmt) {
                return true;
            }
        }

        if let Some(expr) = &block.expr {
            if Self::contains_return_statements(expr) {
                return true;
            }
        }

        false
    }

    pub fn stmt_contains_return_statements(stmt: &hir::Stmt) -> bool {
        match &stmt.kind {
            hir::StmtKind::Return(_) => true,
            hir::StmtKind::Expr(expr) => Self::contains_return_statements(expr),
            hir::StmtKind::Let(_, expr, _) => Self::contains_return_statements(expr),
            _ => false,
        }
    }

    pub fn contains_temp_variables(expr: &hir::Expr, temp_var_manager: &TempVarManager) -> bool {
        if temp_var_manager.is_temp_var(expr) {
            return true;
        }

        Self::contains_any_temp_variables(expr)
    }

    /// Check if expression contains any temp variables (global detection)
    pub fn contains_any_temp_variables(expr: &hir::Expr) -> bool {
        if TempVarManager::is_any_temp_var(expr) {
            return true;
        }

        match &expr.kind {
            hir::ExprKind::Binary(_, lhs, rhs) => {
                Self::contains_any_temp_variables(lhs) || Self::contains_any_temp_variables(rhs)
            }
            hir::ExprKind::Unary(_, operand) => Self::contains_any_temp_variables(operand),
            hir::ExprKind::Call(call) => call
                .arguments
                .iter()
                .any(|arg| Self::contains_any_temp_variables(arg)),
            hir::ExprKind::List(elements) => elements
                .iter()
                .any(|elem| Self::contains_any_temp_variables(elem)),
            hir::ExprKind::FieldAccess(obj, _) => Self::contains_any_temp_variables(obj),
            hir::ExprKind::Loop(block) => Self::block_contains_any_temp_variables(block),
            hir::ExprKind::Block(block) => Self::block_contains_any_temp_variables(block),
            hir::ExprKind::Match(scrutinee, arms) => {
                if Self::contains_any_temp_variables(scrutinee) {
                    return true;
                }
                arms.iter()
                    .any(|arm| Self::block_contains_any_temp_variables(&arm.block))
            }
            _ => false,
        }
    }

    pub fn block_contains_any_temp_variables(block: &hir::Block) -> bool {
        for stmt in &block.stmts {
            if Self::stmt_contains_any_temp_variables(stmt) {
                return true;
            }
        }

        if let Some(ref expr) = block.expr {
            if Self::contains_any_temp_variables(expr) {
                return true;
            }
        }

        false
    }

    pub fn stmt_contains_any_temp_variables(stmt: &hir::Stmt) -> bool {
        match &stmt.kind {
            hir::StmtKind::Let(_, expr, _) => Self::contains_any_temp_variables(expr),
            hir::StmtKind::Expr(expr) => Self::contains_any_temp_variables(expr),
            hir::StmtKind::Return(Some(expr)) => Self::contains_any_temp_variables(expr),
            hir::StmtKind::Return(None) => false,
            _ => false,
        }
    }

    pub fn block_contains_temp_variables(
        block: &hir::Block,
        temp_var_manager: &TempVarManager,
    ) -> bool {
        for stmt in &block.stmts {
            if Self::stmt_contains_temp_variables(stmt, temp_var_manager) {
                return true;
            }
        }

        if let Some(ref expr) = block.expr {
            if Self::contains_temp_variables(expr, temp_var_manager) {
                return true;
            }
        }

        false
    }

    pub fn stmt_contains_temp_variables(
        stmt: &hir::Stmt,
        temp_var_manager: &TempVarManager,
    ) -> bool {
        match &stmt.kind {
            hir::StmtKind::Let(_, expr, _) => Self::contains_temp_variables(expr, temp_var_manager),
            hir::StmtKind::Expr(expr) => Self::contains_temp_variables(expr, temp_var_manager),
            hir::StmtKind::Return(Some(expr)) => {
                Self::contains_temp_variables(expr, temp_var_manager)
            }
            hir::StmtKind::Return(None) => false,
            _ => false,
        }
    }
}

/// Generic transformation strategy interface
pub trait TransformationStrategy: std::fmt::Debug {
    fn should_transform(&self, expr: &hir::Expr) -> bool;
    fn transform(
        &mut self,
        expr: hir::Expr,
        ctx: &mut HirOptContext,
        stmt_builder: &mut StatementBuilder,
    ) -> TransformResult;
}

/// Generic expression transformer that uses pluggable strategies
#[derive(Debug)]
pub struct ExpressionTransformer {
    strategies: Vec<Box<dyn TransformationStrategy>>,
    stmt_builder: StatementBuilder,
}

impl ExpressionTransformer {
    pub fn new() -> Self {
        Self {
            strategies: Vec::new(),
            stmt_builder: StatementBuilder::new(),
        }
    }

    /// Create a new ExpressionTransformer with a specific starting counter
    /// This ensures nested transformers don't reuse temp variable names
    pub fn new_with_counter(starting_counter: usize) -> Self {
        Self {
            strategies: Vec::new(),
            stmt_builder: StatementBuilder::new_with_counter(starting_counter),
        }
    }

    pub fn add_strategy(&mut self, strategy: Box<dyn TransformationStrategy>) {
        self.strategies.push(strategy);
    }

    pub fn current_counter(&self) -> usize {
        self.stmt_builder.temp_var_manager.current_counter()
    }

    pub fn transform_expression(
        &mut self,
        expr: hir::Expr,
        ctx: &mut HirOptContext,
    ) -> TransformResult {
            
        // Find the first strategy that can handle this expression
        for (i, strategy) in self.strategies.iter_mut().enumerate() {
            if strategy.should_transform(&expr) {
                return strategy.transform(expr, ctx, &mut self.stmt_builder);
            }
        }

        eprintln!("DEBUG: No strategy matched, returning expression as-is");
        // Default: no transformation needed
        TransformResult {
            expr,
            statements: Vec::new(),
        }
    }

    pub fn temp_var_manager(&mut self) -> &mut TempVarManager {
        self.stmt_builder.temp_var_manager()
    }
}