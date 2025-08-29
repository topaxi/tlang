use tlang_hir::hir;

use crate::generator::{BlockContext, CodegenJS};

impl CodegenJS {
    pub(crate) fn generate_stmt(&mut self, statement: &hir::Stmt) {
        self.generate_comments(&statement.leading_comments);

        match &statement.kind {
            hir::StmtKind::Expr(expr) => {
                // Special handling for loops in statement context
                if let hir::ExprKind::Loop(block) = &expr.kind {
                    self.push_indent();
                    self.generate_loop_statement(block);
                    self.push_newline();
                } else {
                    self.push_indent();
                    self.push_context(BlockContext::Statement);
                    self.generate_expr(expr, None);
                    self.pop_context();

                    if self.needs_semicolon(Some(expr)) {
                        self.push_char(';');
                    }
                    self.push_newline();
                }
            }
            hir::StmtKind::Let(pattern, expression, _ty) => {
                self.generate_variable_declaration(pattern, expression);
            }
            hir::StmtKind::FunctionDeclaration(decl) => self.generate_function_declaration(decl),
            hir::StmtKind::DynFunctionDeclaration(decl) => {
                self.generate_dyn_function_declaration(decl);
            }
            hir::StmtKind::Return(expr) => self.generate_return_statement(expr.as_deref()),
            hir::StmtKind::EnumDeclaration(decl) => self.generate_enum_declaration(decl),
            hir::StmtKind::StructDeclaration(decl) => self.generate_struct_declaration(decl),
        }

        self.generate_comments(&statement.trailing_comments);
    }

    #[inline(always)]
    pub(crate) fn generate_statements(&mut self, statements: &[hir::Stmt]) {
        for statement in statements {
            self.generate_stmt(statement);
            self.flush_statement_buffer();
        }
    }

    pub(crate) fn generate_variable_declaration(&mut self, pattern: &hir::Pat, value: &hir::Expr) {
        match &pattern.kind {
            hir::PatKind::Identifier(_, ident) => {
                self.generate_variable_declaration_identifier(ident.as_str(), value);
            }
            hir::PatKind::List(patterns) => {
                self.generate_variable_declaration_list_pattern(patterns, value);
            }
            _ => todo!("Variable declaration pattern matching is not implemented yet."),
        }
    }

    fn generate_variable_declaration_identifier(&mut self, name: &str, value: &hir::Expr) {
        let shadowed_name = self.current_scope().resolve_variable(name);
        let var_name = self.current_scope().declare_variable(name);
        self.push_context(BlockContext::Expression);
        if let Some(shadowed_name) = shadowed_name {
            self.current_scope()
                .declare_variable_alias(name, &shadowed_name);
        }
        self.push_let_declaration_to_expr(&var_name, value);
        self.push_str(";\n");
        self.current_scope().declare_variable_alias(name, &var_name);
        self.pop_context();
    }

    fn generate_variable_declaration_list_pattern(
        &mut self,
        patterns: &[hir::Pat],
        value: &hir::Expr,
    ) {
        self.push_indent();
        self.push_str("let [");

        let mut bindings = vec![];

        for (i, pattern) in patterns.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            match &pattern.kind {
                hir::PatKind::Identifier(_, ident) => {
                    let shadowed_name = self.current_scope().resolve_variable(ident.as_str());
                    let var_name = self.current_scope().declare_variable(ident.as_str());
                    self.push_str(&var_name);
                    if let Some(shadowed_name) = shadowed_name {
                        self.current_scope()
                            .declare_variable_alias(ident.as_str(), &shadowed_name);
                    }
                    bindings.push((ident, var_name));
                }
                hir::PatKind::Rest(pattern) => {
                    self.push_str("...");
                    self.generate_pat(pattern);
                }
                hir::PatKind::Wildcard => {}
                pattern_kind => todo!(
                    "Variable declaration pattern matching for {:?} is not implemented yet.",
                    pattern_kind
                ),
            }
        }

        self.push_str("] = ");
        self.push_context(BlockContext::Expression);
        self.generate_expr(value, None);

        for (ident_pattern, var_name) in bindings {
            self.current_scope()
                .declare_variable_alias(ident_pattern.as_str(), &var_name);
        }

        self.pop_context();
        self.push_str(";\n");
    }

    /// Generate a loop in statement context (not expression context)
    fn generate_loop_statement(&mut self, block: &hir::Block) {
        self.push_str("for (;;) {");
        self.push_newline();
        self.inc_indent();

        for stmt in &block.stmts {
            self.generate_stmt_in_loop_context(stmt);
        }

        if let Some(completion_expr) = &block.expr {
            self.push_indent();
            self.generate_expr_in_loop_context(completion_expr, None);
            if self.needs_semicolon_in_loop_context(completion_expr) {
                self.push_char(';');
            }
            self.push_newline();
        }

        self.dec_indent();
        self.push_indent();
        self.push_str("}");
    }

    /// Generate a statement within a statement loop context
    fn generate_stmt_in_loop_context(&mut self, stmt: &hir::Stmt) {
        match &stmt.kind {
            hir::StmtKind::Expr(expr) => {
                self.push_indent();
                self.generate_expr_in_loop_context(expr, None);
                if self.needs_semicolon_in_loop_context(expr) {
                    self.push_char(';');
                }
                self.push_newline();
            }
            _ => {
                // For other statement types, use normal generation
                self.generate_stmt(stmt);
            }
        }
    }

    /// Generate an expression within a statement loop context (break becomes break, not return)
    fn generate_expr_in_loop_context(
        &mut self,
        expr: &hir::Expr,
        parent_op: Option<hir::BinaryOpKind>,
    ) {
        match &expr.kind {
            hir::ExprKind::Break(break_expr) => {
                self.push_str("break");
                if let Some(break_value) = break_expr {
                    // Note: break with value in statement loop context is unusual
                    // but we'll generate it anyway
                    self.push_char(' ');
                    self.generate_expr_in_loop_context(break_value, parent_op);
                }
            }
            hir::ExprKind::Continue => {
                self.push_str("continue");
            }
            hir::ExprKind::Block(block) => {
                self.push_str("{");
                self.push_newline();
                self.inc_indent();

                for stmt in &block.stmts {
                    self.generate_stmt_in_loop_context(stmt);
                }

                if let Some(completion_expr) = &block.expr {
                    self.push_indent();
                    self.generate_expr_in_loop_context(completion_expr, None);
                    if self.needs_semicolon_in_loop_context(completion_expr) {
                        self.push_char(';');
                    }
                    self.push_newline();
                }

                self.dec_indent();
                self.push_indent();
                self.push_str("}");
            }
            hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                self.push_str("if (");
                self.generate_expr_in_loop_context(condition, None);
                self.push_str(") ");
                self.generate_block_in_loop_context(then_branch);

                for else_clause in else_branches {
                    if let Some(else_condition) = &else_clause.condition {
                        self.push_str(" else if (");
                        self.generate_expr_in_loop_context(else_condition, None);
                        self.push_str(") ");
                    } else {
                        self.push_str(" else ");
                    }
                    self.generate_block_in_loop_context(&else_clause.consequence);
                }
            }
            _ => {
                // For other expressions, use normal generation
                self.push_context(BlockContext::Statement);
                self.generate_expr(expr, parent_op);
                self.pop_context();
            }
        }
    }

    /// Generate a block within a statement loop context
    fn generate_block_in_loop_context(&mut self, block: &hir::Block) {
        self.push_str("{");
        self.push_newline();
        self.inc_indent();

        for stmt in &block.stmts {
            self.generate_stmt_in_loop_context(stmt);
        }

        if let Some(completion_expr) = &block.expr {
            self.push_indent();
            self.generate_expr_in_loop_context(completion_expr, None);
            if self.needs_semicolon_in_loop_context(completion_expr) {
                self.push_char(';');
            }
            self.push_newline();
        }

        self.dec_indent();
        self.push_indent();
        self.push_str("}");
    }

    /// Check if an expression needs a semicolon in loop context
    fn needs_semicolon_in_loop_context(&self, expr: &hir::Expr) -> bool {
        !matches!(
            expr.kind,
            hir::ExprKind::Block(..)
                | hir::ExprKind::IfElse(..)
                | hir::ExprKind::Break(..)
                | hir::ExprKind::Continue
        )
    }
}
