use tlang_hir::hir;

use crate::generator::{BlockContext, CodegenJS};
use crate::expr_generator::expr_contains_return;

impl CodegenJS {
    pub(crate) fn generate_stmt(&mut self, statement: &hir::Stmt) {
        self.generate_comments(&statement.leading_comments);

        match &statement.kind {
            hir::StmtKind::Expr(expr) => {
                self.push_indent();
                self.push_context(BlockContext::Statement);
                self.generate_expr(expr, None, BlockContext::Statement);
                self.pop_context();
            }
            hir::StmtKind::Let(pattern, expression, _ty) => {
                // Check if the initializer requires statement transformation
                let needs_stmt_transform = match &expression.kind {
                    hir::ExprKind::Block(_) | hir::ExprKind::IfElse(_,_,_) | hir::ExprKind::Match(_,_) => {
                        expr_contains_return(expression)
                    }
                    _ => false,
                };

                if needs_stmt_transform {
                    // Handle complex expressions with returns by transforming to statements.
                    self.generate_variable_declaration_stmt_transform(pattern, expression);
                } else {
                    // Use existing logic for simple expressions or those without return.
                    self.generate_variable_declaration(pattern, expression);
                }
            }
            hir::StmtKind::FunctionDeclaration(decl) => self.generate_function_declaration(decl),
            hir::StmtKind::DynFunctionDeclaration(decl) => {
                self.generate_dyn_function_declaration(decl)
            }
            hir::StmtKind::Return(box expr) => self.generate_return_statement(expr.as_ref()),
            hir::StmtKind::EnumDeclaration(decl) => self.generate_enum_declaration(decl),
            hir::StmtKind::StructDeclaration(decl) => self.generate_struct_declaration(decl),
            hir::StmtKind::None => {}
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
        self.generate_expr(value, None, BlockContext::Expression);

        for (ident_pattern, var_name) in bindings {
            self.current_scope()
                .declare_variable_alias(ident_pattern.as_str(), &var_name);
        }

        self.pop_context();
        self.push_str(";\n");
    }

    /// Generates a variable declaration where the initializer expression
    /// contains a `return` and needs to be generated as statements.
    fn generate_variable_declaration_stmt_transform(
        &mut self,
        pattern: &hir::Pat,
        expression: &hir::Expr,
    ) {
        // 1. Declare the variable(s) from the pattern first.
        match &pattern.kind {
            hir::PatKind::Identifier(_, ident) => {
                let var_name = self.current_scope().declare_variable(ident.as_str());
                // Generate `let var_name;`
                self.push_indent();
                self.push_str(&format!("let {};\n", var_name));
                self.flush_statement_buffer(); // Ensure declaration is separate

                // 2. Generate the initializer expression as statements assigning to the var.
                self.generate_expr_assignment_stmt(expression, &var_name, None);
                self.flush_statement_buffer(); // Ensure assignment is separate

                self.current_scope()
                    .declare_variable_alias(ident.as_str(), &var_name);
            }
            // TODO: Handle list patterns and other patterns if needed for statement transform
            _ => {
                // Fallback or error for unsupported patterns in this context
                eprintln!("Warning: Statement transformation for let with complex pattern {:?} containing return is not fully implemented. Falling back to default generation.", pattern.kind);
                self.generate_variable_declaration(pattern, expression);
            }
        }
    }
}
