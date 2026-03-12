use tlang_hir as hir;

use crate::generator::{BlockContext, CodegenJS};

impl CodegenJS {
    pub(crate) fn generate_stmt(&mut self, statement: &hir::Stmt) {
        self.generate_comments(&statement.leading_comments);

        match &statement.kind {
            hir::StmtKind::Expr(expr) => {
                // Self-referencing TailCall handles its own indentation and
                // line breaks (it renders param reassignment + continue rec).
                // Mutual TailCall is a regular call — rendered normally.
                //
                // Naked block in stmt position (e.g. synthesized by the ANF
                // transform for break-value rewrites): inline its stmts
                // without an extra push_indent(), since inner stmts manage
                // their own indentation.
                if let hir::ExprKind::Block(block) = &expr.kind {
                    self.push_context(BlockContext::Statement);
                    self.generate_block(block);
                    self.pop_context();
                } else if self.is_self_referencing_tail_call(expr) {
                    self.push_context(BlockContext::Statement);
                    self.generate_expr(expr, None);
                    self.pop_context();
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
            hir::StmtKind::ProtocolDeclaration(decl) => {
                self.generate_protocol_declaration(decl);
            }
            hir::StmtKind::ImplBlock(impl_block) => {
                self.generate_impl_block(impl_block);
            }
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

    pub(crate) fn generate_protocol_declaration(&mut self, decl: &hir::ProtocolDeclaration) {
        let name = decl.name.as_str();
        self.push_indent();
        self.push_str("const ");
        self.push_str(name);
        self.push_str(" = {};\n");

        // Generate dispatch function for each protocol method
        for method in &decl.methods {
            let method_name = method.name.as_str();
            self.push_indent();
            self.push_str(name);
            self.push_char('.');
            self.push_str(method_name);
            self.push_str(" = function(self, ...args) {\n");
            self.inc_indent();
            self.push_indent();
            self.push_str("const __type = Array.isArray(self) ? \"List\" : self?.constructor?.name ?? typeof self;\n");
            self.push_indent();
            self.push_str("return ");
            self.push_str(name);
            self.push_str("[__type].");
            self.push_str(method_name);
            self.push_str("(self, ...args);\n");
            self.dec_indent();
            self.push_indent();
            self.push_str("};\n");
        }
    }

    pub(crate) fn generate_impl_block(&mut self, impl_block: &hir::ImplBlock) {
        let protocol_name = impl_block.protocol_name.to_string();
        let target_type = impl_block.target_type.to_string();

        // Ensure protocol has a namespace for this type
        self.push_indent();
        self.push_str(&protocol_name);
        self.push_char('.');
        self.push_str(&target_type);
        self.push_str(" = {};\n");

        for method in &impl_block.methods {
            let method_name = match &method.name.kind {
                hir::ExprKind::Path(path) => path.last_ident().to_string(),
                hir::ExprKind::FieldAccess(_, ident) => ident.to_string(),
                _ => unreachable!(),
            };

            self.push_indent();
            self.push_str(&protocol_name);
            self.push_char('.');
            self.push_str(&target_type);
            self.push_char('.');
            self.push_str(&method_name);
            self.push_str(" = ");
            self.generate_function_expression(method);
            self.push_str(";\n");
        }

        for apply_ident in &impl_block.apply_methods {
            let method_name = apply_ident.as_str();
            assert!(
                !is_builtin_type(&target_type),
                "Cannot use 'apply' for built-in type '{target_type}': \
                 applying methods to built-in types is not allowed to preserve backwards compatibility"
            );
            let proto = js_prototype_for_type(&target_type);
            self.push_indent();
            self.push_str("$installMethod(");
            self.push_str(&proto);
            self.push_str(", \"");
            self.push_str(method_name);
            self.push_str("\", ");
            self.push_str(&protocol_name);
            self.push_char('.');
            self.push_str(method_name);
            self.push_str(");\n");
        }
    }
}

fn is_builtin_type(type_name: &str) -> bool {
    matches!(type_name, "List" | "Option" | "Result" | "ListIterator")
}

fn js_prototype_for_type(type_name: &str) -> String {
    format!("{type_name}.prototype")
}
