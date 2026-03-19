use oxc_ast::NONE;
use oxc_ast::ast::*;
use oxc_span::SPAN;
use tlang_ast::node::Visibility;
use tlang_ast::token::Literal;
use tlang_hir as hir;

use crate::generator::{CodegenJS, InnerCodegen};

impl<'a> InnerCodegen<'a> {
    pub fn generate_stmts(&mut self, stmts: &[hir::Stmt]) -> Vec<Statement<'a>> {
        let mut result = Vec::new();
        for stmt in stmts {
            let comments = &stmt.leading_comments;
            let mut generated = self.generate_stmt(stmt);
            if !comments.is_empty()
                && let Some(first) = generated.first_mut()
            {
                self.attach_leading_comments(first, comments);
            }
            result.extend(generated);
        }
        result
    }

    pub fn generate_stmt(&mut self, statement: &hir::Stmt) -> Vec<Statement<'a>> {
        match &statement.kind {
            hir::StmtKind::Expr(expr) => self.generate_expr_stmt(expr),
            hir::StmtKind::Let(pattern, expression, _ty) => {
                self.generate_variable_declaration(pattern, expression)
            }
            hir::StmtKind::FunctionDeclaration(decl) => {
                let stmts = self.generate_function_declaration(decl);
                if decl.visibility == Visibility::Public {
                    stmts
                        .into_iter()
                        .map(|s| self.wrap_export_named(s))
                        .collect()
                } else {
                    stmts
                }
            }
            hir::StmtKind::DynFunctionDeclaration(decl) => {
                self.generate_dyn_function_declaration(decl)
            }
            hir::StmtKind::Return(expr) => self.generate_return_stmt(expr.as_deref()),
            hir::StmtKind::EnumDeclaration(decl) => {
                let stmt = self.generate_enum_declaration(decl);
                if decl.visibility == Visibility::Public {
                    vec![self.wrap_export_named(stmt)]
                } else {
                    vec![stmt]
                }
            }
            hir::StmtKind::StructDeclaration(decl) => {
                let stmt = self.generate_struct_declaration(decl);
                if decl.visibility == Visibility::Public {
                    vec![self.wrap_export_named(stmt)]
                } else {
                    vec![stmt]
                }
            }
            hir::StmtKind::ProtocolDeclaration(decl) => {
                let stmts = self.generate_protocol_declaration(decl);
                if decl.visibility == Visibility::Public {
                    stmts
                        .into_iter()
                        .map(|s| self.wrap_export_named(s))
                        .collect()
                } else {
                    stmts
                }
            }
            hir::StmtKind::ImplBlock(impl_block) => self.generate_impl_block(impl_block),
        }
    }

    /// Wrap a statement in an `export` declaration. If the statement contains
    /// a declaration (function, class, variable), it becomes
    /// `export <declaration>`. Otherwise the statement is returned unchanged.
    fn wrap_export_named(&self, stmt: Statement<'a>) -> Statement<'a> {
        let declaration = match stmt {
            Statement::FunctionDeclaration(func) => Declaration::FunctionDeclaration(func),
            Statement::ClassDeclaration(class) => Declaration::ClassDeclaration(class),
            Statement::VariableDeclaration(var) => Declaration::VariableDeclaration(var),
            other => return other,
        };

        let export = self.ast.module_declaration_export_named_declaration(
            SPAN,
            Some(declaration),
            self.ast.vec(),
            None::<StringLiteral<'a>>,
            ImportOrExportKind::Value,
            NONE,
        );
        Statement::from(export)
    }

    fn generate_expr_stmt(&mut self, expr: &hir::Expr) -> Vec<Statement<'a>> {
        match &expr.kind {
            // Naked block in stmt position: inline its stmts.
            hir::ExprKind::Block(block) => self.generate_block_stmts_propagate_scope(block),
            // Self-referencing tail call emits param reassignment + continue rec.
            hir::ExprKind::TailCall(call_expr) if self.is_self_referencing_tail_call(expr) => {
                self.generate_recursive_call_stmts(call_expr)
            }
            // If-else as statement
            hir::ExprKind::IfElse(cond, then_branch, else_branches) => {
                self.generate_if_else_stmts(cond, then_branch, else_branches)
            }
            // Match as statement
            hir::ExprKind::Match(match_expr, arms) => self.generate_match_stmts(match_expr, arms),
            // Loop as statement
            hir::ExprKind::Loop(block) => self.generate_loop_stmts(block),
            // Break
            hir::ExprKind::Break(value) => self.generate_break_stmts(value),
            // Continue
            hir::ExprKind::Continue => vec![self.ast.statement_continue(SPAN, None)],
            // Everything else: wrap in ExpressionStatement
            _ => {
                let e = self.generate_expr(expr);
                vec![self.expr_stmt(e)]
            }
        }
    }

    fn generate_variable_declaration(
        &mut self,
        pattern: &hir::Pat,
        value: &hir::Expr,
    ) -> Vec<Statement<'a>> {
        match &pattern.kind {
            hir::PatKind::Identifier(_, ident) => {
                self.generate_variable_declaration_identifier(ident.as_str(), value)
            }
            hir::PatKind::List(patterns) => {
                self.generate_variable_declaration_list_pattern(patterns, value)
            }
            _ => todo!("Variable declaration pattern matching is not implemented yet."),
        }
    }

    fn generate_variable_declaration_identifier(
        &mut self,
        name: &str,
        value: &hir::Expr,
    ) -> Vec<Statement<'a>> {
        let shadowed_name = self.current_scope().resolve_variable(name);
        let var_name = self.current_scope().declare_variable(name);

        if let Some(ref shadowed_name) = shadowed_name {
            self.current_scope()
                .declare_variable_alias(name, shadowed_name);
        }

        // `let foo;` for None-initialised ANF temporaries
        let init = if matches!(&value.kind, hir::ExprKind::Literal(l) if **l == Literal::None) {
            None
        } else {
            Some(self.generate_expr(value))
        };

        self.current_scope().declare_variable_alias(name, &var_name);

        vec![self.let_decl(&var_name, init)]
    }

    fn generate_variable_declaration_list_pattern(
        &mut self,
        patterns: &[hir::Pat],
        value: &hir::Expr,
    ) -> Vec<Statement<'a>> {
        let mut elements = Vec::new();
        let mut rest = None;
        let mut bindings = Vec::new();

        for pattern in patterns {
            match &pattern.kind {
                hir::PatKind::Identifier(_, ident) => {
                    let shadowed_name = self.current_scope().resolve_variable(ident.as_str());
                    let var_name = self.current_scope().declare_variable(ident.as_str());
                    if let Some(ref shadowed_name) = shadowed_name {
                        self.current_scope()
                            .declare_variable_alias(ident.as_str(), shadowed_name);
                    }
                    bindings.push((ident.to_string(), var_name.clone()));
                    elements.push(Some(self.binding_pattern_ident(&var_name)));
                }
                hir::PatKind::Rest(inner) => {
                    if let hir::PatKind::Identifier(_, ident) = &inner.kind {
                        let var_name = self.current_scope().declare_variable(ident.as_str());
                        bindings.push((ident.to_string(), var_name.clone()));
                        rest = Some(self.binding_pattern_ident(&var_name));
                    }
                }
                hir::PatKind::Wildcard => {
                    elements.push(None);
                }
                _ => todo!(
                    "Variable declaration pattern matching for {:?} is not implemented yet.",
                    pattern.kind
                ),
            }
        }

        let init = self.generate_expr(value);

        for (ident_name, var_name) in &bindings {
            self.current_scope()
                .declare_variable_alias(ident_name, var_name);
        }

        vec![self.let_array_destructuring(self.ast.vec_from_iter(elements), rest, init)]
    }

    #[allow(clippy::too_many_lines)]
    pub fn generate_protocol_declaration(
        &mut self,
        decl: &hir::ProtocolDeclaration,
    ) -> Vec<Statement<'a>> {
        let name = decl.name.as_str();
        self.register_protocol(name);
        let js_name = CodegenJS::protocol_js_name(name);

        let mut stmts = Vec::new();

        // const $Protocol = {};
        stmts.push(self.const_decl(&js_name, self.ast.expression_object(SPAN, self.ast.vec())));

        // Generate dispatch function for each protocol method
        for method in &decl.methods {
            let method_name = method.name.as_str();

            // $Protocol.methodName = function(self, ...args) {
            //   const __type = Array.isArray(self) ? "List" : self?.constructor?.name ?? typeof self;
            //   const __impl = $Protocol[__type] ?? $Protocol.__default;
            //   return __impl.methodName(self, ...args);
            // };
            let self_param = self.formal_param("self");
            let rest_binding = self.binding_pattern_ident("args");
            let rest_elem = self.ast.binding_rest_element(SPAN, rest_binding);
            let rest = self
                .ast
                .formal_parameter_rest(SPAN, self.ast.vec(), rest_elem, NONE);
            let params = self.ast.formal_parameters(
                SPAN,
                FormalParameterKind::FormalParameter,
                self.ast.vec1(self_param),
                Some(rest),
            );

            // const __type = Array.isArray(self) ? "List" : self?.constructor?.name ?? typeof self;
            let is_array_call = self.call_expr(
                self.static_member_expr(self.ident_expr("Array"), "isArray"),
                vec![Argument::from(self.ident_expr("self"))],
            );
            let constructor_name = self.chain_expr(self.optional_static_member_expr(
                self.optional_static_member_expr(self.ident_expr("self"), "constructor"),
                "name",
            ));
            let typeof_self =
                self.ast
                    .expression_unary(SPAN, UnaryOperator::Typeof, self.ident_expr("self"));
            let nullish = self.ast.expression_logical(
                SPAN,
                constructor_name,
                LogicalOperator::Coalesce,
                typeof_self,
            );
            let type_expr = self.ast.expression_conditional(
                SPAN,
                is_array_call,
                self.str_expr("List"),
                nullish,
            );
            let type_decl = self.const_decl("__type", type_expr);

            // const __impl = $Protocol[__type] ?? $Protocol.__default;
            let proto_type_access =
                self.computed_member_expr(self.ident_expr(&js_name), self.ident_expr("__type"));
            let proto_default_access =
                self.static_member_expr(self.ident_expr(&js_name), "__default");
            let impl_expr = self.ast.expression_logical(
                SPAN,
                proto_type_access,
                LogicalOperator::Coalesce,
                proto_default_access,
            );
            let impl_decl = self.const_decl("__impl", impl_expr);

            // return __impl.methodName(self, ...args);
            let method_access = self.static_member_expr(self.ident_expr("__impl"), method_name);
            let spread_args = Argument::SpreadElement(
                self.ast.alloc_spread_element(SPAN, self.ident_expr("args")),
            );
            let dispatch_call = self.call_expr(
                method_access,
                vec![Argument::from(self.ident_expr("self")), spread_args],
            );
            let return_stmt = self.ast.statement_return(SPAN, Some(dispatch_call));

            let body = self.fn_body(self.ast.vec_from_iter([type_decl, impl_decl, return_stmt]));
            let func = self.ast.expression_function(
                SPAN,
                FunctionType::FunctionExpression,
                None,
                false,
                false,
                false,
                NONE,
                NONE,
                params,
                NONE,
                Some(body),
            );

            // $Protocol.method = func;
            let target = self.assignment_target_member(self.ident_expr(&js_name), method_name);
            let assign = self.assign_expr(target, func);
            stmts.push(self.expr_stmt(assign));
        }

        // Generate default implementation functions for methods with bodies
        for method in &decl.methods {
            if method.body.is_some() {
                let method_name = method.name.as_str();

                // $Protocol.__default = $Protocol.__default || {};
                let default_access =
                    self.static_member_expr(self.ident_expr(&js_name), "__default");
                let default_or_obj = self.ast.expression_logical(
                    SPAN,
                    default_access,
                    LogicalOperator::Or,
                    self.ast.expression_object(SPAN, self.ast.vec()),
                );
                let target = self.assignment_target_member(self.ident_expr(&js_name), "__default");
                let assign = self.assign_expr(target, default_or_obj);
                stmts.push(self.expr_stmt(assign));

                // $Protocol.__default.methodName = function(self, ...args) { <body> };
                let fn_decl = self.build_fn_decl_from_protocol_method(method);
                let fn_expr = self.generate_function_expression(&fn_decl);

                let default_obj = self.static_member_expr(self.ident_expr(&js_name), "__default");
                let target = self.assignment_target_member(default_obj, method_name);
                let assign = self.assign_expr(target, fn_expr);
                stmts.push(self.expr_stmt(assign));
            }
        }

        stmts
    }

    fn build_fn_decl_from_protocol_method(
        &self,
        method: &hir::ProtocolMethodSignature,
    ) -> hir::FunctionDeclaration {
        let name = hir::Expr {
            hir_id: method.hir_id,
            kind: hir::ExprKind::Path(Box::new(hir::Path::new(
                vec![hir::PathSegment::new(method.name)],
                method.span,
            ))),
            span: method.span,
        };

        hir::FunctionDeclaration {
            hir_id: method.hir_id,
            visibility: Visibility::Private,
            name,
            parameters: method.parameters.clone(),
            return_type: method.return_type.clone(),
            body: method.body.clone().unwrap_or_else(|| hir::Block {
                hir_id: method.hir_id,
                stmts: vec![],
                expr: None,
                scope: Default::default(),
                span: method.span,
            }),
            span: method.span,
        }
    }

    pub fn generate_impl_block(&mut self, impl_block: &hir::ImplBlock) -> Vec<Statement<'a>> {
        let protocol_name = impl_block.protocol_name.to_string();
        let target_type = impl_block.target_type.to_string();
        // Note: protocol_name.res.hir_id() and target_type.res.hir_id() carry
        // the resolved node definitions, but JS output requires string names.
        let js_protocol_name = CodegenJS::protocol_js_name(&protocol_name);

        let mut stmts = Vec::new();

        // $Protocol.TargetType = {};
        let target =
            self.assignment_target_member(self.ident_expr(&js_protocol_name), &target_type);
        let assign = self.assign_expr(target, self.ast.expression_object(SPAN, self.ast.vec()));
        stmts.push(self.expr_stmt(assign));

        for method in &impl_block.methods {
            let method_name = match &method.name.kind {
                hir::ExprKind::Path(path) => path.last_ident().to_string(),
                hir::ExprKind::FieldAccess(_, ident) => ident.to_string(),
                _ => unreachable!(),
            };

            let func = self.generate_function_expression(method);
            let proto_type =
                self.static_member_expr(self.ident_expr(&js_protocol_name), &target_type);
            let target = self.assignment_target_member(proto_type, &method_name);
            let assign = self.assign_expr(target, func);
            stmts.push(self.expr_stmt(assign));
        }

        for apply_ident in &impl_block.apply_methods {
            let method_name = apply_ident.as_str();
            assert!(
                !is_builtin_type(&target_type),
                "Cannot use 'apply' for built-in type '{target_type}': \
                 applying methods to built-in types is not allowed to preserve backwards compatibility"
            );
            let proto = js_prototype_for_type(&target_type);

            // $installMethod(TargetType.prototype, "methodName", $Protocol.methodName);
            let install_call = self.call_expr(
                self.ident_expr("$installMethod"),
                vec![
                    Argument::from(self.ident_expr(&proto)),
                    Argument::from(self.str_expr(method_name)),
                    Argument::from(
                        self.static_member_expr(self.ident_expr(&js_protocol_name), method_name),
                    ),
                ],
            );
            stmts.push(self.expr_stmt(install_call));
        }

        stmts
    }
}

fn is_builtin_type(type_name: &str) -> bool {
    matches!(
        type_name,
        "List" | "Option" | "Result" | "ListIterator" | "string::StringBuf"
    )
}

fn js_prototype_for_type(type_name: &str) -> String {
    format!("{type_name}.prototype")
}
