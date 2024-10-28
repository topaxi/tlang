use tlang_hir::hir;

use crate::generator::{BlockContext, CodegenJS};

impl CodegenJS {
    fn generate_function_arguments(
        &mut self,
        declarations: &[hir::FunctionDeclaration],
    ) -> (Option<String>, Vec<String>) {
        // TODO: What was my plan here?
        //let is_member_method = is_member_method(&declarations[0].name);
        let first_declaration_number_of_args = declarations[0].parameters.len();
        // check whether all declarations have the same number of parameters
        let same_number_of_args = declarations
            .iter()
            .all(|d| d.parameters.len() == first_declaration_number_of_args);

        if same_number_of_args {
            let mut bindings = Vec::with_capacity(first_declaration_number_of_args);

            for i in 0..declarations[0].parameters.len() {
                // If the name of this parameter is the same in all declarations, we can reuse the
                // actual defined name. Otherwise we use `arg{i}` as the name.
                let arg_name = declarations.iter().find_map(|d| {
                    let param = &d.parameters[i];
                    match &param.pattern.kind {
                        hir::PatKind::Identifier(_id, ident_pattern) => {
                            Some(ident_pattern.name.to_string())
                        }
                        hir::PatKind::Enum(identifier, _) => {
                            Some(get_enum_name(&identifier).to_lowercase())
                        }
                        _ => None,
                    }
                });

                let arg_name = if arg_name.is_some()
                    && declarations
                        .iter()
                        .all(|d| match &d.parameters[i].pattern.kind {
                            hir::PatKind::Identifier(_, ident_pattern) => {
                                Some(ident_pattern.name.to_string()) == arg_name
                            }
                            hir::PatKind::Enum(path, _) => {
                                Some(get_enum_name(&path).to_lowercase()) == arg_name
                            }
                            _ => true,
                        }) {
                    #[allow(clippy::unnecessary_unwrap)]
                    arg_name.unwrap()
                } else {
                    self.current_scope().declare_variable(&format!("arg{i}"))
                };

                if i > 0 {
                    self.push_str(", ");
                }
                // TODO: if member method and self, push `this`.
                self.push_str(&arg_name);
                bindings.push(arg_name);
            }

            (None, bindings)
        } else {
            let args_binding = self.current_scope().declare_variable("args");
            self.push_str("...");
            self.push_str(&args_binding);

            let max_args = declarations
                .iter()
                .map(|d| d.parameters.len())
                .max()
                .unwrap();

            let mut bindings = Vec::with_capacity(max_args);

            for i in 0..max_args {
                // TODO: if member method and self, push `this`.
                bindings.push(format!("args[{i}]"));
            }

            (Some(args_binding), bindings)
        }
    }

    fn generate_function_parameter_list(&mut self, parameters: &[hir::FunctionParameter]) {
        self.push_char('(');

        let mut iter = parameters.iter();

        if let Some(param) = iter.next() {
            self.generate_pat(&param.pattern);

            // If the first param was the self param, we didn't render anything and we need to skip
            // the comma being rendered in the loop ahead.
            if param.pattern.is_self() {
                if let Some(param) = iter.next() {
                    self.generate_pat(&param.pattern);
                }
            }
        }

        for param in iter {
            self.push_str(", ");
            self.generate_pat(&param.pattern);
        }

        self.push_char(')');
    }

    fn generate_struct_method_binding(&mut self, declaration: &hir::FunctionDeclaration) {
        if is_static_method(&declaration.name) {
            let lhs = match &declaration.name.kind {
                hir::ExprKind::Path(path) => path.join("."),
                _ => unreachable!(),
            };

            self.push_indent();
            self.push_str(&format!("{} = ", lhs));
        } else if is_member_method(&declaration.name) {
            let target_name: String = match &declaration.name.kind {
                hir::ExprKind::FieldAccess(base, ..) => fn_identifier_to_string(base),
                _ => unreachable!(),
            };

            let field_name = match &declaration.name.kind {
                hir::ExprKind::FieldAccess(_, field) => field.to_string(),
                _ => unreachable!(),
            };

            self.push_indent();
            self.push_str(&target_name);
            self.push_str("Constructor.prototype.");
            self.push_str(&field_name);
            self.push_str(" = ");
        } else {
            self.push_indent();
        }
    }

    pub(crate) fn generate_function_declaration(&mut self, declaration: &hir::FunctionDeclaration) {
        let name_as_str = fn_identifier_to_string(&declaration.name);
        let is_tail_recursive =
            is_function_body_tail_recursive_block(&name_as_str, &declaration.body);
        self.push_function_context(
            &name_as_str,
            &declaration.parameters,
            &[],
            is_tail_recursive,
        );

        self.generate_struct_method_binding(declaration);
        self.push_str("function ");
        self.push_str(&name_as_str);
        self.push_scope();
        self.generate_function_parameter_list(&declaration.parameters);
        self.push_str(" {\n");
        self.flush_statement_buffer();
        self.inc_indent();
        self.generate_function_body(&declaration.body, is_tail_recursive);
        self.dec_indent();
        self.push_indent();
        self.push_str("}\n");
        self.pop_scope();
        self.pop_function_context();
    }

    pub(crate) fn generate_function_expression(
        self: &mut CodegenJS,
        declaration: &hir::FunctionDeclaration,
    ) {
        self.push_scope();

        let name_as_str = fn_identifier_to_string(&declaration.name);
        let is_tail_recursive =
            is_function_body_tail_recursive_block(&name_as_str, &declaration.body);

        self.push_function_context(
            &name_as_str,
            &declaration.parameters,
            &[],
            is_tail_recursive,
        );

        self.push_str("function");

        match name_as_str.as_str() {
            "anonymous" => {}
            _ => {
                self.push_char(' ');
                self.push_str(&name_as_str);
            }
        };

        self.generate_function_parameter_list(&declaration.parameters);
        self.push_str(" {\n");
        self.inc_indent();
        self.flush_statement_buffer();
        self.generate_function_body(&declaration.body, is_tail_recursive);
        self.dec_indent();
        self.push_indent();
        self.push_char('}');
        self.pop_function_context();
        self.pop_scope();
    }

    fn generate_function_parameter(&mut self, pattern: &hir::Pat) {
        match &pattern.kind {
            // Do not run generate_identifier, as that would resolve the parameter as a variable.
            hir::PatKind::Identifier(ident_pattern) => {
                self.current_scope()
                    .declare_variable(ident_pattern.name.as_str());
                self.push_str(ident_pattern.name.as_str());
            }
            hir::PatKind::Literal(_) => self.generate_pat(pattern),
            hir::PatKind::List(_) => todo!(),
            // Wildcards are handled within pipeline and call expressions,
            hir::PatKind::Wildcard => {
                unreachable!("Unexpected wildcard in function parameter.")
            }
            _ => todo!(),
        }
    }

    fn generate_function_body(&mut self, body: &hir::Block, is_tail_recursive: bool) {
        self.flush_function_pre_body();
        if is_tail_recursive {
            self.push_indent();
            self.push_str("rec:while (true) {\n");
            self.inc_indent();
            self.generate_function_body_block(body);
            self.dec_indent();
            self.push_indent();
            self.push_str("}\n");
        } else {
            self.generate_function_body_block(body);
        }
    }

    fn generate_function_body_block(&mut self, block: &hir::Block) {
        self.generate_statements(&block.stmts);

        if !block.has_completion() {
            return;
        }

        // We only render the return statement if we are not in a tail recursive function body
        // and the node is RecursiveCall pointing to the current function.
        if let Some(function_context) = self.get_function_context() {
            if function_context.is_tail_recursive && block.has_completion() {
                if let Some(ref expression) = block.expr {
                    if let hir::ExprKind::TailCall(..) = expression.kind {
                        self.generate_expr(expression, None);
                        return;
                    }
                }
            }
        }

        self.push_indent();
        self.push_str("return ");
        self.push_context(BlockContext::Expression);
        self.generate_optional_expr(&block.expr, None);
        self.pop_context();
        self.push_char(';');
        self.push_newline();
        self.flush_statement_buffer();
    }

    pub(crate) fn generate_return_statement(self: &mut CodegenJS, expr: &Option<hir::Expr>) {
        // We do not render a return statement if we are in a tail recursive function body.
        // Which calls the current function recursively.
        if expr.is_some() {
            if let hir::ExprKind::TailCall(callee, ..) = &expr.as_ref().unwrap().kind {
                let call_identifier = if let hir::ExprKind::Path(_) = callee.kind {
                    Some(fn_identifier_to_string(&callee))
                } else {
                    None
                };

                if call_identifier.is_some() {
                    if let Some(function_context) = self.get_function_context() {
                        if function_context.is_tail_recursive
                            && function_context.name == call_identifier.unwrap()
                        {
                            return self.generate_optional_expr(expr, None);
                        }
                    }
                }
            }
        }

        self.push_indent();
        self.push_str("return");

        if let Some(expr) = expr {
            self.push_char(' ');
            self.generate_expr(expr, None);
        }

        self.push_str(";\n");
    }

    pub(crate) fn generate_recursive_call_expression(&mut self, expr: &hir::CallExpression) {
        // If call expression is referencing the current function, all we do is update the arguments,
        // as we are in a while loop.
        if let hir::ExprKind::Path(_) = &expr.callee.kind {
            if let Some(function_context) = self.get_function_context() {
                if function_context.is_tail_recursive
                        // TODO: Comparing identifier by string might not be the best idea.
                        && function_context.name == fn_identifier_to_string(&expr.callee)
                {
                    let params = function_context.parameter_bindings.clone();

                    let tmp_vars = params
                        .iter()
                        .map(|_| self.current_scope().declare_tmp_variable())
                        .collect::<Vec<_>>();

                    for (i, arg) in expr.arguments.iter().enumerate() {
                        self.push_let_declaration_to_expr(&tmp_vars[i], arg);
                        self.push_str(";\n");
                    }

                    for (i, arg_name) in params.iter().enumerate() {
                        self.push_indent();
                        self.push_str(arg_name);
                        self.push_str(" = ");
                        self.push_str(&tmp_vars[i]);
                        self.push_str(";\n");
                    }

                    self.push_indent();
                    self.push_str("continue rec;\n");

                    return;
                }
            }
        }

        // For any other referenced function, we do a normal call expression.
        self.generate_call_expression(expr)
    }

    fn flush_function_pre_body(self: &mut CodegenJS) {
        for (name, value) in &self.consume_function_pre_body_declarations() {
            self.push_let_declaration_to_identifier(name, value);
            self.current_scope().declare_variable_alias(name, name);
        }
        self.flush_statement_buffer();
    }
}

fn get_enum_name(path: &hir::Path) -> String {
    path.segments[path.segments.len() - 2].ident.to_string()
}

fn is_function_body_tail_recursive_stmt(function_name: &str, stmt: &hir::Stmt) -> bool {
    match &stmt.kind {
        hir::StmtKind::Expr(expr) => is_function_body_tail_recursive(function_name, expr),
        hir::StmtKind::Return(expr) => {
            if let Some(expr) = expr.as_ref() {
                is_function_body_tail_recursive(function_name, expr)
            } else {
                false
            }
        }
        _ => false,
    }
}

fn is_function_body_tail_recursive_block(function_name: &str, block: &hir::Block) -> bool {
    for statement in &block.stmts {
        if is_function_body_tail_recursive_stmt(function_name, statement) {
            return true;
        }
    }

    if let Some(ref expression) = block.expr {
        return is_function_body_tail_recursive(function_name, expression);
    }

    false
}

fn is_function_body_tail_recursive(function_name: &str, node: &hir::Expr) -> bool {
    // Recursively traverse nodes to check for tail recursive calls to the function itself.
    // We currently only support tail recursion to the function itself, not any other function.
    // Therefore we look for RecursiveCall nodes which reference the current function name.
    match &node.kind {
        hir::ExprKind::TailCall(call_expr) => {
            // If the function is an identifier, check if it's the same as the current function name.
            if let hir::ExprKind::Path(_) = &call_expr.callee.kind {
                if fn_identifier_to_string(&call_expr.callee) == function_name {
                    return true;
                }
            }
            false
        }
        hir::ExprKind::Block(block) => is_function_body_tail_recursive_block(function_name, block),
        hir::ExprKind::Match(expr) => {
            is_function_body_tail_recursive(function_name, &expr.expression)
        }
        hir::ExprKind::IfElse(expr) => {
            is_function_body_tail_recursive(function_name, &expr.condition)
                || is_function_body_tail_recursive(function_name, &expr.then_branch)
                || expr.else_branches.iter().any(|else_clause| {
                    is_function_body_tail_recursive(function_name, &else_clause.consequence)
                })
        }
        _ => false,
    }
}

fn is_member_method(node: &hir::Expr) -> bool {
    matches!(&node.kind, hir::ExprKind::FieldExpression(_))
}

fn is_static_method(node: &hir::Expr) -> bool {
    if let hir::ExprKind::Path(path) = &node.kind {
        return path.segments.len() > 1;
    }

    false
}

pub(crate) fn fn_identifier_to_string(expr: &hir::Expr) -> String {
    match &expr.kind {
        hir::ExprKind::Path(path) => path.join("__"),
        hir::ExprKind::FieldExpression(expr) => expr.field.to_string(),
        kind => todo!("fn_identifier_to_string: {:?}", kind),
    }
}
