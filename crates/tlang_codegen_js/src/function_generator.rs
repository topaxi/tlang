use tlang_ast::token::kw;
use tlang_hir::hir;

use crate::expr_generator::expr_can_render_as_js_expr;
use crate::generator::{BlockContext, CodegenJS};

impl CodegenJS {
    fn generate_function_param(&mut self, param: &hir::FunctionParameter) {
        if param.name.is_self() {
            self.current_scope()
                .declare_variable_alias(kw::_Self, "this");
        } else if param.name.is_wildcard() {
            // nothing to do
        } else {
            let var_name = self
                .current_scope()
                .declare_local_variable(param.name.as_str());
            self.push_str(&var_name);
            self.current_scope()
                .declare_variable_alias(param.name.as_str(), &var_name);
        }
    }

    fn generate_function_parameter_list(&mut self, parameters: &[hir::FunctionParameter]) {
        self.push_char('(');

        let mut iter = parameters.iter();

        if let Some(param) = iter.next() {
            self.generate_function_param(param);

            // If the first param was the self param, we didn't render anything and we need to skip
            // the comma being rendered in the loop ahead.
            if param.name.is_self() {
                if let Some(param) = iter.next() {
                    self.generate_function_param(param)
                }
            }
        }

        for param in iter {
            self.push_str(", ");
            self.push_str(param.name.as_str());
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
        let name_as_str = self
            .current_scope()
            .declare_local_variable(&fn_identifier_to_string(&declaration.name));
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
        let is_anonymous = name_as_str == "anonymous";
        let is_tail_recursive =
            !is_anonymous && is_function_body_tail_recursive_block(&name_as_str, &declaration.body);
        let generate_arrow = is_anonymous && declaration.body.stmts.is_empty();

        self.push_function_context(
            &name_as_str,
            &declaration.parameters,
            &[],
            is_tail_recursive,
        );

        if generate_arrow {
            self.generate_function_parameter_list(&declaration.parameters);
            self.push_str(" =>");
        } else {
            self.push_str("function");

            if !is_anonymous {
                self.push_char(' ');
                self.push_str(&name_as_str);
            }

            self.generate_function_parameter_list(&declaration.parameters);
        }

        if generate_arrow
            && declaration.body.expr.is_some()
            && expr_can_render_as_js_expr(declaration.body.expr.as_ref().unwrap())
        {
            self.push_char(' ');
            self.generate_expr(declaration.body.expr.as_ref().unwrap(), None);
        } else {
            self.push_str(" {\n");
            self.inc_indent();
            self.flush_statement_buffer();
            self.generate_function_body(&declaration.body, is_tail_recursive);
            self.dec_indent();
            self.push_indent();
            self.push_char('}');
        }

        self.pop_function_context();
        self.pop_scope();
    }

    fn generate_function_body(&mut self, body: &hir::Block, is_tail_recursive: bool) {
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
        self.flush_statement_buffer();
        self.generate_statements(&block.stmts);

        if block.has_completion() {
            self.generate_return_statement(block.expr.as_ref());
            self.flush_statement_buffer();
        }
    }

    pub(crate) fn generate_return_statement(self: &mut CodegenJS, expr: Option<&hir::Expr>) {
        // We do not render a return statement if we are in a tail recursive function body.
        // Which calls the current function recursively.
        if let Some(hir::ExprKind::TailCall(call_expr)) = expr.map(|e| &e.kind) {
            let call_identifier = if let hir::ExprKind::Path(_) = &call_expr.callee.kind {
                Some(fn_identifier_to_string(&call_expr.callee))
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

        self.push_indent();
        self.push_context(BlockContext::Expression);
        self.push_str("return");

        if let Some(expr) = expr {
            self.push_char(' ');
            self.push_completion_variable(Some("return"));
            self.generate_expr(expr, None);
            self.pop_completion_variable();
        }

        if self.needs_semicolon(expr) {
            self.push_char(';');
        }
        self.push_newline();
        self.pop_context();
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
        hir::ExprKind::Match(_, arms, ..) => arms
            .iter()
            .any(|arm| is_function_body_tail_recursive(function_name, &arm.expr)),
        hir::ExprKind::IfElse(expr, then_branch, else_branches) => {
            is_function_body_tail_recursive(function_name, expr)
                || if then_branch.expr.is_some() {
                    is_function_body_tail_recursive(
                        function_name,
                        then_branch.expr.as_ref().unwrap(),
                    )
                } else if let Some(stmt) = then_branch.stmts.last() {
                    is_function_body_tail_recursive_stmt(function_name, stmt)
                } else {
                    false
                }
                || else_branches.iter().any(|else_clause| {
                    if else_clause.consequence.expr.is_some() {
                        is_function_body_tail_recursive(
                            function_name,
                            else_clause.consequence.expr.as_ref().unwrap(),
                        )
                    } else if let Some(stmt) = else_clause.consequence.stmts.last() {
                        is_function_body_tail_recursive_stmt(function_name, stmt)
                    } else {
                        false
                    }
                })
        }
        _ => false,
    }
}

fn is_member_method(node: &hir::Expr) -> bool {
    matches!(&node.kind, hir::ExprKind::FieldAccess(..))
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
        hir::ExprKind::FieldAccess(_base, field) => field.to_string(),
        kind => todo!("fn_identifier_to_string: {:?}", kind),
    }
}
