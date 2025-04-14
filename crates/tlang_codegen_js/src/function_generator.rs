use tlang_hir::hir;

use crate::expr_generator::{expr_can_render_as_js_expr, expr_contains_return};
use crate::generator::{BlockContext, CodegenJS, FunctionContext};

impl CodegenJS {
    fn generate_function_param(&mut self, param: &hir::FunctionParameter, is_self: bool) {
        if is_self {
            let var_name = self
                .current_scope()
                .declare_local_variable(param.name.as_str());
            self.current_scope()
                .declare_variable_alias(&var_name, "this");
        } else if param.name.is_wildcard() {
            // nothing to do
        } else {
            let var_name = self
                .current_scope()
                .declare_local_variable(param.name.as_str());
            self.push_str(&var_name);
        }
    }

    fn generate_function_parameter_list(
        &mut self,
        parameters: &[hir::FunctionParameter],
        is_method: bool,
    ) {
        self.push_char('(');

        let mut iter = parameters.iter();

        if let Some(param) = iter.next() {
            self.generate_function_param(param, is_method);

            // If the first param was the self param, we didn't render anything and we need to skip
            // the comma being rendered in the loop ahead.
            if is_method {
                if let Some(param) = iter.next() {
                    self.generate_function_param(param, false)
                }
            }
        }

        for param in iter {
            self.push_str(", ");
            self.push_str(param.name.as_str());
        }

        self.push_char(')');
    }

    fn generate_struct_method_binding(&mut self, name: &hir::Expr) {
        match &name.kind {
            hir::ExprKind::Path(path) if path.segments.len() > 1 => {
                self.push_indent();
                self.push_str(&path.join("."));
                self.push_str(" = ");
            }
            hir::ExprKind::FieldAccess(base, field) => {
                self.push_indent();
                self.push_str(&fn_identifier_to_string(base));
                self.push_str(".prototype.");
                self.push_str(field.as_str());
                self.push_str(" = ");
            }
            _ => self.push_indent(),
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

        let is_method = matches!(declaration.name.kind, hir::ExprKind::FieldAccess(_, _));

        self.generate_struct_method_binding(&declaration.name);
        self.push_str("function ");
        self.push_str(&name_as_str);
        self.push_scope();
        self.generate_function_parameter_list(&declaration.parameters, is_method);
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

    pub(crate) fn generate_dyn_function_declaration(
        &mut self,
        declaration: &hir::DynFunctionDeclaration,
    ) {
        let is_method = matches!(declaration.name.kind, hir::ExprKind::FieldAccess(_, _));
        let name_as_str = self
            .current_scope()
            .declare_local_variable(&fn_identifier_to_string(&declaration.name));

        self.generate_struct_method_binding(&declaration.name);
        self.push_str("function ");
        self.push_str(&name_as_str);
        self.push_str("() {\n");
        self.inc_indent();

        for (i, (variant_arg_len, _)) in declaration.variants.iter().enumerate() {
            if i > 0 {
                self.push_str(" else ");
            } else {
                self.push_indent();
            }

            self.push_str("if (arguments.length === ");
            self.push_str(&(variant_arg_len - is_method as usize).to_string());
            self.push_str(") {\n");
            self.inc_indent();
            self.push_indent();
            self.push_str("return ");

            if is_method {
                self.push_str("this.");
            }

            self.push_str(&name_as_str);
            self.push_str("$$");
            self.push_str(&variant_arg_len.to_string());

            self.push_char('(');
            for i in 0..(variant_arg_len - is_method as usize) {
                if i > 0 {
                    self.push_str(", ");
                }

                self.push_str("arguments[");
                self.push_str(&i.to_string());
                self.push_char(']');
            }
            self.push_str(");\n");

            self.dec_indent();
            self.push_indent();

            self.push_char('}');
        }

        self.push_newline();
        self.dec_indent();
        self.push_indent();
        self.push_str("}\n");
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
            self.generate_function_parameter_list(&declaration.parameters, false);
            self.push_str(" =>");
        } else {
            self.push_str("function");

            if !is_anonymous {
                self.push_char(' ');
                self.push_str(&name_as_str);
            }

            self.generate_function_parameter_list(&declaration.parameters, false);
        }

        if generate_arrow
            && declaration.body.expr.is_some()
            && expr_can_render_as_js_expr(declaration.body.expr.as_ref().unwrap())
        {
            self.push_char(' ');
            self.generate_expr(declaration.body.expr.as_ref().unwrap(), None, BlockContext::Expression);
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
            let final_expr = block.expr.as_ref().unwrap(); // block.has_completion() ensures this unwrap is safe.

            // If the final expression actually CONTAINS a return, generate it as a statement.
            // Otherwise, use a standard return statement.
            if expr_contains_return(final_expr) {
                 self.generate_expr_stmt(final_expr, None); // Generate as statement
                 self.flush_statement_buffer();
            } else {
                 // Generate `return <expr>;`
                 self.generate_return_statement(Some(final_expr));
                 self.flush_statement_buffer();
            }
        }
    }

    pub(crate) fn generate_return_statement(self: &mut CodegenJS, expr_opt: Option<&hir::Expr>) {
        // Check if this is a tail recursive call.
        if expr_opt.map_or(false, |e| self.is_self_referencing_tail_call(e)) {
            let call_expr = match &expr_opt.unwrap().kind {
                hir::ExprKind::TailCall(call) => call,
                _ => unreachable!("Expected TailCall expression"),
            };

            // Tail recursive call, generate assignments and continue.
            self.generate_recursive_call_expression(call_expr);
            self.push_indent();
            self.push_str("continue rec;\n");
        } else {
            // Regular return statement.
            if let Some(expr) = expr_opt {
                 // Check if the expression to return is complex and needs statement generation.
                 let needs_stmt_gen = matches!(expr.kind, hir::ExprKind::Block(_) | hir::ExprKind::IfElse(_,_,_) | hir::ExprKind::Match(_,_));

                 if needs_stmt_gen && !expr_contains_return(expr) {
                     // Generate complex expr directly as statements if it doesn't contain return.
                     self.generate_expr_stmt(expr, None);
                 } else {
                    // Otherwise, generate `return <expr>;`
                    self.push_indent();
                    self.push_str("return");
                    // Add space only if there is an expression to return
                    if expr_opt.is_some() { 
                        self.push_char(' ');
                    }
                    self.generate_expr(expr, None, BlockContext::Expression);
                    self.push_str(";\n");
                 }
            } else {
                // Generate `return;`
                self.push_indent();
                self.push_str("return;\n");
            }
        }
    }

    fn get_self_referencing_tail_call_function_context(
        &self,
        expr: &hir::CallExpression,
    ) -> Option<&FunctionContext> {
        if let hir::ExprKind::Path(_) = &expr.callee.kind {
            self.get_function_context().filter(|&function_context| {
                function_context.is_tail_recursive
                    && function_context.name == fn_identifier_to_string(&expr.callee)
            })
        } else {
            None
        }
    }

    pub(crate) fn is_self_referencing_tail_call(&self, expr: &hir::Expr) -> bool {
        if let hir::ExprKind::TailCall(call_expr) = &expr.kind {
            self.get_self_referencing_tail_call_function_context(call_expr)
                .is_some()
        } else {
            false
        }
    }

    pub(crate) fn generate_recursive_call_expression(&mut self, expr: &hir::CallExpression) {
        // If call expression is referencing the current function, all we do is update the arguments,
        // as we are in a while loop.
        if let Some(function_context) = self.get_self_referencing_tail_call_function_context(expr) {
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
            .any(|arm| is_function_body_tail_recursive_block(function_name, &arm.block)),
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

/// Helper function to check if an expression needs statement transformation.
fn needs_statement_transform(expr: &hir::Expr) -> bool {
    match &expr.kind {
        hir::ExprKind::Block(_) | hir::ExprKind::IfElse(_,_,_) | hir::ExprKind::Match(_,_) => {
            expr_contains_return(expr)
        }
        _ => false,
    }
}

pub(crate) fn fn_identifier_to_string(expr: &hir::Expr) -> String {
    match &expr.kind {
        hir::ExprKind::Path(path) => path.join("__"),
        hir::ExprKind::FieldAccess(_base, field) => field.to_string(),
        _ => unreachable!(),
    }
}
