use oxc_ast::NONE;
use oxc_ast::ast::*;
use oxc_span::SPAN;
use tlang_hir as hir;

use crate::generator::InnerCodegen;
use crate::js;

impl<'a> InnerCodegen<'a> {
    pub fn generate_function_declaration(
        &mut self,
        declaration: &hir::FunctionDeclaration,
    ) -> Vec<Statement<'a>> {
        let js_name = fn_identifier_to_string(&declaration.name);
        // Use the name pre-registered by `pre_register_declarations` if
        // available; otherwise register a fresh local binding.
        let name_as_str = self
            .name_map
            .resolve(declaration.hir_id)
            .map(|s| s.to_string())
            .unwrap_or_else(|| self.name_map.register_local(declaration.hir_id, &js_name));
        let is_tail_recursive =
            is_function_body_tail_recursive_block(&name_as_str, &declaration.body);
        self.push_function_context(
            &name_as_str,
            &declaration.parameters,
            &[],
            is_tail_recursive,
        );

        let is_method = matches!(declaration.name.kind, hir::ExprKind::FieldAccess(_, _));

        self.push_scope();
        let params = self.generate_function_parameter_list(&declaration.parameters, is_method);
        let body_stmts = self.generate_function_body(&declaration.body, is_tail_recursive);
        let body = self.fn_body(self.ast.vec_from_iter(body_stmts));
        self.pop_scope();
        self.pop_function_context();

        let formal_params = self.formal_params(self.ast.vec_from_iter(params));

        // Check if this is a method binding (e.g. Foo.bar or Foo.prototype.bar)
        let binding = self.get_method_binding(&declaration.name);

        if let Some((target, method_name)) = binding {
            // target.method = function name(...) { ... };
            let func = self.ast.expression_function(
                Self::hir_span(declaration.span),
                FunctionType::FunctionExpression,
                Some(self.binding_ident(&name_as_str)),
                false,
                false,
                false,
                NONE,
                NONE,
                formal_params,
                NONE,
                Some(body),
            );
            let assign_target = self.assignment_target_member(target, &method_name);
            let assign = self.assign_expr(assign_target, func);
            vec![self.expr_stmt(assign)]
        } else {
            // function name(...) { ... }
            let decl = Statement::FunctionDeclaration(self.ast.alloc_function(
                Self::hir_span(declaration.span),
                FunctionType::FunctionDeclaration,
                Some(self.binding_ident(&name_as_str)),
                false,
                false,
                false,
                NONE,
                NONE,
                formal_params,
                NONE,
                Some(body),
            ));
            vec![decl]
        }
    }

    pub fn generate_dyn_function_declaration(
        &mut self,
        declaration: &hir::DynFunctionDeclaration,
    ) -> Vec<Statement<'a>> {
        let is_method = matches!(declaration.name.kind, hir::ExprKind::FieldAccess(_, _));
        let js_name = fn_identifier_to_string(&declaration.name);
        // Use the name pre-registered by `pre_register_declarations` if
        // available; otherwise register a fresh local binding.
        let name_as_str = self
            .name_map
            .resolve(declaration.hir_id)
            .map(|s| s.to_string())
            .unwrap_or_else(|| self.name_map.register_local(declaration.hir_id, &js_name));

        // Build the dispatcher body: if/else chain on arguments.length
        let mut if_chain: Option<Statement<'a>> = None;

        for (variant_arg_len, _) in declaration.variants.iter().rev() {
            let adjusted_len = *variant_arg_len - is_method as usize;

            // if (arguments.length === N)
            let test = self.ast.expression_binary(
                SPAN,
                self.static_member_expr(self.ident_expr("arguments"), "length"),
                BinaryOperator::StrictEquality,
                self.num_expr(adjusted_len as f64),
            );

            // Build call: [this.]name$$N(arguments[0], arguments[1], ...)
            let callee_name = format!("{name_as_str}$${variant_arg_len}");
            let callee = if is_method {
                self.static_member_expr(self.this_expr(), &callee_name)
            } else {
                self.ident_expr(&callee_name)
            };

            let args: Vec<Argument<'a>> = (0..adjusted_len)
                .map(|i| {
                    Argument::from(self.computed_member_expr(
                        self.ident_expr("arguments"),
                        self.num_expr(i as f64),
                    ))
                })
                .collect();

            let call = self.call_expr(callee, args);
            let return_stmt = self.ast.statement_return(SPAN, Some(call));
            let consequent = self.block_stmt(vec![return_stmt]);

            if_chain = Some(self.ast.statement_if(SPAN, test, consequent, if_chain));
        }

        let body = self.fn_body(
            self.ast
                .vec_from_iter(if_chain.into_iter().collect::<Vec<_>>()),
        );
        let formal_params = self.formal_params(self.ast.vec());

        let binding = self.get_method_binding(&declaration.name);

        if let Some((target, method_name)) = binding {
            let func = self.ast.expression_function(
                SPAN,
                FunctionType::FunctionExpression,
                Some(self.binding_ident(&name_as_str)),
                false,
                false,
                false,
                NONE,
                NONE,
                formal_params,
                NONE,
                Some(body),
            );
            let assign_target = self.assignment_target_member(target, &method_name);
            let assign = self.assign_expr(assign_target, func);
            vec![self.expr_stmt(assign)]
        } else {
            let decl = Statement::FunctionDeclaration(self.ast.alloc_function(
                SPAN,
                FunctionType::FunctionDeclaration,
                Some(self.binding_ident(&name_as_str)),
                false,
                false,
                false,
                NONE,
                NONE,
                formal_params,
                NONE,
                Some(body),
            ));
            vec![decl]
        }
    }

    pub fn generate_function_expression(
        &mut self,
        declaration: &hir::FunctionDeclaration,
    ) -> Expression<'a> {
        self.generate_function_expression_inner(declaration, true)
    }

    pub fn generate_method_expression(
        &mut self,
        declaration: &hir::FunctionDeclaration,
    ) -> Expression<'a> {
        self.generate_function_expression_inner(declaration, false)
    }

    fn generate_function_expression_inner(
        &mut self,
        declaration: &hir::FunctionDeclaration,
        named: bool,
    ) -> Expression<'a> {
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

        let params = self.generate_function_parameter_list(&declaration.parameters, false);
        let formal_params = self.formal_params(self.ast.vec_from_iter(params));

        let result = if let (true, Some(expr)) = (generate_arrow, declaration.body.expr.as_ref()) {
            if expr_can_render_as_js_expr(expr) {
                // Arrow with expression body: () => expr
                let body_expr = self.generate_expr(expr);
                let body_stmt = self.expr_stmt(body_expr);
                let body = self.fn_body(self.ast.vec1(body_stmt));
                self.ast.expression_arrow_function(
                    Self::hir_span(declaration.span),
                    true,
                    false,
                    NONE,
                    formal_params,
                    NONE,
                    body,
                )
            } else {
                let body_stmts = self.generate_function_body(&declaration.body, false);
                let body = self.fn_body(self.ast.vec_from_iter(body_stmts));
                self.ast.expression_arrow_function(
                    Self::hir_span(declaration.span),
                    false,
                    false,
                    NONE,
                    formal_params,
                    NONE,
                    body,
                )
            }
        } else {
            let body_stmts = self.generate_function_body(&declaration.body, is_tail_recursive);
            let body = self.fn_body(self.ast.vec_from_iter(body_stmts));

            let id = if is_anonymous || !named {
                None
            } else {
                Some(self.binding_ident(&name_as_str))
            };

            self.ast.expression_function(
                Self::hir_span(declaration.span),
                FunctionType::FunctionExpression,
                id,
                false,
                false,
                false,
                NONE,
                NONE,
                formal_params,
                NONE,
                Some(body),
            )
        };

        self.pop_function_context();
        self.pop_scope();
        result
    }

    fn generate_function_parameter_list(
        &mut self,
        parameters: &[hir::FunctionParameter],
        is_method: bool,
    ) -> Vec<FormalParameter<'a>> {
        let mut result = Vec::new();
        let mut iter = parameters.iter();

        if let Some(param) = iter.next() {
            if is_method {
                // Self param: map directly to `this`.
                self.name_map.register_exact(param.hir_id, "this");
                // Skip adding to params list — `this` is implicit in JS methods
                if let Some(param) = iter.next()
                    && !param.name.is_wildcard()
                {
                    let var_name = self
                        .name_map
                        .register_local(param.hir_id, param.name.as_str());
                    result.push(self.formal_param(&var_name));
                }
            } else if !param.name.is_wildcard() {
                let var_name = self
                    .name_map
                    .register_local(param.hir_id, param.name.as_str());
                result.push(self.formal_param(&var_name));
            }
        }

        for param in iter {
            if !param.name.is_wildcard() {
                let var_name = self
                    .name_map
                    .register_local(param.hir_id, param.name.as_str());
                result.push(self.formal_param(&var_name));
            }
        }

        result
    }

    fn generate_function_body(
        &mut self,
        body: &hir::Block,
        is_tail_recursive: bool,
    ) -> Vec<Statement<'a>> {
        let inner_stmts = self.generate_function_body_block(body);

        if is_tail_recursive {
            // rec: while (true) { ... }
            let body_block = self.block_stmt(inner_stmts);
            let while_stmt = self
                .ast
                .statement_while(SPAN, self.bool_expr(true), body_block);
            let labeled = self.ast.statement_labeled(
                SPAN,
                self.ast.label_identifier(SPAN, "rec"),
                while_stmt,
            );
            vec![labeled]
        } else {
            inner_stmts
        }
    }

    fn generate_function_body_block(&mut self, block: &hir::Block) -> Vec<Statement<'a>> {
        let mut stmts = self.generate_stmts(&block.stmts);

        if block.has_completion() {
            let return_stmts = self.generate_return_stmt(block.expr.as_ref());
            stmts.extend(return_stmts);
        }

        stmts
    }

    pub fn generate_return_stmt(&mut self, expr: Option<&hir::Expr>) -> Vec<Statement<'a>> {
        if let Some(e) = expr {
            // Self-referencing tail call — emit parameter reassignment + continue
            if let hir::ExprKind::TailCall(call_expr) = &e.kind
                && self.is_self_referencing_tail_call(e)
            {
                return self.generate_recursive_call_stmts(call_expr);
            }

            // IfElse containing tail calls in branches — emit if-else statement form
            if let hir::ExprKind::IfElse(cond, then_branch, else_branches) = &e.kind
                && self.if_else_has_self_referencing_tail_call(then_branch, else_branches)
            {
                return self.generate_if_else_return_stmts(cond, then_branch, else_branches);
            }
        }

        let argument = expr.map(|e| self.generate_expr(e));
        vec![self.ast.statement_return(SPAN, argument)]
    }

    /// Returns `true` if any branch of an if-else contains a self-referencing tail call
    /// at completion position (possibly nested in further if-else expressions).
    fn if_else_has_self_referencing_tail_call(
        &self,
        then_branch: &hir::Block,
        else_branches: &[hir::ElseClause],
    ) -> bool {
        self.block_completion_has_self_referencing_tail_call(then_branch)
            || else_branches
                .iter()
                .any(|eb| self.block_completion_has_self_referencing_tail_call(&eb.consequence))
    }

    fn block_completion_has_self_referencing_tail_call(&self, block: &hir::Block) -> bool {
        if let Some(expr) = &block.expr {
            self.expr_has_self_referencing_tail_call(expr)
        } else {
            false
        }
    }

    fn expr_has_self_referencing_tail_call(&self, expr: &hir::Expr) -> bool {
        match &expr.kind {
            hir::ExprKind::TailCall(_) => self.is_self_referencing_tail_call(expr),
            hir::ExprKind::IfElse(_, then_branch, else_branches) => {
                self.if_else_has_self_referencing_tail_call(then_branch, else_branches)
            }
            _ => false,
        }
    }

    /// Generate an if-else chain as statements where each branch completion is emitted
    /// via `generate_return_stmt` (enabling TCO inside branches).
    fn generate_if_else_return_stmts(
        &mut self,
        cond: &hir::Expr,
        then_branch: &hir::Block,
        else_branches: &[hir::ElseClause],
    ) -> Vec<Statement<'a>> {
        let test = self.generate_expr(cond);

        self.push_scope();
        let mut then_stmts = self.generate_stmts(&then_branch.stmts);
        let then_return = self.generate_return_stmt(then_branch.expr.as_ref());
        then_stmts.extend(then_return);
        self.pop_scope();
        let consequent = self.block_stmt(then_stmts);

        let alternate = if else_branches.is_empty() {
            None
        } else {
            Some(self.generate_else_return_chain(else_branches))
        };

        vec![self.ast.statement_if(SPAN, test, consequent, alternate)]
    }

    fn generate_else_return_chain(&mut self, else_branches: &[hir::ElseClause]) -> Statement<'a> {
        if else_branches.is_empty() {
            return self.block_stmt(vec![]);
        }

        let branch = &else_branches[0];

        self.push_scope();
        let mut body_stmts = self.generate_stmts(&branch.consequence.stmts);
        let return_stmts = self.generate_return_stmt(branch.consequence.expr.as_ref());
        body_stmts.extend(return_stmts);
        self.pop_scope();
        let body = self.block_stmt(body_stmts);

        if let Some(ref condition) = branch.condition {
            let test = self.generate_expr(condition);
            let alternate = if else_branches.len() > 1 {
                Some(self.generate_else_return_chain(&else_branches[1..]))
            } else {
                None
            };
            self.ast.statement_if(SPAN, test, body, alternate)
        } else {
            body
        }
    }

    pub fn is_self_referencing_tail_call(&self, expr: &hir::Expr) -> bool {
        if let hir::ExprKind::TailCall(call_expr) = &expr.kind {
            self.get_self_referencing_tail_call_context(call_expr)
                .is_some()
        } else {
            false
        }
    }

    fn get_self_referencing_tail_call_context(
        &self,
        expr: &hir::CallExpression,
    ) -> Option<&crate::generator::FunctionContext> {
        if let hir::ExprKind::Path(_) = &expr.callee.kind {
            self.get_function_context().filter(|ctx| {
                ctx.is_tail_recursive && ctx.name == fn_identifier_to_string(&expr.callee)
            })
        } else {
            None
        }
    }

    pub fn generate_recursive_call_stmts(
        &mut self,
        call_expr: &hir::CallExpression,
    ) -> Vec<Statement<'a>> {
        if let Some(ctx) = self.get_self_referencing_tail_call_context(call_expr) {
            let params = ctx.parameter_bindings.clone();

            // Assign args to tmp vars first
            let tmp_vars: Vec<String> = params.iter().map(|_| self.name_map.alloc_tmp()).collect();

            let mut stmts = Vec::new();

            for (i, arg) in call_expr.arguments.iter().enumerate() {
                let value = self.generate_expr(arg);
                stmts.push(self.let_decl(&tmp_vars[i], Some(value)));
            }

            // Then assign tmp vars to params
            for (i, param_name) in params.iter().enumerate() {
                let target = self.assignment_target_ident(param_name);
                let value = self.ident_expr(&tmp_vars[i]);
                stmts.push(self.expr_stmt(self.assign_expr(target, value)));
            }

            // continue rec;
            stmts.push(
                self.ast
                    .statement_continue(SPAN, Some(self.ast.label_identifier(SPAN, "rec"))),
            );

            return stmts;
        }

        // Mutual tail call → regular call
        let call = self.generate_call_expression(call_expr);
        vec![self.expr_stmt(call)]
    }

    fn get_method_binding(&mut self, name: &hir::Expr) -> Option<(Expression<'a>, String)> {
        match &name.kind {
            hir::ExprKind::Path(path) if path.segments.len() > 1 => {
                let parts: Vec<String> = path
                    .segments
                    .iter()
                    .map(|s| js::safe_js_variable_name(s.ident.as_str()))
                    .collect();
                let method_name = parts.last().unwrap().clone();
                let mut obj = self.ident_expr(&parts[0]);
                for part in &parts[1..parts.len() - 1] {
                    obj = self.static_member_expr(obj, part);
                }
                Some((obj, method_name))
            }
            hir::ExprKind::FieldAccess(base, field) => {
                let base_name = fn_identifier_to_string(base);
                let obj = self.static_member_expr(self.ident_expr(&base_name), "prototype");
                Some((obj, js::safe_js_variable_name(field.as_str())))
            }
            _ => None,
        }
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
    match &node.kind {
        hir::ExprKind::TailCall(call_expr) => {
            if let hir::ExprKind::Path(_) = &call_expr.callee.kind
                && fn_identifier_to_string(&call_expr.callee) == function_name
            {
                return true;
            }
            false
        }
        hir::ExprKind::Block(block) => is_function_body_tail_recursive_block(function_name, block),
        hir::ExprKind::Match(_, arms, ..) => arms
            .iter()
            .any(|arm| is_function_body_tail_recursive_block(function_name, &arm.block)),
        hir::ExprKind::IfElse(expr, then_branch, else_branches) => {
            is_function_body_tail_recursive(function_name, expr)
                || if let Some(expr) = &then_branch.expr {
                    is_function_body_tail_recursive(function_name, expr)
                } else if let Some(stmt) = then_branch.stmts.last() {
                    is_function_body_tail_recursive_stmt(function_name, stmt)
                } else {
                    false
                }
                || else_branches.iter().any(|else_clause| {
                    if let Some(expr) = &else_clause.consequence.expr {
                        is_function_body_tail_recursive(function_name, expr)
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

pub(crate) fn fn_identifier_to_string(expr: &hir::Expr) -> String {
    match &expr.kind {
        hir::ExprKind::Path(path) => js::safe_js_variable_name(&path.join("__")),
        hir::ExprKind::FieldAccess(_base, field) => js::safe_js_variable_name(&field.to_string()),
        _ => unreachable!(),
    }
}

fn expr_can_render_as_js_expr(expr: &hir::Expr) -> bool {
    match &expr.kind {
        hir::ExprKind::Path(..)
        | hir::ExprKind::Literal(..)
        | hir::ExprKind::FunctionExpression(..)
        | hir::ExprKind::Range(..)
        | hir::ExprKind::Wildcard => true,
        hir::ExprKind::Binary(_, lhs, rhs) => {
            expr_can_render_as_js_expr(lhs) && expr_can_render_as_js_expr(rhs)
        }
        hir::ExprKind::Unary(_, inner) => expr_can_render_as_js_expr(inner),
        hir::ExprKind::Call(call) => call.arguments.iter().all(expr_can_render_as_js_expr),
        hir::ExprKind::Cast(inner, _) => expr_can_render_as_js_expr(inner),
        hir::ExprKind::FieldAccess(base, _) => expr_can_render_as_js_expr(base),
        hir::ExprKind::IndexAccess(base, index) => {
            expr_can_render_as_js_expr(base) && expr_can_render_as_js_expr(index)
        }
        hir::ExprKind::List(items) => items.iter().all(expr_can_render_as_js_expr),
        hir::ExprKind::Dict(kvs) => kvs
            .iter()
            .all(|(k, v)| expr_can_render_as_js_expr(k) && expr_can_render_as_js_expr(v)),
        _ => false,
    }
}
