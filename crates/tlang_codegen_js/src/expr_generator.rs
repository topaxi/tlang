use tlang_ast::node::{self as ast, Ident};
use tlang_ast::token::Literal;
use tlang_hir::hir;

use crate::binary_operator_generator::{
    JSAssociativity, JSOperatorInfo, map_operator_info, should_wrap_with_parentheses,
};
use crate::generator::{BlockContext, CodegenJS};
use crate::hir_js_pass::if_else_can_render_as_ternary;
use crate::js;

impl CodegenJS {
    /// Generates blocks in expression position.
    pub(crate) fn generate_block_expression(&mut self, block: &hir::Block) {
        let has_completion_var = self.current_completion_variable().is_some();
        let completion_tmp_var = if block.has_completion() {
            self.current_completion_variable()
                .map(str::to_string)
                .unwrap_or_else(|| self.current_scope().declare_tmp_variable())
        } else {
            String::new()
        };

        self.push_scope();

        // In a case of `let a = { 1 }`, we want to render the expression as a statement.
        // At this state, we should have `let a = ` in the statement buffer.
        // We do this by temporarily swapping the statement buffer, generating the
        // expression as an statement, and then swapping the statement buffer back.
        let lhs = if has_completion_var {
            String::new()
        } else {
            self.replace_statement_buffer_with_empty_string()
        };

        if block.has_completion() {
            if has_completion_var {
                // Halp I made a mess
            } else {
                self.push_indent();
                self.push_let_declaration(&completion_tmp_var);
                self.push_str("{\n");
                self.inc_indent();
            }
        }

        self.push_completion_variable(None);
        self.generate_statements(&block.stmts);
        self.pop_completion_variable();

        if !block.has_completion() {
            self.push_statement_buffer();
            self.push_str(&lhs);
            self.flush_statement_buffer();
            self.pop_statement_buffer();
            self.flush_statement_buffer();
            self.pop_scope();
            return;
        }

        let completion = block.expr.as_ref().unwrap();
        if completion_tmp_var == "return" {
            if self.is_self_referencing_tail_call(completion) {
                self.generate_expr(completion, None);
            } else {
                self.push_indent();
                self.push_str("return ");
                self.generate_expr(completion, None);
                self.push_char(';');
                self.push_newline();
            }
        } else {
            self.push_indent();
            self.push_str(&completion_tmp_var);
            self.push_str(" = ");
            self.generate_expr(completion, None);
            self.push_char(';');
            self.push_newline();
        }
        if !has_completion_var {
            self.dec_indent();
            self.push_indent();
            // Don't add }; in expression context - let the caller handle statement termination
            if self.current_context() == BlockContext::Statement {
                self.push_str("};\n");
            } else {
                self.push_char('}');
                self.push_newline();
            }
            self.flush_statement_buffer();
            self.push_str(&lhs);
            self.push_str(completion_tmp_var.as_str());
        }
        self.flush_statement_buffer();
        self.pop_scope();
    }

    pub(crate) fn generate_literal(&mut self, literal: &Literal) {
        match literal {
            Literal::Integer(value) => {
                self.push_str(&value.to_string());
            }
            Literal::UnsignedInteger(value) => {
                self.push_str(&value.to_string());
            }
            Literal::Float(value) => {
                self.push_str(&value.to_string());
            }
            Literal::Boolean(value) => {
                self.push_str(&value.to_string());
            }
            Literal::String(value) | Literal::Char(value) => {
                self.push_str(&format!("\"{value}\""));
            }
            Literal::None => unreachable!(),
        }
    }

    fn generate_block(&mut self, block: &hir::Block) {
        // When generating a block in statement context, wrap it in braces
        if self.current_context() == BlockContext::Statement {
            self.push_str("{\n");
            self.inc_indent();
        }

        self.push_scope();

        self.generate_statements(&block.stmts);

        // Special handling for loop expressions in block completion position
        if let Some(expr) = block.expr.as_ref() {
            if let hir::ExprKind::Loop(loop_block) = &expr.kind {
                // Generate the loop as a statement instead of an expression
                self.push_indent();
                self.generate_loop_statement(loop_block);
                self.push_newline();
            } else {
                self.generate_optional_expr(block.expr.as_ref(), None);
            }
        }

        self.pop_scope();

        if self.current_context() == BlockContext::Statement {
            self.dec_indent();
            self.push_indent();
            self.push_str("}");
        }
    }

    #[inline(always)]
    pub(crate) fn generate_optional_expr(
        &mut self,
        expr: Option<&hir::Expr>,
        parent_op: Option<hir::BinaryOpKind>,
    ) {
        if let Some(expr) = expr {
            self.generate_expr(expr, parent_op);
        }
    }

    pub(crate) fn generate_expr(&mut self, expr: &hir::Expr, parent_op: Option<hir::BinaryOpKind>) {
        match &expr.kind {
            hir::ExprKind::Block(block) if self.current_context() == BlockContext::Expression => {
                self.generate_block_expression(block);
            }
            hir::ExprKind::Block(block) => self.generate_block(block),
            hir::ExprKind::Loop(_block) => {
                panic!(
                    "Loop expressions should be transformed to statements by HirJsPass before reaching codegen"
                );
            }
            hir::ExprKind::Break(expr) => {
                // In loop contexts, generate proper break; otherwise, generate return
                if self.is_in_loop_context() {
                    // In JavaScript loops, break cannot have a value
                    // For loops with accumulators, we just generate 'break' and handle the return value elsewhere
                    self.push_str("break");
                    // Note: expr value is ignored in JavaScript loop context
                    // The accumulator value should be handled by the loop return logic
                } else {
                    self.push_str("return");
                    if let Some(expr) = expr {
                        self.push_char(' ');
                        self.generate_expr(expr, parent_op);
                    }
                }
                self.push_char(';');
            }
            hir::ExprKind::Continue => self.push_str("continue"),
            hir::ExprKind::Call(expr) => self.generate_call_expression(expr),
            hir::ExprKind::TailCall(expr) => self.generate_recursive_call_expression(expr),
            hir::ExprKind::Cast(expr, _) => {
                self.generate_expr(expr, parent_op);
            }
            hir::ExprKind::FieldAccess(base, field) => {
                self.generate_field_access_expression(base, field);
            }
            hir::ExprKind::Path(path) => self.generate_path_expression(path),
            hir::ExprKind::IndexAccess(base, index) => {
                self.generate_index_access_expression(base, index);
            }
            hir::ExprKind::Let(_pattern, _expr) => todo!("Let expression not implemented yet."),
            hir::ExprKind::Literal(literal) => self.generate_literal(literal),
            hir::ExprKind::List(items) => self.generate_list_expression(items),
            hir::ExprKind::Dict(kvs) => self.generate_dict_expression(kvs),
            hir::ExprKind::Unary(op, expr) => self.generate_unary_op(op, expr),
            hir::ExprKind::Binary(op, lhs, rhs) => {
                self.generate_binary_op(*op, lhs, rhs, parent_op);
            }
            hir::ExprKind::Match(expr, arms) => self.generate_match_expression(expr, arms),
            hir::ExprKind::IfElse(expr, then_branch, else_branches) => {
                self.generate_if_else(expr, then_branch, else_branches, parent_op);
            }
            hir::ExprKind::FunctionExpression(decl) => self.generate_function_expression(decl),
            hir::ExprKind::Range(_) => todo!("Range expression not implemented yet."),
            hir::ExprKind::Wildcard => {}
        }
    }

    fn generate_unary_op(&mut self, op: &ast::UnaryOp, expr: &hir::Expr) {
        match op {
            ast::UnaryOp::Not => self.push_char('!'),
            ast::UnaryOp::Minus => self.push_char('-'),
            ast::UnaryOp::Spread => self.push_str("..."),
            ast::UnaryOp::Rest => unreachable!("Rest operator is not an operator but a pattern"),
        }

        self.generate_expr(expr, None);
    }

    fn generate_field_access_expression(&mut self, base: &hir::Expr, field: &Ident) {
        self.generate_expr(base, None);
        self.push_char('.');
        self.push_str(field.as_str());
    }

    fn generate_index_access_expression(&mut self, base: &hir::Expr, index: &hir::Expr) {
        self.generate_expr(base, None);
        self.push_char('[');
        self.generate_expr(index, None);
        self.push_char(']');
    }

    fn generate_path_expression(&mut self, path: &hir::Path) {
        // If we have a full identifier which resolves in the current scope, we use it directly.
        // We currently do not really have a path resolution mechanism, just a naive lookup.
        if let Some(identifier) = self.current_scope().resolve_variable(&path.to_string()) {
            self.push_str(&js::safe_js_variable_name(&identifier));
            return;
        }

        let first_segment = path.segments.first().unwrap();

        self.generate_identifier(&first_segment.ident);
        self.push_str(
            &path.segments[1..]
                .iter()
                .map(|segment| js::safe_js_variable_name(segment.ident.as_str()))
                .fold("".to_string(), |acc, segment| acc + "." + &segment),
        );
    }

    fn generate_list_expression(&mut self, items: &[hir::Expr]) {
        self.push_char('[');
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }
            self.generate_expr(item, None);
        }
        self.push_char(']');
    }

    fn generate_dict_expression(&mut self, kvs: &[(hir::Expr, hir::Expr)]) {
        self.push_str("{\n");
        self.inc_indent();
        for (key, value) in kvs {
            self.push_indent();
            self.generate_expr(key, None);

            // If both key and value are the same identifier, we can use shorthand syntax.
            if key.path() != value.path() {
                self.push_str(": ");
                self.generate_expr(value, None);
            }
            self.push_str(",\n");
        }
        self.dec_indent();
        self.push_indent();
        self.push_char('}');
    }

    fn generate_identifier(&mut self, name: &Ident) {
        let name_string = name.as_str();
        let identifier = self
            .current_scope()
            .resolve_variable(name_string)
            .unwrap_or_else(|| name_string.to_string());
        self.push_str(&js::safe_js_variable_name(&identifier));
    }

    fn generate_ternary_op(
        &mut self,
        expr: &hir::Expr,
        then_expr: &hir::Expr,
        else_expr: &hir::Expr,
        parent_op: Option<hir::BinaryOpKind>,
    ) {
        let needs_parenthesis = if let Some(parent_op) = parent_op {
            should_wrap_with_parentheses(
                map_operator_info(parent_op),
                JSOperatorInfo {
                    precedence: 0,
                    associativity: JSAssociativity::Right,
                },
            )
        } else {
            false
        };

        if needs_parenthesis {
            self.push_char('(');
        }

        self.generate_expr(expr, None);
        self.push_str(" ? ");
        self.generate_expr(then_expr, None);
        self.push_str(" : ");
        self.generate_expr(else_expr, None);

        if needs_parenthesis {
            self.push_char(')');
        }
    }

    pub(crate) fn should_render_if_else_as_ternary(
        &self,
        expr: &hir::Expr,
        then_branch: &hir::Block,
        else_branches: &[hir::ElseClause],
    ) -> bool {
        self.get_render_ternary()
            && (self.current_context() == BlockContext::Expression
                || self.current_completion_variable() == Some("return")) // Also allow ternary in return statements
            && else_branches.len() == 1 // Let's not nest ternary expressions for now.
            && if_else_can_render_as_ternary(expr, then_branch, else_branches)
    }

    fn generate_if_else(
        &mut self,
        expr: &hir::Expr,
        then_branch: &hir::Block,
        else_branches: &[hir::ElseClause],
        parent_op: Option<hir::BinaryOpKind>,
    ) {
        if self.should_render_if_else_as_ternary(expr, then_branch, else_branches) {
            self.generate_ternary_op(
                expr,
                then_branch.expr.as_ref().unwrap(),
                else_branches[0].consequence.expr.as_ref().unwrap(),
                parent_op,
            );
            return;
        }

        let mut lhs = String::new();
        // TODO: Potentially in a return position or other expression, before we generate the if
        //       statement, replace the current statement buffer with a new one, generate the if
        //       statement, and then swap the statement buffer back.
        //       Similar to how we generate blocks in expression position.
        // TODO: Find a way to do this generically for all expressions which are represented as
        //       statements in JavaScript.
        // TODO: In case of recursive calls in tail position, we'll want to omit lhs.
        let has_block_completions =
            self.current_context() == BlockContext::Expression && then_branch.has_completion();
        if has_block_completions {
            // TODO: We could probably reuse existing completion vars here.
            if let Some("return") = self.current_completion_variable() {
                self.push_completion_variable(Some("return"));
                lhs = self.replace_statement_buffer_with_empty_string();
                self.push_indent();
            } else {
                lhs = self.replace_statement_buffer_with_empty_string();
                let completion_tmp_var = self.current_scope().declare_tmp_variable();
                self.push_let_declaration(&completion_tmp_var);
                self.push_completion_variable(Some(&completion_tmp_var));
            }
        } else {
            self.push_completion_variable(None);
        }
        self.push_str("if (");
        let indent_level = self.current_indent();
        self.set_indent(0);
        self.generate_expr(expr, None);
        self.set_indent(indent_level);
        self.push_str(") {\n");
        self.inc_indent();
        self.flush_statement_buffer();
        self.generate_block_expression(then_branch);
        self.dec_indent();

        for else_branch in else_branches {
            self.push_indent();
            self.push_str("} else");

            if let Some(ref condition) = else_branch.condition {
                self.push_str(" if (");
                let indent_level = self.current_indent();
                self.set_indent(0);
                self.generate_expr(condition, None);
                self.set_indent(indent_level);
                self.push_char(')');
            }

            self.push_str(" {\n");
            self.inc_indent();
            self.flush_statement_buffer();
            self.generate_block_expression(&else_branch.consequence);
            self.dec_indent();
        }

        self.push_indent();
        self.push_char('}');
        if has_block_completions && self.current_completion_variable() != Some("return") {
            self.push_newline();

            // If we have an lhs, put the completion var as the rhs of the lhs.
            // Otherwise, we assign the completion_var to the previous completion_var.
            if lhs.is_empty() {
                self.push_indent();
                let prev_completion_var = self
                    .nth_completion_variable(self.current_completion_variable_count() - 2)
                    .unwrap()
                    .to_string();
                self.push_str(&prev_completion_var);
                self.push_str(" = ");
                self.push_current_completion_variable();
                self.push_str(";\n");
            } else {
                self.push_str(&lhs);
                self.push_current_completion_variable();
            }
        }
        self.pop_completion_variable();
    }

    fn generate_partial_application(
        &mut self,
        call_expr: &hir::CallExpression,
        wildcard_count: usize,
    ) {
        let mut placeholders = Vec::with_capacity(wildcard_count);

        self.push_char('(');

        if wildcard_count == 1 {
            self.push_char('_');
            placeholders.push('_'.to_string());
        } else {
            for n in 0..wildcard_count {
                if n > 0 {
                    self.push_str(", ");
                }

                let tmp_var = self.current_scope().declare_unique_variable("_");

                self.push_str(&tmp_var);

                placeholders.push(tmp_var);
            }
        }

        self.push_str(") => ");

        self.generate_expr(&call_expr.callee, None);
        self.push_char('(');

        let mut wildcard_index = 0;
        for (i, arg) in call_expr.arguments.iter().enumerate() {
            if i > 0 {
                self.push_str(", ");
            }

            if let hir::ExprKind::Wildcard = arg.kind {
                self.push_str(&placeholders[wildcard_index]);
                wildcard_index += 1;
            } else {
                self.generate_expr(arg, None);
            }
        }

        self.push_char(')');
    }

    pub(crate) fn generate_call_expression(&mut self, call_expr: &hir::CallExpression) {
        // Check for special temp var + block combination
        if let hir::ExprKind::Path(path) = &call_expr.callee.kind {
            if path.segments.len() == 1 && path.segments[0].ident.as_str() == "__TEMP_VAR_BLOCK__" {
                // First argument is the temp variable name (as string literal)
                // Second argument is the block expression
                if call_expr.arguments.len() == 2
                    && let hir::ExprKind::Literal(lit) = &call_expr.arguments[0].kind
                    && let tlang_ast::token::Literal::String(temp_name) = lit.as_ref()
                {
                    // Generate: let $tmp$0;{...}
                    self.push_str("let ");
                    self.push_str(temp_name);
                    self.push_str(";");

                    // Generate the block expression with expression context to get braces
                    self.push_context(BlockContext::Expression);
                    if let hir::ExprKind::Block(block) = &call_expr.arguments[1].kind {
                        self.push_str("{\n");
                        self.inc_indent();
                        self.push_scope();
                        self.generate_statements(&block.stmts);
                        self.generate_optional_expr(block.expr.as_ref(), None);
                        self.pop_scope();
                        self.dec_indent();
                        self.push_indent();
                        self.push_str("}");
                    }
                    self.pop_context();
                    return;
                }
            }

            // Check for special temp var + if/else combination
            if path.segments.len() == 1 && path.segments[0].ident.as_str() == "__TEMP_VAR_IF_ELSE__"
            {
                // First argument is the temp variable name (as string literal)
                // Second argument is the if/else expression
                if call_expr.arguments.len() == 2
                    && let hir::ExprKind::Literal(lit) = &call_expr.arguments[0].kind
                    && let tlang_ast::token::Literal::String(temp_name) = lit.as_ref()
                {
                    // Generate: let $tmp$0;if(...){...}else{...}
                    self.push_str("let ");
                    self.push_str(temp_name);
                    self.push_str(";");

                    // Generate the if/else expression as a statement
                    if let hir::ExprKind::IfElse(condition, then_branch, else_branches) =
                        &call_expr.arguments[1].kind
                    {
                        self.push_str("if (");
                        let indent_level = self.current_indent();
                        self.set_indent(0);
                        self.generate_expr(condition, None);
                        self.set_indent(indent_level);
                        self.push_str(") {\n");
                        self.inc_indent();
                        self.generate_statements(&then_branch.stmts);
                        self.generate_optional_expr(then_branch.expr.as_ref(), None);
                        self.dec_indent();

                        for else_branch in else_branches {
                            self.push_indent();
                            self.push_str("} else");

                            if let Some(ref condition) = else_branch.condition {
                                self.push_str(" if (");
                                let indent_level = self.current_indent();
                                self.set_indent(0);
                                self.generate_expr(condition, None);
                                self.set_indent(indent_level);
                                self.push_char(')');
                            }

                            self.push_str(" {\n");
                            self.inc_indent();
                            self.generate_statements(&else_branch.consequence.stmts);
                            self.generate_optional_expr(
                                else_branch.consequence.expr.as_ref(),
                                None,
                            );
                            self.dec_indent();
                        }

                        self.push_indent();
                        self.push_char('}');
                    }
                    return;
                }
            }

            // Check for special temp var + match combination
            if path.segments.len() == 1 && path.segments[0].ident.as_str() == "__TEMP_VAR_MATCH__" {
                // First argument is the temp variable name (as string literal)
                // Second argument is the match expression
                if call_expr.arguments.len() == 2
                    && let hir::ExprKind::Literal(lit) = &call_expr.arguments[0].kind
                    && let tlang_ast::token::Literal::String(temp_name) = lit.as_ref()
                {
                    // DEBUG: Output when __TEMP_VAR_MATCH__ is processed
                    self.push_str("/* DEBUG: __TEMP_VAR_MATCH__ with temp_name=");
                    self.push_str(temp_name);
                    self.push_str(" */");
                    self.push_newline();
                    
                    // Generate the match expression with the completion variable
                    if let hir::ExprKind::Match(match_expr, match_arms) =
                        &call_expr.arguments[1].kind
                    {
                        self.generate_match_expression_with_completion_var(
                            match_expr,
                            match_arms,
                            Some(temp_name),
                        );

                        // Note: Do not output temp variable name here - HIR JS pass handles assignment separately
                    }
                    return;
                }
            }

            // Check for special temp var + loop combination
            if path.segments.len() == 1 && path.segments[0].ident.as_str() == "__TEMP_VAR_LOOP__" {
                // First argument is the temp variable name (as string literal)
                // Second argument is the loop expression
                if call_expr.arguments.len() == 2
                    && let hir::ExprKind::Literal(lit) = &call_expr.arguments[0].kind
                    && let tlang_ast::token::Literal::String(temp_name) = lit.as_ref()
                {
                    // Generate: let $tmp$0;[loop as statement]
                    self.push_str("let ");
                    self.push_str(temp_name);
                    self.push_str(";");

                    // Generate the loop as a statement, transforming breaks
                    if let hir::ExprKind::Loop(block) = &call_expr.arguments[1].kind {
                        self.generate_loop_statement_with_temp_var(block, temp_name);
                    }
                    return;
                }
            }
        }

        // TODO: If the call is to a struct, we instead call it with `new` and map the fields to
        // the positional arguments of the constructor.

        let wildcard_count = call_expr.wildcard_count();

        if wildcard_count > 0 {
            return self.generate_partial_application(call_expr, wildcard_count);
        }

        self.generate_expr(&call_expr.callee, None);
        self.push_char('(');

        let mut args_iter = call_expr.arguments.iter();

        if let Some(arg) = args_iter.next() {
            self.generate_expr(arg, None);
        }

        for arg in args_iter {
            self.push_str(", ");
            self.generate_expr(arg, None);
        }

        self.push_char(')');
    }

    /// Generate a loop as a statement with break expressions transformed to assignments
    pub(crate) fn generate_loop_statement_with_temp_var(
        &mut self,
        block: &hir::Block,
        temp_var: &str,
    ) {
        self.push_str("for (;;) {");
        self.push_newline();
        self.inc_indent();

        // Push loop context so break/continue expressions work correctly
        self.push_context(BlockContext::Loop);

        // Generate statements in the loop body, transforming break expressions
        for stmt in &block.stmts {
            self.generate_stmt_with_loop_temp_var(stmt, temp_var);
        }

        // Handle block completion if exists - this would be the default return value
        if let Some(completion_expr) = &block.expr {
            self.push_indent();

            // Generate the expression and capture the result in the temp variable
            // The pattern match will assign the result to the temp variable through finalization
            self.generate_expr_with_loop_temp_var(completion_expr, temp_var, None);
            self.push_newline();

            // The pattern match generator will handle the accumulator assignment in loop context
            // through its finalize_match_expression function, so we don't need to do it here
        }

        // Pop loop context
        self.pop_context();

        // At the end of the loop, assign the accumulator value to the temp variable
        // This ensures the loop result is the final accumulator value
        self.push_indent();

        // For loops in loop context, the pattern match might use a different completion variable
        // than the one provided, so we need to get the correct one to use
        if self.is_in_loop_context() {
            let actual_completion_var = self.current_scope().get_latest_temp_variable();
            if actual_completion_var != temp_var && actual_completion_var.starts_with("$tmp$") {
                // Use the actual completion variable that was created by the pattern match
                self.push_str(temp_var);
                self.push_str(" = ");
                self.push_str(&actual_completion_var);
            } else {
                self.push_str(temp_var);
                self.push_str(" = accumulator$$");
            }
        } else {
            self.push_str(temp_var);
            self.push_str(" = accumulator$$");
        }
        self.push_newline();

        self.dec_indent();
        self.push_indent();
        self.push_str("}");
    }

    /// Generate a statement within a loop context, transforming break expressions
    fn generate_stmt_with_loop_temp_var(&mut self, stmt: &hir::Stmt, temp_var: &str) {
        match &stmt.kind {
            hir::StmtKind::Expr(expr) => {
                self.push_indent();
                self.push_context(BlockContext::Statement);
                self.generate_expr_with_loop_temp_var(expr, temp_var, None);
                self.pop_context();

                // Don't add semicolons for match expressions or binary assignments containing matches
                let needs_semicolon = match &expr.kind {
                    hir::ExprKind::Match(..) => false,
                    hir::ExprKind::Binary(hir::BinaryOpKind::Assign, _, rhs) => {
                        // If the RHS is a match expression, don't add semicolon
                        !matches!(rhs.kind, hir::ExprKind::Match(..))
                    }
                    _ => false, // Conservative: don't add semicolons in loop temp var context
                };

                if needs_semicolon {
                    self.push_char(';');
                }
                self.push_newline();
            }
            // For other statement types, generate normally since they don't contain break
            _ => {
                self.generate_stmt(stmt);
            }
        }
    }

    /// Generate an expression within a loop context, transforming break expressions  
    fn generate_expr_with_loop_temp_var(
        &mut self,
        expr: &hir::Expr,
        temp_var: &str,
        parent_op: Option<hir::BinaryOpKind>,
    ) {
        match &expr.kind {
            hir::ExprKind::Break(break_expr) => {
                // Transform break to assignment + break
                if let Some(break_value) = break_expr {
                    self.push_str(temp_var);
                    self.push_str(" = ");
                    self.generate_expr(break_value, parent_op);
                    self.push_str("; ");
                }
                self.push_str("break");
            }
            hir::ExprKind::Block(block) => {
                // Recursively handle blocks within the loop
                self.push_str("{");
                self.push_newline();
                self.inc_indent();

                for stmt in &block.stmts {
                    self.generate_stmt_with_loop_temp_var(stmt, temp_var);
                }

                if let Some(completion_expr) = &block.expr {
                    self.push_indent();
                    self.generate_expr_with_loop_temp_var(completion_expr, temp_var, None);
                    self.push_newline();
                }

                self.dec_indent();
                self.push_indent();
                self.push_str("}");
            }
            hir::ExprKind::IfElse(condition, then_branch, else_branches) => {
                // Handle if-else that might contain break expressions
                self.push_str("if (");
                self.generate_expr(condition, None);
                self.push_str(") ");
                self.generate_block_with_loop_temp_var(then_branch, temp_var);

                for else_clause in else_branches {
                    if let Some(else_condition) = &else_clause.condition {
                        self.push_str(" else if (");
                        self.generate_expr(else_condition, None);
                        self.push_str(") ");
                    } else {
                        self.push_str(" else ");
                    }
                    self.generate_block_with_loop_temp_var(&else_clause.consequence, temp_var);
                }
            }
            hir::ExprKind::Match(match_expr, match_arms) => {
                // Handle match expressions with the loop temp variable as completion variable
                self.generate_match_expression_with_completion_var(
                    match_expr,
                    match_arms,
                    Some(temp_var),
                );
            }
            // For other expressions, generate normally
            _ => {
                self.generate_expr(expr, parent_op);
            }
        }
    }

    /// Generate a block within a loop context, transforming break expressions
    fn generate_block_with_loop_temp_var(&mut self, block: &hir::Block, temp_var: &str) {
        self.push_str("{");
        self.push_newline();
        self.inc_indent();

        for stmt in &block.stmts {
            self.generate_stmt_with_loop_temp_var(stmt, temp_var);
        }

        if let Some(completion_expr) = &block.expr {
            self.push_indent();
            self.generate_expr_with_loop_temp_var(completion_expr, temp_var, None);
            self.push_newline();
        }

        self.dec_indent();
        self.push_indent();
        self.push_str("}");
    }
}
