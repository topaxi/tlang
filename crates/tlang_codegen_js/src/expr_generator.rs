use tlang_ast::node::{self as ast, Ident};
use tlang_ast::token::Literal;
use tlang_hir::hir;

use crate::binary_operator_generator::{
    JSAssociativity, JSOperatorInfo, map_operator_info, should_wrap_with_parentheses,
};
use crate::generator::{BlockContext, CodegenJS};
use crate::js;
use crate::js_expr_utils::if_else_can_render_as_ternary;

impl CodegenJS {
    /// Check if a completion variable reference should be omitted
    /// This happens when all execution paths in the block end with return statements
    fn should_omit_completion_var_reference(&self, block: &hir::Block, completion_var: &str) -> bool {
        // Skip if not a temp variable
        if !completion_var.starts_with("$hir$") {
            return false;
        }
        
        // Check if all statements in the block end with return statements
        // This indicates the completion variable is never reached
        let result = self.block_all_paths_return(block);
        
        // Debug output
        eprintln!("DEBUG: should_omit_completion_var_reference: {} -> {}", completion_var, result);
        
        result
    }
    
    /// Check if all execution paths in a block end with return statements
    fn block_all_paths_return(&self, block: &hir::Block) -> bool {
        // If the block has no completion expression, check if the last statement is a return
        if block.expr.is_none() {
            if let Some(last_stmt) = block.stmts.last() {
                return matches!(last_stmt.kind, hir::StmtKind::Return(_));
            }
            return false;
        }
        
        // If the block has a completion expression, check if it's unreachable due to returns
        for stmt in &block.stmts {
            if let hir::StmtKind::Expr(expr) = &stmt.kind {
                if let hir::ExprKind::Match(_, arms) = &expr.kind {
                    // Check if all match arms end with return statements
                    let all_arms_return = arms.iter().all(|arm| {
                        if let Some(last_stmt) = arm.block.stmts.last() {
                            matches!(last_stmt.kind, hir::StmtKind::Return(_))
                        } else {
                            false
                        }
                    });
                    if all_arms_return {
                        return true;
                    }
                }
            }
        }
        
        false
    }

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
            
            // Only output the completion variable reference if it's actually reachable
            eprintln!("DEBUG: About to check completion var reference: {}", completion_tmp_var);
            if !self.should_omit_completion_var_reference(block, &completion_tmp_var) {
                eprintln!("DEBUG: Outputting completion var reference: {}", completion_tmp_var);
                self.push_str(completion_tmp_var.as_str());
            } else {
                eprintln!("DEBUG: Omitting completion var reference: {}", completion_tmp_var);
            }
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
            hir::ExprKind::Block(_block) if self.current_context() == BlockContext::Expression => {
                panic!(
                    "Block expressions should be transformed to statements by HirJsPass before reaching codegen"
                );
            }
            hir::ExprKind::Block(block) => self.generate_block(block),
            hir::ExprKind::Loop(block) => {
                eprintln!("ERROR: Found loop expression at HIR ID: {:?}", expr.hir_id);
                eprintln!("Loop block: {:#?}", block);
                eprintln!("Is in loop context: {}", self.is_in_loop_context());
                panic!(
                    "Loop expressions should be transformed to statements by HirJsPass before reaching codegen. HIR ID: {:?}",
                    expr.hir_id
                );
            }
            hir::ExprKind::Break(expr) => {
                // In loop contexts, generate proper break; otherwise, generate return
                // However, if we're inside a function expression (even if nested in a loop), generate return
                if self.is_in_loop_context()
                    && self
                        .get_function_context()
                        .is_none_or(|ctx| !ctx.is_expression)
                {
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
            hir::ExprKind::Continue => {
                self.push_str("continue");
                self.push_char(';');
            }
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
            hir::ExprKind::Match(_expr, _arms) => {
                if self.current_context() == BlockContext::Expression {
                    panic!(
                        "Match expressions should be transformed to statements by HirJsPass before reaching codegen"
                    );
                } else {
                    self.generate_match_expression(_expr, _arms);
                }
            }
            hir::ExprKind::IfElse(expr, then_branch, else_branches) => {
                self.generate_if_else(expr, then_branch, else_branches, parent_op);
            }
            hir::ExprKind::FunctionExpression(decl) => self.generate_function_expression(decl),
            hir::ExprKind::Range(_) => todo!("Range expression not implemented yet."),
            hir::ExprKind::Wildcard => {
                // Wildcard expressions should be rendered as 'undefined' in JavaScript
                // This is used for temp variable initialization in the HIR JS pass
                self.push_str("undefined");
            }
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
        (self.current_context() == BlockContext::Expression
            || self.current_completion_variable() == Some("return") // Also allow ternary in return statements
            || self.current_context() == BlockContext::Statement) // Allow ternary in statement context for assignments
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

        // If we reach here, the if-else cannot be rendered as a ternary expression
        // In most expression contexts, this should have been handled by HIR JS pass
        // For now, allow fallback to completion variable handling for complex cases
        // TODO: Expand HIR JS pass to handle all remaining cases
        if self.current_context() == BlockContext::Expression
            && self.current_completion_variable() != Some("return")
        {
            // For debugging: log when we hit complex cases that could be improved
            eprintln!(
                "Info: Complex if-else expression in expression context using completion variables. \
                This could potentially be handled by HIR JS pass instead. \
                Context: {:?}, Completion variable: {:?}",
                self.current_context(),
                self.current_completion_variable()
            );
        }

        // Handle if-else in statement context (simple case without completion variables)
        self.push_completion_variable(None);
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
        // Legacy temp var handling removed - HIR JS pass now handles complex expressions with wildcards

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
}
