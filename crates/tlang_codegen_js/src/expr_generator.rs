use tlang_ast::node::{self as ast, Ident};
use tlang_ast::token::Literal;
use tlang_hir::hir;

use crate::binary_operator_generator::{
    map_operator_info, should_wrap_with_parentheses, JSAssociativity, JSOperatorInfo,
};
use crate::generator::{BlockContext, CodegenJS};

impl CodegenJS {
    /// Generates blocks in expression position.
    fn generate_block_expression(&mut self, block: &hir::Block) {
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
            if completion.is_tail_call() {
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
            self.push_str("};\n");
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
        }
    }

    fn generate_block(&mut self, block: &hir::Block) {
        self.push_scope();

        self.generate_statements(&block.stmts);
        self.generate_optional_expr(block.expr.as_ref(), None);

        self.pop_scope();
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
                self.generate_block_expression(block)
            }
            hir::ExprKind::Block(block) => self.generate_block(block),
            hir::ExprKind::Call(expr) => self.generate_call_expression(expr),
            hir::ExprKind::FieldAccess(base, field) => {
                self.generate_field_access_expression(base, field)
            }
            hir::ExprKind::Path(path) => self.generate_path_expression(path),
            hir::ExprKind::IndexAccess(base, index) => {
                self.generate_index_access_expression(base, index)
            }
            hir::ExprKind::Let(_pattern, _expr) => todo!("Let expression not implemented yet."),
            hir::ExprKind::Literal(literal) => self.generate_literal(literal),
            hir::ExprKind::List(items) => self.generate_list_expression(items),
            hir::ExprKind::Dict(kvs) => self.generate_dict_expression(kvs),
            hir::ExprKind::Unary(op, expr) => self.generate_unary_op(op, expr),
            hir::ExprKind::Binary(op, lhs, rhs) => {
                self.generate_binary_op(*op, lhs, rhs, parent_op)
            }
            hir::ExprKind::Match(expr, arms) => self.generate_match_expression(expr, arms),
            hir::ExprKind::IfElse(expr, then_branch, else_branches) => {
                self.generate_if_else(expr, then_branch, else_branches, parent_op)
            }
            hir::ExprKind::FunctionExpression(decl) => self.generate_function_expression(decl),
            hir::ExprKind::TailCall(expr) => self.generate_recursive_call_expression(expr),
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
        let first_segment = path.segments.first().unwrap();

        self.generate_identifier(&first_segment.ident);
        self.push_str(
            &path.segments[1..]
                .iter()
                .fold("".to_string(), |acc, segment| {
                    acc + "." + segment.ident.as_str()
                }),
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
        for (i, (key, value)) in kvs.iter().enumerate() {
            if i > 0 {
                self.push_str(",\n");
            }
            self.push_indent();
            self.generate_expr(key, None);

            // If both key and value are the same identifier, we can use shorthand syntax.
            if key.path() != value.path() {
                self.push_str(": ");
                self.generate_expr(value, None);
            }
        }
        self.push_str(",\n");
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
        self.push_str(&identifier);
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
            && self.current_context() == BlockContext::Expression
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
            if !lhs.is_empty() {
                self.push_str(&lhs);
                self.push_current_completion_variable();
            } else {
                self.push_indent();
                let prev_completion_var = self
                    .nth_completion_variable(self.current_completion_variable_count() - 2)
                    .unwrap()
                    .to_string();
                self.push_str(&prev_completion_var);
                self.push_str(" = ");
                self.push_current_completion_variable();
                self.push_str(";\n");
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

fn if_else_can_render_as_ternary(
    expr: &hir::Expr,
    then_branch: &hir::Block,
    else_branches: &[hir::ElseClause],
) -> bool {
    then_branch.stmts.is_empty()
        && expr_can_render_as_js_expr(expr)
        && expr_can_render_as_js_expr(then_branch.expr.as_ref().unwrap())
        && else_branches.iter().all(|else_branch| {
            else_branch.consequence.stmts.is_empty()
                && (else_branch.condition.is_none()
                    || expr_can_render_as_js_expr(else_branch.condition.as_ref().unwrap()))
                && expr_can_render_as_js_expr(else_branch.consequence.expr.as_ref().unwrap())
        })
}

pub(crate) fn expr_can_render_as_js_expr(expr: &hir::Expr) -> bool {
    match &expr.kind {
        hir::ExprKind::Path(..) => true,
        hir::ExprKind::Let(..) => false,
        hir::ExprKind::Literal(..) => true,
        hir::ExprKind::Binary(_, lhs, rhs) => {
            expr_can_render_as_js_expr(lhs) && expr_can_render_as_js_expr(rhs)
        }
        hir::ExprKind::Unary(_, expr) => expr_can_render_as_js_expr(expr),
        hir::ExprKind::Block(..) => false,
        hir::ExprKind::IfElse(..) => false,
        hir::ExprKind::Match(..) => false,
        hir::ExprKind::Call(call_expr) => {
            call_expr.arguments.iter().all(expr_can_render_as_js_expr)
        }
        hir::ExprKind::FieldAccess(base, _) => expr_can_render_as_js_expr(base),
        hir::ExprKind::IndexAccess(base, index) => {
            expr_can_render_as_js_expr(base) && expr_can_render_as_js_expr(index)
        }
        hir::ExprKind::TailCall(_) => false,
        hir::ExprKind::List(exprs) => exprs.iter().all(expr_can_render_as_js_expr),
        hir::ExprKind::Dict(exprs) => exprs
            .iter()
            .all(|kv| expr_can_render_as_js_expr(&kv.0) && expr_can_render_as_js_expr(&kv.1)),
        hir::ExprKind::FunctionExpression(..) => true,
        hir::ExprKind::Range(..) => true,
        hir::ExprKind::Wildcard => true,
    }
}
