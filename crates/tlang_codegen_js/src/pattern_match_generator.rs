use tlang_ast::node::{Expr, ExprKind, MatchArm};

use crate::generator::{BlockContext, CodegenJS};

fn match_args_have_completions(arms: &[MatchArm]) -> bool {
    arms.iter().any(|arm| match &arm.expression.kind {
        ExprKind::Block(block) => block.has_completion(),
        _ => true,
    })
}

impl CodegenJS {
    fn generate_match_arm_expression(&mut self, expression: &Expr) {
        if let ExprKind::Block(_) = &expression.kind {
            self.generate_expr(expression, None);
        } else {
            self.push_indent();
            let completion_tmp_var = self.current_completion_variable().unwrap();
            self.push_str(&format!("{completion_tmp_var} = "));
            self.generate_expr(expression, None);
            self.push_str(";\n");
        }
    }

    pub(crate) fn generate_match_expression(&mut self, expression: &Expr, arms: &[MatchArm]) {
        // TODO: A lot here is copied from the if statement generator.
        let lhs = self.replace_statement_buffer(String::new());
        let has_block_completions = match_args_have_completions(arms);
        let match_value_tmp_var = self.current_scope().declare_tmp_variable();
        self.push_let_declaration_to_expr(&match_value_tmp_var, expression);
        if has_block_completions {
            let completion_tmp_var = self.current_scope().declare_tmp_variable();
            self.push_indent();
            self.push_str(&format!(",{completion_tmp_var};"));
            self.push_completion_variable(Some(completion_tmp_var));
        } else {
            self.push_completion_variable(None);
        }

        self.push_indent();

        for (
            i,
            MatchArm {
                pattern,
                expression,
            },
        ) in arms.iter().enumerate()
        {
            if !pattern.is_wildcard() {
                self.push_str("if (");
                self.push_str(&match_value_tmp_var);
                self.push_str(" === ");
                self.generate_pat(pattern);
                self.push_str(") {\n");
                self.inc_indent();
                self.push_context(BlockContext::Expression);
                self.generate_match_arm_expression(expression);
                self.pop_context();
                self.dec_indent();
                self.push_indent();
                if i == arms.len() - 1 {
                    self.push_char('}');
                } else {
                    self.push_str("} else ");
                }
            } else {
                self.push_str("{\n");
                self.inc_indent();
                self.push_context(BlockContext::Expression);
                self.generate_match_arm_expression(expression);
                self.pop_context();
                self.dec_indent();
                self.push_indent();
                self.push_char('}');
            }
        }

        if has_block_completions {
            self.push_newline();
            // If we have an lhs, put the completion var as the rhs of the lhs.
            // Otherwise, we assign the completion_var to the previous completion_var.
            if !lhs.is_empty() {
                self.push_str(&lhs);
                let completion_var = self.current_completion_variable().unwrap().to_string();
                self.push_str(&completion_var);
            } else {
                self.push_indent();
                let completion_var = self.current_completion_variable().unwrap();
                let prev_completion_var = self
                    .nth_completion_variable(self.current_completion_variable_count() - 2)
                    .unwrap();
                self.push_str(&format!("{prev_completion_var} = {completion_var};\n"));
            }
        }
        self.pop_completion_variable();
    }
}
