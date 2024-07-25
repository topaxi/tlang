use tlang_ast::node::{Expr, ExprKind, MatchArm, Pattern, PatternKind};

use crate::generator::{BlockContext, CodegenJS};

fn match_args_have_completions(arms: &[MatchArm]) -> bool {
    arms.iter().any(|arm| match &arm.expression.kind {
        ExprKind::Block(block) => block.has_completion(),
        _ => true,
    })
}

fn is_wildcard_pattern(pattern: &Pattern) -> bool {
    matches!(pattern.kind, PatternKind::Wildcard)
}

fn generate_match_arm_expression(codegen: &mut CodegenJS, expression: &Expr) {
    if let ExprKind::Block(_) = &expression.kind {
        codegen.generate_expr(expression, None);
    } else {
        let completion_tmp_var = codegen.current_completion_variable().clone().unwrap();
        codegen.push_indent();
        codegen.push_str(&format!("{completion_tmp_var} = "));
        codegen.generate_expr(expression, None);
        codegen.push_str(";\n");
    }
}

pub fn generate_match_expression(codegen: &mut CodegenJS, expression: &Expr, arms: &[MatchArm]) {
    // TODO: A lot here is copied from the if statement generator.
    let lhs = codegen.replace_statement_buffer(String::new());
    let has_block_completions = match_args_have_completions(arms);
    let match_value_tmp_var = codegen.current_scope().declare_tmp_variable();
    codegen.push_str(&format!("let {match_value_tmp_var} = "));
    codegen.generate_expr(expression, None);
    if has_block_completions {
        let completion_tmp_var = codegen.current_scope().declare_tmp_variable();
        codegen.push_indent();
        codegen.push_str(&format!(",{completion_tmp_var};"));
        codegen.push_completion_variable(Some(completion_tmp_var));
    } else {
        codegen.push_completion_variable(None);
    }

    codegen.push_indent();

    for (
        i,
        MatchArm {
            pattern,
            expression,
        },
    ) in arms.iter().enumerate()
    {
        if !is_wildcard_pattern(pattern) {
            codegen.push_str("if (");
            codegen.push_str(&match_value_tmp_var);
            codegen.push_str(" === ");
            codegen.generate_pat(pattern);
            codegen.push_str(") {\n");
            codegen.inc_indent();
            codegen.push_context(BlockContext::Expression);
            generate_match_arm_expression(codegen, expression);
            codegen.pop_context();
            codegen.dec_indent();
            codegen.push_indent();
            if i == arms.len() - 1 {
                codegen.push_str("}");
            } else {
                codegen.push_str("} else ");
            }
        } else {
            codegen.push_str("{\n");
            codegen.inc_indent();
            codegen.push_context(BlockContext::Expression);
            generate_match_arm_expression(codegen, expression);
            codegen.pop_context();
            codegen.dec_indent();
            codegen.push_indent();
            codegen.push_str("}");
        }
    }

    if has_block_completions {
        codegen.push_str("\n");
        // If we have an lhs, put the completion var as the rhs of the lhs.
        // Otherwise, we assign the completion_var to the previous completion_var.
        if !lhs.is_empty() {
            codegen.push_str(&lhs);
            let completion_var = codegen.current_completion_variable().unwrap().clone();
            codegen.push_str(&completion_var);
        } else {
            codegen.push_indent();
            let completion_var = codegen.current_completion_variable().clone().unwrap();
            let prev_completion_var = codegen
                .nth_completion_variable(codegen.current_completion_variable_count() - 2)
                .unwrap();
            codegen.push_str(&format!("{prev_completion_var} = {completion_var};\n"));
        }
    }
    codegen.pop_completion_variable();
}
