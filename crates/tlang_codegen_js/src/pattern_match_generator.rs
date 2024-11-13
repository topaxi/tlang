use std::collections::HashSet;

use tlang_ast::node as ast;

use crate::generator::{BlockContext, CodegenJS};

fn match_args_have_completions(arms: &[ast::MatchArm]) -> bool {
    arms.iter().any(|arm| match &arm.expression.kind {
        ast::ExprKind::Block(block) => block.has_completion(),
        _ => true,
    })
}

impl CodegenJS {
    fn generate_match_arm_expression(&mut self, expression: &ast::Expr) {
        if let ast::ExprKind::Block(_) = &expression.kind {
            self.generate_expr(expression, None);
        } else {
            self.push_indent();
            let completion_tmp_var = self.current_completion_variable().unwrap();
            self.push_str(&format!("{completion_tmp_var} = "));
            self.generate_expr(expression, None);
            self.push_str(";\n");
        }
    }

    fn get_pat_identifiers(pattern: &ast::Pattern) -> Vec<String> {
        match &pattern.kind {
            ast::PatternKind::Identifier(ident_pattern) => {
                vec![ident_pattern.name.to_string()]
            }
            ast::PatternKind::_Self => vec!["this".to_string()],
            ast::PatternKind::Enum(enum_pattern) => {
                let mut bindings = Vec::new();
                for pattern in &enum_pattern.elements {
                    bindings.extend(Self::get_pat_identifiers(pattern));
                }
                bindings
            }
            ast::PatternKind::Literal(_) => Vec::new(),
            ast::PatternKind::List(patterns) => {
                let mut bindings = Vec::new();
                for pattern in patterns {
                    bindings.extend(Self::get_pat_identifiers(pattern));
                }
                bindings
            }
            ast::PatternKind::Rest(pattern) => Self::get_pat_identifiers(pattern),
            ast::PatternKind::Wildcard | ast::PatternKind::None => Vec::new(),
        }
    }

    fn generate_pat_condition(&mut self, pat: &ast::Pattern, parent_js_expr: &str) {
        self.push_pat(pat);

        match &pat.kind {
            ast::PatternKind::Identifier(ident_pattern) => {
                let binding_name = self
                    .current_scope()
                    .resolve_variable(ident_pattern.as_str())
                    .unwrap();

                self.push_str(&format!("({} = {}, true)", binding_name, parent_js_expr));
            }
            ast::PatternKind::_Self => todo!("Implement self pattern."),
            ast::PatternKind::Enum(enum_pattern) => {
                let enum_variant_name = enum_pattern.path.segments.last().unwrap();

                self.push_str(&format!(
                    "{}.tag === \"{}\"",
                    parent_js_expr, enum_variant_name
                ));

                if enum_pattern.named_fields {
                    for _pattern in enum_pattern.elements.iter() {
                        self.push_str(" && ");

                        todo!("Instead of named fields, we should probably have a key value kinda thing here.");
                        //let parent_js_expr = format!("{}.{}", parent_js_expr);

                        //self.generate_pat_condition(pattern, &parent_js_expr);
                    }
                } else {
                    for (i, pattern) in enum_pattern.elements.iter().enumerate() {
                        self.push_str(" && ");

                        let parent_js_expr = format!("{}[{i}]", parent_js_expr);

                        self.generate_pat_condition(pattern, &parent_js_expr);
                    }
                }
            }
            ast::PatternKind::Literal(literal) => {
                self.push_str(parent_js_expr);
                self.push_str(" === ");
                self.generate_literal(literal)
            }
            ast::PatternKind::List(patterns) => {
                if patterns.is_empty() {
                    self.push_str(parent_js_expr);
                    self.push_str(".length === 0");
                } else {
                    self.push_str(parent_js_expr);
                    self.push_str(".length >= ");
                    self.push_str(
                        &patterns
                            .iter()
                            .filter(|pat| !matches!(pat.kind, ast::PatternKind::Rest(_)))
                            .count()
                            .to_string(),
                    );

                    for (i, pattern) in patterns.iter().enumerate() {
                        self.push_str(" && ");
                        // This feels awkward, could this be handled when we match the Rest pattern
                        // below?
                        let parent_js_expr = if matches!(pattern.kind, ast::PatternKind::Rest(_)) {
                            format!("{}.slice({})", parent_js_expr, i)
                        } else {
                            format!("{}[{}]", parent_js_expr, i)
                        };
                        self.generate_pat_condition(pattern, &parent_js_expr);
                    }
                }
            }
            ast::PatternKind::Rest(pattern) => {
                self.generate_pat_condition(pattern, parent_js_expr);
            }
            ast::PatternKind::Wildcard | ast::PatternKind::None => {}
        }

        self.pop_pat();
    }

    pub(crate) fn generate_match_expression(&mut self, match_expr: &ast::MatchExpression) {
        // TODO: A lot here is copied from the if statement generator.
        let lhs = self.replace_statement_buffer(String::new());
        let has_block_completions = match_args_have_completions(&match_expr.arms);
        let match_value_tmp_var = self.current_scope().declare_tmp_variable();
        self.push_let_declaration_to_expr(&match_value_tmp_var, &match_expr.expression);

        let mut unique = HashSet::new();
        // There's optimization opportunities in case the match expression is a
        // list of identifiers, then we can just alias the identifiers within the arms. This
        // case should be pretty common in function overloads.
        let all_pat_identifiers = match_expr.arms.iter().flat_map(|arm| {
            let mut idents = vec![];
            idents.extend(Self::get_pat_identifiers(&arm.pattern));
            if let Some(guard) = &arm.guard {
                if let ast::ExprKind::Let(pat, _) = &guard.kind {
                    idents.extend(Self::get_pat_identifiers(pat));
                }
            }
            idents
        });

        let has_let_guard = match_expr.arms.iter().any(|arm| {
            if let Some(guard) = &arm.guard {
                if let ast::ExprKind::Let(..) = &guard.kind {
                    return true;
                }
            }
            false
        });

        let let_guard_var = if has_let_guard {
            let binding = self.current_scope().declare_tmp_variable();
            self.push_char(',');
            self.push_str(&binding);
            binding
        } else {
            String::new()
        };

        for binding in all_pat_identifiers {
            if unique.insert(binding.clone()) {
                let binding = self.current_scope().declare_variable(&binding);
                self.push_char(',');
                self.push_str(&binding);
            }
        }

        if has_block_completions {
            let completion_tmp_var = self.current_scope().declare_tmp_variable();
            self.push_char(',');
            self.push_str(&completion_tmp_var);
            self.push_completion_variable(Some(completion_tmp_var));
        } else {
            self.push_completion_variable(None);
        }

        self.push_char(';');

        for (
            i,
            ast::MatchArm {
                id: _,
                pattern,
                guard,
                expression,
            },
        ) in match_expr.arms.iter().enumerate()
        {
            if !pattern.is_wildcard() {
                self.push_str("if (");
                self.generate_pat_condition(pattern, &match_value_tmp_var);
                if let Some(guard) = guard {
                    self.generate_match_arm_guard(guard, &let_guard_var);
                }
                self.push_str(") {\n");
                self.inc_indent();
                self.push_context(BlockContext::Expression);
                self.generate_match_arm_expression(expression);
                self.pop_context();
                self.dec_indent();
                self.push_indent();
                if i == match_expr.arms.len() - 1 {
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

    fn generate_match_arm_guard(&mut self, guard: &ast::Expr, let_guard_var: &str) {
        self.push_str(" && ");
        // If the guard is a let expression, we special case this here, normal if let expressions
        // are handled in the lowering process and will be transformed to a match expression.
        if let ast::ExprKind::Let(pat, expr) = &guard.kind {
            self.push_char('(');
            self.push_str(let_guard_var);
            self.push_str(" = ");
            self.generate_expr(expr, None);
            self.push_str(", true) && ");
            self.generate_pat_condition(pat, let_guard_var);
        } else {
            self.generate_expr(guard, None);
        }
    }
}
