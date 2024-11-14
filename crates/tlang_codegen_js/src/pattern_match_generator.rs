use std::collections::HashSet;

use tlang_hir::hir;

use crate::generator::{BlockContext, CodegenJS};

#[derive(Debug, Default)]
pub(crate) struct MatchContextStack(Vec<MatchContext>);

impl MatchContextStack {
    fn push(&mut self, context: MatchContext) {
        self.0.push(context);
    }

    fn pop(&mut self) -> Option<MatchContext> {
        self.0.pop()
    }

    fn last(&self) -> Option<&MatchContext> {
        self.0.last()
    }

    fn has_fixed_ident(&self) -> bool {
        self.last()
            .map_or(false, |context| context.fixed_ident.is_some())
    }

    fn get_fixed_ident(&self) -> Option<&str> {
        self.last()
            .and_then(|context| context.fixed_ident.as_deref())
    }

    fn fixed_list(&self) -> bool {
        self.last().map_or(false, |context| context.fixed_list)
    }

    fn get_fixed_list_identifier(&self, index: usize) -> Option<&String> {
        self.last()
            .and_then(|context| context.fixed_list_identifiers.get(index))
    }
}

#[derive(Debug)]
struct MatchContext {
    // Are we matching a fixed identifier?
    fixed_ident: Option<String>,

    // Are we matching a fixed list containing only identifiers?
    fixed_list: bool,

    // List of identifiers that are matched in the fixed list.
    fixed_list_identifiers: Vec<String>,
}

fn match_args_have_completions(arms: &[hir::MatchArm]) -> bool {
    arms.iter().any(|arm| match &arm.expr.kind {
        hir::ExprKind::Block(block) => block.has_completion(),
        _ => true,
    })
}

impl CodegenJS {
    fn generate_match_arm_expression(&mut self, expression: &hir::Expr) {
        if let hir::ExprKind::Block(_) = &expression.kind {
            self.generate_expr(expression, None);
        } else if self.current_completion_variable() == Some("return") {
            if expression.is_tail_call() {
                self.generate_expr(expression, None);
            } else {
                self.push_indent();
                self.push_str("return ");
                self.generate_expr(expression, None);
                self.push_str(";\n");
            }
        } else {
            self.push_indent();
            let completion_tmp_var = self.current_completion_variable().unwrap();
            self.push_str(&format!("{completion_tmp_var} = "));
            self.generate_expr(expression, None);
            self.push_str(";\n");
        }
    }

    fn get_pat_identifiers(pattern: &hir::Pat) -> Vec<String> {
        match &pattern.kind {
            hir::PatKind::Identifier(_, ident_pattern) => {
                vec![ident_pattern.name.to_string()]
            }
            hir::PatKind::Enum(_path, patterns) => {
                let mut bindings = Vec::new();
                for (_ident, pattern) in patterns {
                    bindings.extend(Self::get_pat_identifiers(pattern));
                }
                bindings
            }
            hir::PatKind::Literal(_) => Vec::new(),
            hir::PatKind::List(patterns) => {
                let mut bindings = Vec::new();
                for pattern in patterns {
                    bindings.extend(Self::get_pat_identifiers(pattern));
                }
                bindings
            }
            hir::PatKind::Rest(pattern) => Self::get_pat_identifiers(pattern),
            hir::PatKind::Wildcard => Vec::new(),
        }
    }

    fn generate_pat_condition(&mut self, pat: &hir::Pat, parent_js_expr: &str) {
        match &pat.kind {
            hir::PatKind::Identifier(_, ident_pattern) => {
                // If you ended up here due to a `somevar = somevar`, we shouldn't have rendered
                // the match arm condition in the first place.
                let binding_name = self
                    .current_scope()
                    .resolve_variable(ident_pattern.as_str())
                    .unwrap();

                let matching_value = if let Some(ident) = self.match_context_stack.get_fixed_ident()
                {
                    ident
                } else {
                    parent_js_expr
                };

                self.push_str(&format!("({} = {}, true)", binding_name, parent_js_expr));
            }
            hir::PatKind::Enum(path, patterns) => {
                let enum_variant_name = path.segments.last().unwrap();

                self.push_str(&format!(
                    "{}.tag === \"{}\"",
                    parent_js_expr,
                    enum_variant_name.ident.as_str()
                ));

                for (ident, pattern) in patterns.iter().filter(|(_, pat)| !pat.is_wildcard()) {
                    self.push_str(" && ");

                    let parent_js_expr = if ident.as_str().chars().all(char::is_numeric) {
                        format!("{}[{}]", parent_js_expr, ident)
                    } else {
                        format!("{}.{}", parent_js_expr, ident.as_str())
                    };

                    self.generate_pat_condition(pattern, &parent_js_expr);
                }
            }
            hir::PatKind::Literal(literal) => {
                self.push_str(parent_js_expr);
                self.push_str(" === ");
                self.generate_literal(literal)
            }
            hir::PatKind::List(patterns) if patterns.is_empty() => {
                self.push_str(parent_js_expr);
                self.push_str(".length === 0");
            }
            hir::PatKind::List(patterns) => {
                if self.match_context_stack.fixed_list() {
                    let mut push_and = false;
                    for (i, pattern) in patterns.iter().enumerate() {
                        if pattern.is_wildcard() || pattern.is_identifier() {
                            continue;
                        }

                        if push_and {
                            self.push_str(" && ");
                        } else {
                            push_and = true;
                        }

                        let parent_js_expr = self
                            .match_context_stack
                            .get_fixed_list_identifier(i)
                            .unwrap()
                            .clone();

                        self.generate_pat_condition(pattern, &parent_js_expr);
                    }
                    return;
                }

                self.push_str(parent_js_expr);
                self.push_str(".length >= ");
                self.push_str(
                    &patterns
                        .iter()
                        .filter(|pat| !matches!(pat.kind, hir::PatKind::Rest(_)))
                        .count()
                        .to_string(),
                );

                for (i, pattern) in patterns.iter().enumerate() {
                    if pattern.is_wildcard() {
                        continue;
                    }

                    self.push_str(" && ");
                    // This feels awkward, could this be handled when we match the Rest pattern
                    // below?
                    let parent_js_expr = if matches!(pattern.kind, hir::PatKind::Rest(_)) {
                        format!("{}.slice({})", parent_js_expr, i)
                    } else {
                        format!("{}[{}]", parent_js_expr, i)
                    };
                    self.generate_pat_condition(pattern, &parent_js_expr);
                }
            }
            hir::PatKind::Rest(pattern) => {
                self.generate_pat_condition(pattern, parent_js_expr);
            }
            hir::PatKind::Wildcard => {}
        }
    }

    pub(crate) fn generate_match_expression(&mut self, expr: &hir::Expr, arms: &[hir::MatchArm]) {
        // TODO: A lot here is copied from the if statement generator.
        let mut lhs = self.replace_statement_buffer(String::new());
        let has_block_completions = match_args_have_completions(arms);
        let mut has_let = false;
        if has_block_completions {
            // TODO: We could probably reuse existing completion vars here.
            if let Some("return") = self.current_completion_variable() {
                self.push_completion_variable(Some("return".to_string()));
                lhs = self.replace_statement_buffer_with_empty_string();
            } else {
                let completion_tmp_var = self.current_scope().declare_tmp_variable();
                self.push_indent();
                self.push_str("let ");
                self.push_str(&completion_tmp_var);
                self.push_completion_variable(Some(completion_tmp_var));
                has_let = true;
            }
        } else {
            self.push_completion_variable(None);
        }

        let matching_value_is_fixed_list = if let hir::ExprKind::List(exprs) = &expr.kind {
            exprs
                .iter()
                .all(|expr| matches!(&expr.kind, hir::ExprKind::Path(_)))
        } else {
            false
        };
        let matching_value_fixed_ident = if let hir::ExprKind::Path(path) = &expr.kind {
            Some(path.join("::"))
        } else {
            None
        };

        let fixed_list_identifiers = if matching_value_is_fixed_list {
            match &expr.kind {
                hir::ExprKind::List(exprs) => exprs
                    .iter()
                    .map(|expr| {
                        if let hir::ExprKind::Path(path) = &expr.kind {
                            path.join("::")
                        } else {
                            unreachable!()
                        }
                    })
                    .collect(),
                hir::ExprKind::Path(path) => vec![path.join("::")],
                _ => unreachable!(),
            }
        } else {
            vec![]
        };

        self.match_context_stack.push(MatchContext {
            fixed_ident: matching_value_fixed_ident,
            fixed_list: matching_value_is_fixed_list,
            fixed_list_identifiers,
        });

        let fixed_match_arms_match_pattern = arms
            .iter()
            .all(|arm| expr_idents_match_pat_idents(expr, &arm.pat));

        let match_value_binding = if let hir::ExprKind::Path(path) = &expr.kind {
            path.segments.last().unwrap().ident.to_string()
        } else if matching_value_is_fixed_list {
            "".to_string()
        } else {
            let match_value_binding = self.current_scope().declare_tmp_variable();

            if has_let {
                self.push_char(',');
                self.push_str(&match_value_binding);
                self.push_str(" = ");
                self.generate_expr(expr, None);
            } else {
                self.push_let_declaration_to_expr(&match_value_binding, expr);
                has_let = true;
            }

            match_value_binding
        };

        let has_let_guard = arms.iter().any(|arm| {
            if let Some(guard) = &arm.guard {
                if let hir::ExprKind::Let(..) = &guard.kind {
                    return true;
                }
            }
            false
        });

        let let_guard_var = if has_let_guard {
            if !has_let {
                self.push_indent();
                self.push_str("let ");
                has_let = true;
            } else {
                self.push_char(',');
            }
            let binding = self.current_scope().declare_tmp_variable();
            self.push_str(&binding);
            binding
        } else {
            String::new()
        };

        let mut unique = HashSet::new();
        // There's optimization opportunities in case the match expression is a
        // list of identifiers, then we can just alias the identifiers within the arms. This
        // case should be pretty common in function overloads.
        let all_pat_identifiers = arms.iter().flat_map(|arm| {
            let mut idents = vec![];
            idents.extend(Self::get_pat_identifiers(&arm.pat));
            if let Some(guard) = &arm.guard {
                if let hir::ExprKind::Let(pat, _) = &guard.kind {
                    idents.extend(Self::get_pat_identifiers(pat));
                }
            }
            idents
        });
        for binding in all_pat_identifiers {
            if unique.insert(binding.clone()) {
                let binding = self.current_scope().declare_variable(&binding);
                if !matching_value_is_fixed_list && !fixed_match_arms_match_pattern {
                    // TODO: Ugly workaround, as we always push a comma when generating pat identifiers
                    //       bindings.
                    if has_let {
                        self.push_char(',');
                    } else {
                        self.push_indent();
                        self.push_str("let ");
                        has_let = true;
                    }
                    self.push_str(&binding);
                }
            }
        }

        if has_let {
            self.push_char(';');
        } else {
            self.push_indent();
        }

        for (
            i,
            hir::MatchArm {
                pat,
                guard,
                expr: arm_expr,
                leading_comments,
                trailing_comments,
            },
        ) in arms.iter().enumerate()
        {
            let no_cond_need = !(pat.is_wildcard()
                || (matching_value_is_fixed_list
                    // TODO: We need to know this up front, to declare variables if this is false.
                    && expr_idents_match_pat_idents(expr, pat)));
            let need_cond = guard.is_some();

            if no_cond_need || need_cond {
                self.push_str("if (");
                self.generate_pat_condition(pat, &match_value_binding);
                if !match_value_binding.is_empty() && guard.is_some() {
                    self.push_str(" && ");
                }
                if let Some(guard) = guard {
                    self.generate_match_arm_guard(guard, &let_guard_var);
                }
                self.push_str(") ");
            }

            self.push_str("{\n");
            self.inc_indent();
            self.generate_comments(leading_comments);
            self.push_context(BlockContext::Expression);
            self.generate_match_arm_expression(arm_expr);
            self.pop_context();
            self.generate_comments(trailing_comments);
            self.dec_indent();
            self.push_indent();

            if i == arms.len() - 1 {
                self.push_char('}');
            } else {
                self.push_str("} else ");
            }
        }

        if has_block_completions && self.current_completion_variable() != Some("return") {
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
        self.match_context_stack.pop();
    }

    fn generate_match_arm_guard(&mut self, guard: &hir::Expr, let_guard_var: &str) {
        // If the guard is a let expression, we special case this here, normal if let expressions
        // are handled in the lowering process and will be transformed to a match expression.
        if let hir::ExprKind::Let(pat, expr) = &guard.kind {
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

fn expr_idents_match_pat_idents(expr: &hir::Expr, pat: &hir::Pat) -> bool {
    if let hir::PatKind::Identifier(_, ident) = &pat.kind {
        if let hir::ExprKind::Path(path) = &expr.kind {
            return ident.as_str() == path.join("::");
        }
    }

    if let hir::PatKind::List(list_pats) = &pat.kind {
        if let hir::ExprKind::List(list_exprs) = &expr.kind {
            return list_pats.len() == list_exprs.len()
                && list_pats
                    .iter()
                    .all(|pat| pat.is_identifier() || pat.is_wildcard())
                && list_exprs
                    .iter()
                    .all(|expr| matches!(&expr.kind, hir::ExprKind::Path(_)))
                && list_pats.iter().zip(list_exprs.iter()).all(|(pat, expr)| {
                    if pat.is_wildcard() {
                        return true;
                    }

                    let pat_ident = if let hir::PatKind::Identifier(_, ident) = &pat.kind {
                        ident.as_str()
                    } else {
                        unreachable!()
                    };
                    let expr_ident = if let hir::ExprKind::Path(path) = &expr.kind {
                        path.join("::")
                    } else {
                        unreachable!()
                    };

                    pat_ident == expr_ident
                });
        }
    }

    false
}
