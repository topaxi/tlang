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

    fn fixed_list(&self) -> bool {
        self.last()
            .is_some_and(|context| matches!(context, MatchContext::ListOfIdentifiers(_)))
    }

    fn get_fixed_list_identifier(&self, index: usize) -> Option<&String> {
        self.last().and_then(|context| {
            if let MatchContext::ListOfIdentifiers(identifiers) = context {
                identifiers.get(index)
            } else {
                None
            }
        })
    }
}

#[derive(Debug)]
enum MatchContext {
    Dynamic,
    Identifier(String),
    ListOfIdentifiers(Vec<String>),
}

fn match_args_have_completions(arms: &[hir::MatchArm]) -> bool {
    arms.iter().any(|arm| arm.block.has_completion())
}

impl CodegenJS {
    fn generate_match_arm_block(&mut self, block: &hir::Block) {
        self.flush_statement_buffer();
        if !block.stmts.is_empty() || block.expr.is_none() {
            self.generate_block_statement(block);
        } else if let Some(expr) = &block.expr {
            if self.is_self_referencing_tail_call(expr) {
                self.generate_return_statement(Some(expr));
            } else {
                self.push_indent();
                self.push_str("return ");
                self.generate_expr(expr, None, BlockContext::Expression);
                self.push_str(";\n");
            }
        } else {
            self.generate_block_statement(block);
        }
    }

    fn get_pat_identifiers(pattern: &hir::Pat) -> Vec<String> {
        match &pattern.kind {
            hir::PatKind::Identifier(_, ident_pattern) => {
                vec![ident_pattern.to_string()]
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

    fn generate_pat_condition(&mut self, pat: &hir::Pat, root_pat: bool, parent_js_expr: &str) {
        match &pat.kind {
            hir::PatKind::Identifier(_, ident_pattern) => {
                let binding_name = self
                    .current_scope()
                    .resolve_variable(ident_pattern.as_str())
                    .unwrap();

                self.push_char('(');
                self.push_str(&binding_name);
                self.push_str(" = ");
                self.push_str(parent_js_expr);
                self.push_str(", true)");
            }
            hir::PatKind::Enum(path, patterns) => {
                self.push_str(parent_js_expr);
                self.push_str(".tag === ");
                let resolved = self
                    .current_scope()
                    .resolve_variable(path.join("::").as_str())
                    .unwrap_or_else(|| path.join("."));
                self.push_str(&resolved);

                for (ident, pattern) in patterns.iter().filter(|(_, pat)| !pat.is_wildcard()) {
                    self.push_str(" && ");

                    let parent_js_expr = if ident.as_str().chars().all(char::is_numeric) {
                        parent_js_expr.to_string() + "[" + ident.as_str() + "]"
                    } else {
                        parent_js_expr.to_string() + "." + ident.as_str()
                    };

                    self.generate_pat_condition(pattern, false, &parent_js_expr);
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
                if root_pat && self.match_context_stack.fixed_list() {
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

                        self.generate_pat_condition(pattern, false, &parent_js_expr);
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
                    let parent_js_expr = if matches!(pattern.kind, hir::PatKind::Rest(_)) {
                        parent_js_expr.to_string() + ".slice(" + &i.to_string() + ")"
                    } else {
                        parent_js_expr.to_string() + "[" + &i.to_string() + "]"
                    };
                    self.generate_pat_condition(pattern, false, &parent_js_expr);
                }
            }
            hir::PatKind::Rest(pattern) => {
                self.generate_pat_condition(pattern, false, parent_js_expr);
            }
            hir::PatKind::Wildcard => {}
        }
    }

    pub(crate) fn generate_match_expression(&mut self, expr: &hir::Expr, arms: &[hir::MatchArm]) {
        let original_buffer = self.replace_statement_buffer_with_empty_string();

        match &expr.kind {
            hir::ExprKind::Path(path) => {
                let match_context_identifier = self
                    .current_scope()
                    .resolve_variable(path.first_ident().as_str())
                    .expect("Match expression identifier not found");

                self.match_context_stack
                    .push(MatchContext::Identifier(match_context_identifier));
            }
            hir::ExprKind::List(exprs)
                if !exprs.is_empty() && exprs.iter().all(|expr| expr.is_path()) =>
            {
                self.match_context_stack
                    .push(MatchContext::ListOfIdentifiers(
                        exprs
                            .iter()
                            .map(|expr| expr.path().unwrap().first_ident().to_string())
                            .collect(),
                    ))
            }
            _ => {
                self.match_context_stack.push(MatchContext::Dynamic);
            }
        }

        let match_value_binding = match self.match_context_stack.last() {
            Some(MatchContext::Identifier(identifier)) => identifier.clone(),
            Some(MatchContext::ListOfIdentifiers(_)) => "".to_string(),
            _ => {
                let match_value_binding = self.current_scope().declare_tmp_variable();
                self.push_let_declaration_to_expr(&match_value_binding, expr);
                match_value_binding
            }
        };

        let let_guard_var = if arms.iter().any(|arm| arm.has_let_guard()) {
            let binding = self.current_scope().declare_tmp_variable();
            self.push_str(&binding);
            binding
        } else {
            String::new()
        };

        let mut unique = HashSet::new();
        let mut all_pat_identifiers = arms
            .iter()
            .flat_map(|arm| {
                let mut idents = Self::get_pat_identifiers(&arm.pat);
                if let Some(guard) = &arm.guard {
                    if let hir::ExprKind::Let(pat, _) = &guard.kind {
                        idents.extend(Self::get_pat_identifiers(pat));
                    }
                }
                idents
            })
            .collect::<Vec<_>>();

        match self.match_context_stack.last() {
            Some(MatchContext::Identifier(ident)) => {
                all_pat_identifiers.retain(|pat_ident| pat_ident != ident);
            }
            Some(MatchContext::ListOfIdentifiers(idents)) => {
                all_pat_identifiers.retain(|pat_ident| !idents.contains(pat_ident));
            }
            _ => {}
        }

        for binding in all_pat_identifiers {
            if unique.insert(binding.clone()) {
                let binding = self.current_scope().declare_variable(&binding);
                self.push_str(&binding);
            }
        }

        self.push_str("}");

        // Restore original buffer content if needed (though likely empty now)
        let current_buffer = self.replace_statement_buffer(original_buffer);

        // The IIFE approach directly appends to the main buffer.
        // If this old function were still used, this part would need rethinking.
        // TODO: This function (`generate_match_expression`) is likely superseded by the
        //       IIFE logic (`generate_match_return`/`generate_match_statement`)
        //       and its buffer/output handling is probably incorrect now.
        //       Verify if it's still used and refactor or remove.
        self.push_str(&current_buffer); // Append the generated match if/else chain to current buffer

        // Old logic tried to push the final result (temp var or undefined).
        // IIFE handles this via its return value.

        self.match_context_stack.pop();
    }

    fn generate_match_arm_guard(&mut self, guard: &hir::Expr, let_guard_var: &str) {
        if let hir::ExprKind::Let(pat, expr) = &guard.kind {
            self.push_char('(');
            self.push_str(let_guard_var);
            self.push_str(" = ");
            self.generate_expr(expr, None, BlockContext::Expression);
            self.push_str(", true) && ");
            self.generate_pat_condition(pat, false, let_guard_var);
        } else {
            self.generate_expr(guard, None, BlockContext::Expression);
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

fn is_fixed_list_pattern(pat: &hir::Pat) -> bool {
    match &pat.kind {
        hir::PatKind::List(pats) => {
            !pats.is_empty()
                && pats
                    .iter()
                    .all(|pat| pat.is_wildcard() || pat.is_identifier())
        }
        _ => false,
    }
}
