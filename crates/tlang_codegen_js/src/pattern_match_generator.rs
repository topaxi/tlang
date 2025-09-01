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
        if !block.stmts.is_empty() {
            // TODO: Handle statements in match arms properly
            // For now, avoid generate_block_expression which can add };
            self.generate_statements(&block.stmts);
        }

        if let Some(expr) = &block.expr {
            // Handle break and continue expressions specially - they cannot be assigned or returned
            match &expr.kind {
                hir::ExprKind::Break(_) => {
                    self.push_indent();
                    self.generate_expr(expr, None); // Main generator now handles loop context
                    self.push_str("\n");
                }
                hir::ExprKind::Continue => {
                    self.push_indent();
                    self.generate_expr(expr, None);
                    self.push_str(";\n"); // Continue needs semicolon
                }
                _ => {
                    if self.current_completion_variable() == Some("return") {
                        if expr.is_tail_call() {
                            self.generate_expr(expr, None);
                        } else {
                            self.push_indent();
                            self.push_str("return ");
                            self.generate_expr(expr, None);
                            self.push_str(";\n");
                        }
                    } else {
                        self.push_indent();
                        let completion_var =
                            self.current_completion_variable().unwrap().to_string();
                        self.push_str(&completion_var);
                        self.push_str(" = ");
                        self.generate_expr(expr, None);
                        self.push_str(";\n");
                    }
                }
            }
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
                // If you ended up here due to a `somevar = somevar`, we shouldn't have rendered
                // the match arm condition in the first place.
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
                    .resolve_variable(&path.to_string())
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
                self.generate_literal(literal);
            }
            hir::PatKind::List(patterns) if patterns.is_empty() => {
                self.push_str(parent_js_expr);
                self.push_str(".length === 0");
            }
            hir::PatKind::List(patterns) => {
                if root_pat && self.match_context_stack.fixed_list() {
                    let mut push_and = false;
                    for (i, pattern) in patterns.iter().enumerate() {
                        if pattern.is_wildcard() || pattern.is_ident() {
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
                    // This feels awkward, could this be handled when we match the Rest pattern
                    // below?
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

    fn setup_match_completion_variables(&mut self, arms: &[hir::MatchArm]) -> (String, bool) {
        let mut lhs = self.replace_statement_buffer(String::new());
        let has_block_completions =
            self.current_context() == BlockContext::Expression && match_args_have_completions(arms);
        let is_expression_context = self.current_context() == BlockContext::Expression;
        let mut has_let = false;

        if has_block_completions || is_expression_context {
            if let Some("return") = self.current_completion_variable() {
                self.push_completion_variable(Some("return"));
                lhs = self.replace_statement_buffer_with_empty_string();
            } else if let Some(existing_var) = self.current_completion_variable().map(String::from)
            {
                // Reuse existing completion variable if available
                self.push_completion_variable(Some(&existing_var));
            } else {
                let completion_tmp_var = self.current_scope().declare_tmp_variable();
                self.push_indent();
                self.push_str("let ");
                self.push_str(&completion_tmp_var);
                self.push_completion_variable(Some(&completion_tmp_var));
                has_let = true;
            }
        } else if match_args_have_completions(arms) {
            // Even in statement context, if match arms have completions, we need a temp variable
            let completion_tmp_var = self.current_scope().declare_tmp_variable();
            self.push_indent();
            self.push_str("let ");
            self.push_str(&completion_tmp_var);
            self.push_completion_variable(Some(&completion_tmp_var));
            has_let = true;
        } else {
            self.push_completion_variable(None);
        }

        (lhs, has_let)
    }

    fn setup_match_context(&mut self, expr: &hir::Expr) {
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
                    ));
            }
            _ => {
                self.match_context_stack.push(MatchContext::Dynamic);
            }
        }
    }

    fn setup_match_value_binding(&mut self, expr: &hir::Expr, has_let: &mut bool) -> String {
        match self.match_context_stack.last() {
            Some(MatchContext::Identifier(identifier)) => identifier.clone(),
            Some(MatchContext::ListOfIdentifiers(_)) => String::new(), // ohoh...
            _ => {
                let match_value_binding = self.current_scope().declare_tmp_variable();

                if *has_let {
                    self.push_char(';');
                    self.push_newline();
                    self.push_indent();
                    self.push_str("let ");
                    self.push_str(&match_value_binding);
                    self.push_str(" = ");
                    self.generate_expr(expr, None);
                } else {
                    self.push_let_declaration_to_expr(&match_value_binding, expr);
                    *has_let = true;
                }

                match_value_binding
            }
        }
    }

    fn setup_let_guard_variable(&mut self, arms: &[hir::MatchArm], has_let: &mut bool) -> String {
        if arms.iter().any(|arm| arm.has_let_guard()) {
            if *has_let {
                self.push_char(';');
                self.push_newline();
                self.push_indent();
                self.push_str("let ");
            } else {
                self.push_indent();
                self.push_str("let ");
                *has_let = true;
            }
            let binding = self.current_scope().declare_tmp_variable();
            self.push_str(&binding);
            binding
        } else {
            String::new()
        }
    }

    fn setup_pattern_identifiers(&mut self, arms: &[hir::MatchArm], has_let: &mut bool) {
        let mut unique = HashSet::new();
        // There's optimization opportunities in case the match expression is a
        // list of identifiers, then we can just alias the identifiers within the arms. This
        // case should be pretty common in function overloads.
        let mut all_pat_identifiers = arms
            .iter()
            .flat_map(|arm| {
                let mut idents = Self::get_pat_identifiers(&arm.pat);
                if let Some(guard) = &arm.guard
                    && let hir::ExprKind::Let(pat, _) = &guard.kind
                {
                    idents.extend(Self::get_pat_identifiers(pat));
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
                // Generate separate let statements instead of comma-separated declarations
                // to avoid JavaScript syntax errors with different assignment values
                if *has_let {
                    self.push_char(';');
                    self.push_newline();
                }
                self.push_indent();
                self.push_str("let ");
                self.push_str(&binding);
                *has_let = true;
            }
        }
    }

    fn generate_match_arms(
        &mut self,
        expr: &hir::Expr,
        arms: &[hir::MatchArm],
        match_value_binding: &str,
        let_guard_var: &str,
    ) {
        for (
            i,
            hir::MatchArm {
                pat,
                guard,
                block,
                leading_comments,
                trailing_comments,
                ..
            },
        ) in arms.iter().enumerate()
        {
            let no_cond_need = pat.is_wildcard() || expr_idents_match_pat_idents(expr, pat);
            let need_cond = guard.is_some() || pat.is_empty_list();

            if !no_cond_need || need_cond {
                self.push_str("if (");
                self.generate_pat_condition(pat, true, match_value_binding);
                if !pat.is_wildcard() && guard.is_some() && !is_fixed_list_pattern(pat) {
                    self.push_str(" && ");
                }
                if let Some(guard) = guard {
                    self.generate_match_arm_guard(guard, let_guard_var);
                }
                self.push_str(") ");
            }

            self.push_str("{\n");
            self.inc_indent();
            self.generate_comments(leading_comments);
            self.generate_match_arm_block(block);
            self.generate_comments(trailing_comments);
            self.dec_indent();
            self.push_indent();

            if i == arms.len() - 1 {
                self.push_char('}');
            } else {
                self.push_str("} else ");
            }
        }
    }

    fn finalize_match_expression(&mut self, lhs: &str, has_block_completions: bool) {
        // Add a semicolon after the if-else statement when we have a completion variable
        if self.current_completion_variable().is_some()
            && self.current_completion_variable() != Some("return")
            && lhs.is_empty()
            && !self.is_in_loop_context()
        {
            // Don't add semicolon in loop contexts
            // Add a semicolon after the if-else statement - SIMPLE VERSION
            self.push_char(';');
            // DEBUG: This should NOT output the completion variable
        } else if has_block_completions && self.current_completion_variable() != Some("return") {
            self.push_newline();
            // If we have an lhs, put the completion var as the rhs of the lhs.
            if !lhs.is_empty() {
                self.push_str(lhs);
                self.push_current_completion_variable();
            }
        } else {
            // In cases where the match expression is used in assignment contexts but doesn't
            // go through the normal completion variable assignment (lhs is empty, has_block_completions is false),
            // we still need to handle the case where temp variables were created during match arm evaluation.
            // If the completion variable differs from the latest temp variable, assign the latest to the completion variable.
            if self.is_in_loop_context() {
                if let Some(current_completion) = self.current_completion_variable().map(String::from) {
                    if current_completion.starts_with("$tmp$") {
                        let latest_temp_var = self.current_scope().get_latest_temp_variable();
                        if latest_temp_var != current_completion && latest_temp_var.starts_with("$tmp$") {
                            // DEBUG: Output what's happening
                            self.push_str("/* DEBUG: completion=");
                            self.push_str(&current_completion);
                            self.push_str(", latest=");
                            self.push_str(&latest_temp_var);
                            self.push_str(" */");
                            self.push_newline();
                            
                            // Generate an assignment to ensure the completion variable has the correct value
                            self.push_newline();
                            self.push_indent();
                            self.push_str(&current_completion);
                            self.push_str(" = ");
                            self.push_str(&latest_temp_var);
                            self.push_char(';');
                            self.push_newline();
                        }
                    }
                }
            }
        }
        self.pop_completion_variable();
        self.match_context_stack.pop();
    }

    pub(crate) fn generate_match_expression(&mut self, expr: &hir::Expr, arms: &[hir::MatchArm]) {
        self.generate_match_expression_with_completion_var(expr, arms, None);
    }

    pub(crate) fn generate_match_expression_with_completion_var(
        &mut self,
        expr: &hir::Expr,
        arms: &[hir::MatchArm],
        completion_var: Option<&str>,
    ) {
        // TODO: A lot here is copied from the if statement generator.
        let (lhs, mut has_let) = if let Some(var) = completion_var {
            // Use the provided completion variable instead of creating a new one
            self.push_completion_variable(Some(var));
            // Make sure the variable is registered in the scope so subsequent declare_tmp_variable calls work correctly
            self.current_scope().declare_variable_alias(var, var);

            if var == "accumulator$$" {
                // Don't declare accumulator$$ since it's already declared by the for loop
                (String::new(), false)
            } else {
                // Declare the temp variable at the beginning of the expression
                self.push_str("let ");
                self.push_str(var);
                // Don't replace the statement buffer since we want to keep the "let" declaration
                (String::new(), true)
            }
        } else {
            self.setup_match_completion_variables(arms)
        };
        let has_block_completions =
            self.current_context() == BlockContext::Expression && match_args_have_completions(arms);

        self.setup_match_context(expr);

        let match_value_binding = self.setup_match_value_binding(expr, &mut has_let);

        let let_guard_var = self.setup_let_guard_variable(arms, &mut has_let);

        self.setup_pattern_identifiers(arms, &mut has_let);

        if has_let {
            self.push_char(';');
        } else {
            self.push_indent();
        }

        self.generate_match_arms(expr, arms, &match_value_binding, &let_guard_var);

        self.finalize_match_expression(&lhs, has_block_completions);
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
            self.generate_pat_condition(pat, false, let_guard_var);
        } else {
            self.generate_expr(guard, None);
        }
    }
}

fn expr_idents_match_pat_idents(expr: &hir::Expr, pat: &hir::Pat) -> bool {
    if let hir::PatKind::Identifier(_, ident) = &pat.kind
        && let hir::ExprKind::Path(path) = &expr.kind
    {
        return ident.as_str() == path.to_string();
    }

    if let hir::PatKind::List(list_pats) = &pat.kind
        && let hir::ExprKind::List(list_exprs) = &expr.kind
    {
        return list_pats.len() == list_exprs.len()
            && list_pats
                .iter()
                .all(|pat| pat.is_ident() || pat.is_wildcard())
            && list_exprs
                .iter()
                .all(|expr| matches!(&expr.kind, hir::ExprKind::Path(_)))
            && list_pats.iter().zip(list_exprs.iter()).all(|(pat, expr)| {
                if pat.is_wildcard() {
                    return true;
                }

                pat.ident().unwrap().as_str() == expr.path().unwrap().to_string()
            });
    }

    false
}

fn is_fixed_list_pattern(pat: &hir::Pat) -> bool {
    match &pat.kind {
        hir::PatKind::List(pats) => {
            !pats.is_empty() && pats.iter().all(|pat| pat.is_wildcard() || pat.is_ident())
        }
        _ => false,
    }
}
