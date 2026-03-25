use std::collections::HashSet;

use oxc_ast::NONE;
use oxc_ast::ast::*;
use oxc_span::SPAN;
use tlang_hir as hir;

use crate::generator::InnerCodegen;
use tlang_ast::node::Ident;

/// Represents an access path into a match subject value.
/// Used to build OXC expressions for nested pattern access without cloning expressions.
#[derive(Clone, Debug)]
enum AccessPath {
    Ident(String),
    Field(Box<AccessPath>, String),
    Index(Box<AccessPath>, usize),
    Slice(Box<AccessPath>, usize),
}

impl<'a> InnerCodegen<'a> {
    fn access_path_to_expr(&self, path: &AccessPath) -> Expression<'a> {
        match path {
            AccessPath::Ident(name) => self.ident_expr(name),
            AccessPath::Field(base, field) => {
                let obj = self.access_path_to_expr(base);
                self.static_member_expr(obj, field)
            }
            AccessPath::Index(base, idx) => {
                let obj = self.access_path_to_expr(base);
                let index = self.num_expr(*idx as f64);
                self.computed_member_expr(obj, index)
            }
            AccessPath::Slice(base, start) => {
                let obj = self.access_path_to_expr(base);
                let start_expr = self.num_expr(*start as f64);
                self.call_expr(
                    self.static_member_expr(obj, "slice"),
                    vec![Argument::from(start_expr)],
                )
            }
        }
    }

    pub fn generate_match_stmts(
        &mut self,
        expr: &hir::Expr,
        arms: &[hir::MatchArm],
    ) -> Vec<Statement<'a>> {
        let mut result = Vec::new();
        let mut declarators = Vec::new();

        // 1. Determine match value binding and fixed list context
        let (match_value_name, fixed_list_idents) = match &expr.kind {
            hir::ExprKind::Path(path) => {
                let name = self
                    .current_scope()
                    .resolve_variable(path.first_ident().as_str())
                    .expect("Match expression identifier not found");
                (name, None)
            }
            hir::ExprKind::List(exprs)
                if !exprs.is_empty() && exprs.iter().all(|e| e.is_path()) =>
            {
                let mut idents = Vec::with_capacity(exprs.len());
                for e in exprs {
                    let ident = e.path().unwrap().first_ident();
                    let name = ident.as_str();
                    let span = ident.span;
                    match self.current_scope().resolve_variable(name) {
                        Some(resolved) => idents.push(resolved),
                        None => {
                            self.errors
                                .push(crate::error::CodegenError::unresolved_identifier(
                                    name, span,
                                ));
                            idents.push(name.to_string());
                        }
                    }
                }
                (String::new(), Some(idents))
            }
            _ => {
                let tmp = self.current_scope().declare_tmp_variable();
                let val = self.generate_expr(expr);
                declarators.push(self.ast.variable_declarator(
                    SPAN,
                    VariableDeclarationKind::Let,
                    self.binding_pattern_ident(&tmp),
                    NONE,
                    Some(val),
                    false,
                ));
                (tmp, None)
            }
        };

        // 2 & 3. Declare let guard variable and all pattern identifiers
        let let_guard_var = self.declare_match_variables(
            arms,
            &match_value_name,
            &fixed_list_idents,
            &mut declarators,
        );

        // Emit combined let declaration
        if !declarators.is_empty() {
            result.push(Statement::VariableDeclaration(
                self.ast.alloc_variable_declaration(
                    SPAN,
                    VariableDeclarationKind::Let,
                    self.ast.vec_from_iter(declarators),
                    false,
                ),
            ));
        }

        // 4. Generate if/else chain
        let root = AccessPath::Ident(match_value_name);
        if let Some(stmt) =
            self.generate_match_arms_chain(expr, arms, &root, &fixed_list_idents, &let_guard_var)
        {
            result.push(stmt);
        }

        result
    }

    fn declare_match_variables(
        &mut self,
        arms: &[hir::MatchArm],
        match_value_name: &str,
        fixed_list_idents: &Option<Vec<String>>,
        declarators: &mut Vec<VariableDeclarator<'a>>,
    ) -> String {
        let let_guard_var = if arms.iter().any(|arm| arm.has_let_guard()) {
            let var = self.current_scope().declare_tmp_variable();
            declarators.push(self.ast.variable_declarator(
                SPAN,
                VariableDeclarationKind::Let,
                self.binding_pattern_ident(&var),
                NONE,
                None,
                false,
            ));
            var
        } else {
            String::new()
        };

        let mut seen = HashSet::new();
        let filtered: Vec<String> = arms
            .iter()
            .flat_map(|arm| {
                let mut idents = get_pat_identifiers(&arm.pat);
                if let Some(guard) = &arm.guard
                    && let hir::ExprKind::Let(pat, _) = &guard.kind
                {
                    idents.extend(get_pat_identifiers(pat));
                }
                idents
            })
            .filter(|ident| match fixed_list_idents {
                Some(idents) => !idents.contains(ident),
                None => ident != match_value_name,
            })
            .filter(|ident| seen.insert(ident.clone()))
            .collect();

        for ident in &filtered {
            let binding = self.current_scope().declare_variable(ident);
            declarators.push(self.ast.variable_declarator(
                SPAN,
                VariableDeclarationKind::Let,
                self.binding_pattern_ident(&binding),
                NONE,
                None,
                false,
            ));
        }

        let_guard_var
    }

    fn generate_match_arms_chain(
        &mut self,
        expr: &hir::Expr,
        arms: &[hir::MatchArm],
        root: &AccessPath,
        fixed_list_idents: &Option<Vec<String>>,
        let_guard_var: &str,
    ) -> Option<Statement<'a>> {
        if arms.is_empty() {
            return None;
        }

        let arm = &arms[0];
        let remaining = &arms[1..];

        let no_cond_need = arm.pat.is_wildcard() || expr_idents_match_pat_idents(expr, &arm.pat);
        let need_cond = arm.guard.is_some() || arm.pat.is_empty_list();

        let mut body_stmts = self.generate_block_stmts_scoped(&arm.block);
        // Attach match arm comments to first body statement
        if !arm.leading_comments.is_empty()
            && let Some(first) = body_stmts.first_mut()
        {
            self.attach_leading_comments(first, &arm.leading_comments);
        }
        let body = self.block_stmt(body_stmts);

        if !no_cond_need || need_cond {
            let mut conditions: Vec<Expression<'a>> = Vec::new();

            if let Some(pat_cond) =
                self.generate_pat_condition(&arm.pat, true, root, fixed_list_idents)
            {
                conditions.push(pat_cond);
            }

            if let Some(guard) = &arm.guard {
                let guard_cond = self.generate_match_arm_guard_expr(guard, let_guard_var);
                conditions.push(guard_cond);
            }

            let test = if conditions.is_empty() {
                self.bool_expr(true)
            } else {
                let mut combined = conditions.remove(0);
                for cond in conditions {
                    combined =
                        self.ast
                            .expression_logical(SPAN, combined, LogicalOperator::And, cond);
                }
                combined
            };

            let alternate = self.generate_match_arms_chain(
                expr,
                remaining,
                root,
                fixed_list_idents,
                let_guard_var,
            );

            Some(self.ast.statement_if(SPAN, test, body, alternate))
        } else {
            // No condition needed (wildcard or matching identifiers)
            Some(body)
        }
    }

    fn generate_pat_condition(
        &mut self,
        pat: &hir::Pat,
        root_pat: bool,
        access: &AccessPath,
        fixed_list_idents: &Option<Vec<String>>,
    ) -> Option<Expression<'a>> {
        match &pat.kind {
            hir::PatKind::Identifier(_, ident_pattern) => {
                let binding_name = self
                    .current_scope()
                    .resolve_variable(ident_pattern.as_str())
                    .unwrap();

                let parent_expr = self.access_path_to_expr(access);
                let assign =
                    self.assign_expr(self.assignment_target_ident(&binding_name), parent_expr);
                // (binding = parent, true)
                Some(self.ast.expression_sequence(
                    SPAN,
                    self.ast.vec_from_iter([assign, self.bool_expr(true)]),
                ))
            }

            hir::PatKind::Enum(path, patterns) => {
                self.generate_enum_pat_condition(path, patterns, access, fixed_list_idents)
            }

            hir::PatKind::Literal(literal) => {
                let parent_expr = self.access_path_to_expr(access);
                let lit = self.generate_literal(literal);
                Some(self.ast.expression_binary(
                    SPAN,
                    parent_expr,
                    BinaryOperator::StrictEquality,
                    lit,
                ))
            }

            hir::PatKind::List(patterns) if patterns.is_empty() => {
                let parent_expr = self.access_path_to_expr(access);
                let length = self.static_member_expr(parent_expr, "length");
                Some(self.ast.expression_binary(
                    SPAN,
                    length,
                    BinaryOperator::StrictEquality,
                    self.num_expr(0.0),
                ))
            }

            hir::PatKind::List(patterns) => {
                self.generate_list_pat_condition(patterns, root_pat, access, fixed_list_idents)
            }

            hir::PatKind::Rest(pattern) => {
                self.generate_pat_condition(pattern, false, access, fixed_list_idents)
            }

            hir::PatKind::Wildcard => None,
        }
    }

    fn generate_enum_pat_condition(
        &mut self,
        path: &hir::Path,
        patterns: &[(Ident, hir::Pat)],
        access: &AccessPath,
        fixed_list_idents: &Option<Vec<String>>,
    ) -> Option<Expression<'a>> {
        let parent_expr = self.access_path_to_expr(access);
        let tag_access = self.static_member_expr(parent_expr, "tag");
        let resolved = self
            .current_scope()
            .resolve_variable(&path.to_string())
            .unwrap_or_else(|| path.join("."));

        let mut cond = self.ast.expression_binary(
            SPAN,
            tag_access,
            BinaryOperator::StrictEquality,
            self.ident_expr(&resolved),
        );

        for (ident, pattern) in patterns.iter().filter(|(_, pat)| !pat.is_wildcard()) {
            let sub_access = if ident.as_str().chars().all(char::is_numeric) {
                let idx = ident.as_str().parse::<usize>().unwrap();
                AccessPath::Index(Box::new(access.clone()), idx)
            } else {
                AccessPath::Field(Box::new(access.clone()), ident.to_string())
            };

            if let Some(sub_cond) =
                self.generate_pat_condition(pattern, false, &sub_access, fixed_list_idents)
            {
                cond = self
                    .ast
                    .expression_logical(SPAN, cond, LogicalOperator::And, sub_cond);
            }
        }

        Some(cond)
    }

    fn generate_list_pat_condition(
        &mut self,
        patterns: &[hir::Pat],
        root_pat: bool,
        access: &AccessPath,
        fixed_list_idents: &Option<Vec<String>>,
    ) -> Option<Expression<'a>> {
        // Optimization: when matching against a list of identifiers, use them directly
        if root_pat && let Some(idents) = fixed_list_idents {
            return patterns
                .iter()
                .enumerate()
                .filter(|(_, p)| !p.is_wildcard() && !p.is_ident())
                .fold(None, |cond, (i, pattern)| {
                    let sub_access = AccessPath::Ident(idents[i].clone());
                    let sub_cond = self.generate_pat_condition(
                        pattern,
                        false,
                        &sub_access,
                        fixed_list_idents,
                    )?;
                    Some(match cond {
                        Some(c) => {
                            self.ast
                                .expression_logical(SPAN, c, LogicalOperator::And, sub_cond)
                        }
                        None => sub_cond,
                    })
                });
        }

        let parent_expr = self.access_path_to_expr(access);
        let length = self.static_member_expr(parent_expr, "length");
        let min_len = patterns
            .iter()
            .filter(|p| !matches!(p.kind, hir::PatKind::Rest(_)))
            .count();

        let mut cond = self.ast.expression_binary(
            SPAN,
            length,
            BinaryOperator::GreaterEqualThan,
            self.num_expr(min_len as f64),
        );

        for (i, pattern) in patterns.iter().enumerate() {
            if pattern.is_wildcard() {
                continue;
            }

            let sub_access = if matches!(pattern.kind, hir::PatKind::Rest(_)) {
                AccessPath::Slice(Box::new(access.clone()), i)
            } else {
                AccessPath::Index(Box::new(access.clone()), i)
            };

            if let Some(sub_cond) =
                self.generate_pat_condition(pattern, false, &sub_access, fixed_list_idents)
            {
                cond = self
                    .ast
                    .expression_logical(SPAN, cond, LogicalOperator::And, sub_cond);
            }
        }

        Some(cond)
    }

    fn generate_match_arm_guard_expr(
        &mut self,
        guard: &hir::Expr,
        let_guard_var: &str,
    ) -> Expression<'a> {
        if let hir::ExprKind::Let(pat, expr) = &guard.kind {
            let val = self.generate_expr(expr);
            let assign = self.assign_expr(self.assignment_target_ident(let_guard_var), val);
            // (let_guard_var = expr, true)
            let seq = self
                .ast
                .expression_sequence(SPAN, self.ast.vec_from_iter([assign, self.bool_expr(true)]));

            let root_access = AccessPath::Ident(let_guard_var.to_string());
            if let Some(pat_cond) = self.generate_pat_condition(pat, false, &root_access, &None) {
                self.ast
                    .expression_logical(SPAN, seq, LogicalOperator::And, pat_cond)
            } else {
                seq
            }
        } else {
            self.generate_expr(guard)
        }
    }
}

fn get_pat_identifiers(pattern: &hir::Pat) -> Vec<String> {
    match &pattern.kind {
        hir::PatKind::Identifier(_, ident) => vec![ident.to_string()],
        hir::PatKind::Enum(_, patterns) => patterns
            .iter()
            .flat_map(|(_, p)| get_pat_identifiers(p))
            .collect(),
        hir::PatKind::Literal(_) => Vec::new(),
        hir::PatKind::List(patterns) => patterns.iter().flat_map(get_pat_identifiers).collect(),
        hir::PatKind::Rest(pattern) => get_pat_identifiers(pattern),
        hir::PatKind::Wildcard => Vec::new(),
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

#[allow(dead_code)]
fn is_fixed_list_pattern(pat: &hir::Pat) -> bool {
    match &pat.kind {
        hir::PatKind::List(pats) => {
            !pats.is_empty() && pats.iter().all(|pat| pat.is_wildcard() || pat.is_ident())
        }
        _ => false,
    }
}
