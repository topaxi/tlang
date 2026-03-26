use std::collections::HashMap;

use oxc_ast::NONE;
use oxc_ast::ast::*;
use oxc_span::SPAN;
use tlang_hir as hir;
use tlang_hir::HirId;

use crate::builtins;
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
                let name = if let Some(hir_id) = path.res.hir_id() {
                    self.name_map
                        .resolve(hir_id)
                        .expect("Match expression identifier not found")
                        .to_string()
                } else {
                    builtins::lookup(path.first_ident().as_str())
                        .expect("Match expression identifier not found")
                        .to_string()
                };
                (name, None)
            }
            hir::ExprKind::List(exprs)
                if !exprs.is_empty() && exprs.iter().all(|e| e.is_path()) =>
            {
                let mut idents = Vec::with_capacity(exprs.len());
                let mut all_resolved = true;
                for e in exprs {
                    let path = e.path().unwrap();
                    let ident = path.first_ident();
                    let name = ident.as_str();
                    let span = ident.span;

                    let resolved = if let Some(hir_id) = path.res.hir_id() {
                        self.name_map.resolve(hir_id).map(|s| s.to_string())
                    } else {
                        builtins::lookup(name).map(|s| s.to_string())
                    };

                    match resolved {
                        Some(r) => idents.push(r),
                        None => {
                            self.errors
                                .push(crate::error::CodegenError::unresolved_identifier(
                                    name, span,
                                ));
                            all_resolved = false;
                            break;
                        }
                    }
                }
                if all_resolved {
                    (String::new(), Some(idents))
                } else {
                    self.bind_match_subject_as_tmp(expr, &mut declarators)
                }
            }
            _ => self.bind_match_subject_as_tmp(expr, &mut declarators),
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

    /// Bind the match subject `expr` to a fresh tmp variable and append a
    /// `let $tmp = <expr>` declarator.  Returns `(tmp_name, None)`.
    ///
    /// This is the fall-back path used when the match subject cannot be
    /// represented as a fixed list of in-scope identifiers.
    fn bind_match_subject_as_tmp(
        &mut self,
        expr: &hir::Expr,
        declarators: &mut Vec<VariableDeclarator<'a>>,
    ) -> (String, Option<Vec<String>>) {
        let tmp = self.name_map.alloc_tmp();
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

    fn declare_match_variables(
        &mut self,
        arms: &[hir::MatchArm],
        match_value_name: &str,
        fixed_list_idents: &Option<Vec<String>>,
        declarators: &mut Vec<VariableDeclarator<'a>>,
    ) -> String {
        let let_guard_var = if arms.iter().any(|arm| arm.has_let_guard()) {
            let var = self.name_map.alloc_tmp();
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

        // Collect all pattern identifiers across arms.  Multiple arms may
        // introduce the same source name (e.g. `Some(a)` and `Ok(a)`) — they
        // should share one `let` declaration.  We deduplicate by name, mapping
        // all HirIds with the same name to the same JS binding.
        //
        // Identifiers whose name matches the match subject (or one of the
        // fixed list idents) don't need a separate `let` declaration in JS —
        // they reuse the existing binding.  But we still need to register their
        // HirId in the NameMap so that HirId-based resolution works in
        // `generate_pat_condition`.
        let all_idents: Vec<(HirId, String)> = arms
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
            .collect();

        let mut seen_names: HashMap<String, String> = HashMap::new();
        for (hir_id, name) in &all_idents {
            // Check if this pattern identifier aliases the match subject (or one
            // of the fixed-list elements).  We compare by *JS binding name* so
            // that renamed identifiers (e.g. `x` declared as `x$0` due to
            // shadowing) are detected correctly.  `resolve_by_name` looks up the
            // last registered JS name for a given source name.
            //
            // We own the JS name here so that the immutable borrow of `name_map`
            // is released before the mutable `register_exact` call below.
            let subject_js_name: Option<String> =
                self.name_map.resolve_by_name(name).map(|s| s.to_string());
            let is_subject_alias = match (&subject_js_name, fixed_list_idents) {
                (Some(js), Some(idents)) => idents.iter().any(|s| s == js),
                (Some(js), None) => js == match_value_name,
                _ => false,
            };

            if is_subject_alias {
                // Alias this pattern HirId to the actual JS binding of the match
                // subject / fixed-list element (not the raw source name, which may
                // differ after dedup-suffixing).
                // `subject_js_name` is guaranteed `Some` when `is_subject_alias`.
                let js_binding = subject_js_name.unwrap();
                self.name_map.register_exact(*hir_id, &js_binding);
            } else if let Some(js_name) = seen_names.get(name) {
                // Same source name in a different arm — share the JS binding.
                self.name_map.register_exact(*hir_id, js_name);
            } else {
                let js_name = self.name_map.register(*hir_id, name);
                seen_names.insert(name.clone(), js_name.clone());
                declarators.push(self.ast.variable_declarator(
                    SPAN,
                    VariableDeclarationKind::Let,
                    self.binding_pattern_ident(&js_name),
                    NONE,
                    None,
                    false,
                ));
            }
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
            hir::PatKind::Identifier(hir_id, _ident_pattern) => {
                let binding_name = self
                    .name_map
                    .resolve(*hir_id)
                    .expect("Pattern identifier not registered in NameMap");

                let parent_expr = self.access_path_to_expr(access);
                let assign =
                    self.assign_expr(self.assignment_target_ident(binding_name), parent_expr);
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
        let resolved = if let Some(hir_id) = path.res.hir_id() {
            if let Some(s) = self.name_map.resolve(hir_id) {
                s.to_string()
            } else if let Some(s) = builtins::lookup(&path.to_string()) {
                // Semantically resolved but not in NameMap — this is a known
                // builtin looked up by its registered name.
                s.to_string()
            } else {
                // Semantically resolved (has HirId) but not in NameMap and not
                // an explicitly registered builtin.  For qualified enum variant
                // paths like `Option::None`, `path.join(".")` produces the
                // structurally-correct JS form (`Option.None`).
                path.join(".")
            }
        } else {
            builtins::lookup(&path.to_string())
                .or_else(|| builtins::lookup(path.last_ident().as_str()))
                .map(|s| s.to_string())
                // Semantic analysis already validates enum variant paths, so
                // any path that reaches here is structurally valid.  The
                // `path.join(".")` structural translation is correct for all
                // user-defined and builtin enum variants.
                .unwrap_or_else(|| path.join("."))
        };

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

fn get_pat_identifiers(pattern: &hir::Pat) -> Vec<(HirId, String)> {
    match &pattern.kind {
        hir::PatKind::Identifier(hir_id, ident) => vec![(*hir_id, ident.to_string())],
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
