use std::collections::HashMap;

use oxc_ast::NONE;
use oxc_ast::ast::*;
use oxc_span::SPAN;
use tlang_ast::token::Literal;
use tlang_hir as hir;
use tlang_hir::HirId;

use crate::error::CodegenWarning;
use crate::generator::InnerCodegen;
use tlang_ast::node::Ident;
use tlang_builtins_js as builtins;

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

        // 5. Check for overlapping discriminant values across multiple enums.
        self.check_multi_discriminant_enum_overlap(arms, expr.span);

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
            // Generate a runtime error for non-exhaustive matches. Only pass
            // the match value when the root maps to a real JS identifier.
            let callee = self.ident_expr("$matchError");
            let args = match root {
                AccessPath::Ident(name) if !name.is_empty() => {
                    vec![Argument::from(self.access_path_to_expr(root))]
                }
                _ => vec![],
            };
            let call = self.call_expr(callee, args);
            return Some(self.expr_stmt(call));
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
        let is_struct = path.res.is_struct_def();

        let resolved = if let Some(hir_id) = path.res.hir_id() {
            // User-defined enum variant or struct: always pre-registered in
            // the NameMap by `pre_register_declarations`, so the lookup
            // should always succeed.  The `path.join(".")` structural
            // fallback handles any edge cases during incremental compilation.
            self.name_map
                .resolve(hir_id)
                .map(|s| s.to_string())
                .unwrap_or_else(|| path.join("."))
        } else {
            // Builtin path (no HirId assigned by semantic analysis).
            // Unqualified names like `Some` and `None` are looked up directly;
            // qualified paths like `Option::None` are handled by `path.join(".")`
            // which already produces the structurally-correct JS form `Option.None`.
            builtins::lookup(&path.to_string())
                .map(|s| s.to_string())
                .unwrap_or_else(|| path.join("."))
        };

        // Check if this variant belongs to a discriminant enum.
        let is_discriminant_variant = path
            .res
            .hir_id()
            .is_some_and(|id| self.discriminant_variant_hir_ids.contains(&id));

        let mut cond = if is_struct {
            // Struct patterns use `instanceof` checks since JS structs are
            // constructor functions without a `.tag` property.
            let parent_expr = self.access_path_to_expr(access);
            self.ast.expression_binary(
                SPAN,
                parent_expr,
                BinaryOperator::Instanceof,
                self.ident_expr(&resolved),
            )
        } else if is_discriminant_variant {
            // Discriminant enum variants are plain values (not objects),
            // so compare directly with `===`.
            let parent_expr = self.access_path_to_expr(access);
            self.ast.expression_binary(
                SPAN,
                parent_expr,
                BinaryOperator::StrictEquality,
                self.ident_expr(&resolved),
            )
        } else {
            // Enum variant patterns check the `.tag` property.
            let parent_expr = self.access_path_to_expr(access);
            let tag_access = self.static_member_expr(parent_expr, "tag");
            self.ast.expression_binary(
                SPAN,
                tag_access,
                BinaryOperator::StrictEquality,
                self.ident_expr(&resolved),
            )
        };

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

    /// Check if a match expression uses discriminant enum patterns whose
    /// discriminant values overlap, causing earlier arms to silently shadow
    /// later ones.
    ///
    /// This covers both cases:
    /// - **Single enum**: `enum E { A = 1, B = 1 }` — both arms are present
    ///   in the match but only the first can ever be reached.
    /// - **Multiple enums**: `enum A { a = 1 }` and `enum B { b = 1 }` mixed
    ///   in the same match.
    ///
    /// A warning is emitted only when the actual discriminant values of the
    /// patterns used in this match overlap; if all values are disjoint no
    /// warning is produced ("safe enough").
    fn check_multi_discriminant_enum_overlap(
        &mut self,
        arms: &[hir::MatchArm],
        span: tlang_span::Span,
    ) {
        // Collect the (enum_name, variant_name, literal) for every
        // discriminant-enum variant pattern used in the match arms.
        // We collect these in arm order so the first one wins in the overlap
        // map below, matching the runtime semantics.
        let mut used: Vec<(String, String, Literal)> = Vec::new();
        // Track which distinct enum names appear, for the warning message.
        let mut enum_names_seen: Vec<String> = Vec::new();

        for arm in arms {
            if let hir::PatKind::Enum(path, patterns) = &arm.pat.kind {
                // Only simple (no-parameter) discriminant enum patterns.
                if patterns.is_empty()
                    && let Some(variant_hir_id) = path.res.hir_id()
                    && let Some((enum_name, variant_name, lit)) =
                        self.variant_discriminant_values.get(&variant_hir_id)
                {
                    let enum_name = enum_name.clone();
                    let variant_name = variant_name.clone();
                    let lit = *lit;
                    if !enum_names_seen.contains(&enum_name) {
                        enum_names_seen.push(enum_name.clone());
                    }
                    used.push((enum_name, variant_name, lit));
                }
            }
        }

        // Need at least two discriminant variant patterns to have an overlap.
        if used.len() < 2 {
            return;
        }

        // Build a map from normalised literal key → (enum_name, variant_name)
        // for the first variant that claims each value, then detect overlaps.
        let mut value_map: HashMap<LiteralKey, (String, String)> = HashMap::new();
        let mut overlaps: Vec<(String, String, String, String)> = Vec::new();

        for (enum_name, variant_name, lit) in &used {
            let key = LiteralKey::from_literal(lit);
            if let Some((prev_enum, prev_variant)) = value_map.get(&key) {
                overlaps.push((
                    prev_enum.clone(),
                    prev_variant.clone(),
                    enum_name.clone(),
                    variant_name.clone(),
                ));
            } else {
                value_map.insert(key, (enum_name.clone(), variant_name.clone()));
            }
        }

        if overlaps.is_empty() {
            // Values are disjoint — safe enough to not warn.
            return;
        }

        let overlap_details: Vec<String> = overlaps
            .iter()
            .map(|(ea, va, eb, vb)| {
                if ea == eb {
                    format!("`{ea}::{va}` and `{ea}::{vb}` share the same discriminant value")
                } else {
                    format!("`{ea}::{va}` and `{eb}::{vb}` share the same discriminant value")
                }
            })
            .collect();

        let message = if enum_names_seen.len() == 1 {
            format!(
                "match expression uses variants of `{}` with duplicate discriminant values: {}; \
                 earlier arms will shadow later ones",
                enum_names_seen[0],
                overlap_details.join(", "),
            )
        } else {
            format!(
                "match expression uses discriminant enums {} whose values overlap: {}; \
                 earlier arms will shadow later ones",
                enum_names_seen.join(", "),
                overlap_details.join(", "),
            )
        };

        self.warnings.push(CodegenWarning::new(message, span));
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

/// A normalised, hashable key derived from a discriminant [`Literal`] value.
///
/// Integer variants (`Integer` / `UnsignedInteger`) are normalised to `i128`
/// so that `Integer(1)` and `UnsignedInteger(1)` hash to the same bucket and
/// compare as equal (all `u64` values fit in `i128`).  `Float` values use
/// their bit representation for hashing (so `NaN != NaN` and `-0.0 != 0.0`,
/// which matches JavaScript `===` semantics).  String and char values compare
/// by their symbol text.  `None` has its own dedicated variant to avoid
/// confusion with a string-discriminant enum whose value happens to be `"nil"`.
#[derive(PartialEq, Eq, Hash)]
enum LiteralKey {
    Integer(i128),
    Float(u64),
    String(String),
    Bool(bool),
    None,
}

impl LiteralKey {
    fn from_literal(lit: &Literal) -> Self {
        match lit {
            Literal::Integer(n) => LiteralKey::Integer(*n as i128),
            Literal::UnsignedInteger(n) => LiteralKey::Integer(*n as i128),
            Literal::Float(f) => LiteralKey::Float(f.to_bits()),
            Literal::String(s) | Literal::Char(s) => LiteralKey::String(s.to_string()),
            Literal::Boolean(b) => LiteralKey::Bool(*b),
            Literal::None => LiteralKey::None,
        }
    }
}
