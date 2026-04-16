//! Inlay hint collection for the tlang language.
//!
//! This module provides shared inlay hint logic used by both the LSP server
//! and the WASM playground bindings.  Hints are collected from a type-checked
//! HIR module and its associated [`TypeTable`].
//!
//! Supported hint categories:
//! - **Variable/const bindings** without explicit type annotations.
//! - **Function return types** when not annotated.
//! - **Function parameters** without explicit type annotations.
//! - **Pipeline / chained expression steps** — intermediate types after each
//!   `|>` operator and after wrapped member-access / method-call receivers when
//!   the chain spans multiple lines.

use std::collections::{HashMap, HashSet};

use tlang_hir as hir;
use tlang_hir::TyKind;
use tlang_span::{HirId, LineColumn, Span, TypeVarId};
use tlang_typeck::TypeTable;

// Re-export from the dedicated typed_hir module so existing consumers
// (e.g. `use crate::inlay_hints::TypedHir`) keep working.
pub use crate::typed_hir::{TypedHir, lower_and_typecheck};

/// Returns `true` when the type kind contains any unresolved `TyKind::Var`.
fn ty_contains_var(ty: &TyKind) -> bool {
    match ty {
        TyKind::Var(_) => true,
        TyKind::Slice(inner) => ty_contains_var(&inner.kind),
        TyKind::Dict(k, v) => ty_contains_var(&k.kind) || ty_contains_var(&v.kind),
        TyKind::Fn(params, ret) => {
            params.iter().any(|p| ty_contains_var(&p.kind)) || ty_contains_var(&ret.kind)
        }
        TyKind::Path(_, type_args) => type_args.iter().any(|arg| ty_contains_var(&arg.kind)),
        TyKind::Union(tys) => tys.iter().any(|t| ty_contains_var(&t.kind)),
        _ => false,
    }
}

/// A single inlay hint to be displayed in the editor.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InlayHint {
    /// 0-based line number.
    pub line: u32,
    /// 0-based column (character offset).
    pub character: u32,
    /// The label text to display (e.g. `: i64`, `-> bool`).
    pub label: String,
    /// The category of hint.
    pub kind: InlayHintKind,
}

/// The category of an inlay hint.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum InlayHintKind {
    /// Type annotation for a variable, constant, or parameter binding.
    Type,
    /// Return type for a function declaration.
    ReturnType,
    /// Intermediate type produced by one step of a multi-line chain (`|>`,
    /// member access, or method call).
    ChainedPipeline,
}

/// Collect inlay hints from a type-checked HIR module.
///
/// If `range` is `Some`, only hints within the given line range
/// `[start_line, end_line]` (inclusive, 0-based) are returned.
pub fn collect_inlay_hints(typed_hir: &TypedHir, range: Option<(u32, u32)>) -> Vec<InlayHint> {
    let mut hints = Vec::new();
    let ctx = HintCtx {
        type_table: &typed_hir.type_table,
        pipeline_call_ids: &typed_hir.pipeline_call_ids,
        type_var_names: HashMap::new(),
        range,
    };
    collect_block_hints(&typed_hir.module.block, &ctx, &mut hints);
    hints
}

// ── Internal collection context ────────────────────────────────────────

/// Shared read-only context threaded through all hint-collection helpers.
struct HintCtx<'a> {
    type_table: &'a TypeTable,
    pipeline_call_ids: &'a HashSet<HirId>,
    type_var_names: TypeVarNames,
    range: Option<(u32, u32)>,
}

type TypeVarNames = HashMap<TypeVarId, String>;

// ── Block / statement traversal ────────────────────────────────────────

fn collect_block_hints(block: &hir::Block, ctx: &HintCtx<'_>, hints: &mut Vec<InlayHint>) {
    for stmt in &block.stmts {
        collect_stmt_hints(stmt, ctx, hints);
    }
    if let Some(expr) = &block.expr {
        collect_expr_hints(expr, ctx, hints);
    }
}

fn collect_stmt_hints(stmt: &hir::Stmt, ctx: &HintCtx<'_>, hints: &mut Vec<InlayHint>) {
    match &stmt.kind {
        hir::StmtKind::Let(pat, expr, ty) => {
            // Recurse into the initialiser expression.
            collect_expr_hints(expr, ctx, hints);

            // Show a type hint when no explicit annotation was written.
            if matches!(ty.kind, TyKind::Unknown) && !matches!(pat.ty.kind, TyKind::Unknown) {
                collect_pat_type_hints(pat, ctx.type_table, &ctx.type_var_names, ctx.range, hints);
            }
        }
        hir::StmtKind::Const(_, pat, expr, ty) => {
            collect_expr_hints(expr, ctx, hints);

            if matches!(ty.kind, TyKind::Unknown) && !matches!(pat.ty.kind, TyKind::Unknown) {
                collect_pat_type_hints(pat, ctx.type_table, &ctx.type_var_names, ctx.range, hints);
            }
        }
        hir::StmtKind::FunctionDeclaration(decl) => {
            collect_fn_decl_hints(decl, ctx, hints);
        }
        hir::StmtKind::DynFunctionDeclaration(_) => {
            // Dynamic dispatch wrappers — no hints needed.
        }
        hir::StmtKind::Return(expr) => {
            if let Some(e) = expr {
                collect_expr_hints(e, ctx, hints);
            }
        }
        hir::StmtKind::Expr(expr) => {
            collect_expr_hints(expr, ctx, hints);
        }
        hir::StmtKind::EnumDeclaration(_)
        | hir::StmtKind::StructDeclaration(_)
        | hir::StmtKind::ProtocolDeclaration(_) => {
            // Type declarations — no hints needed.
        }
        hir::StmtKind::ImplBlock(impl_block) => {
            for method in &impl_block.methods {
                collect_fn_decl_hints(method, ctx, hints);
            }
        }
    }
}

// ── Function declaration hints ─────────────────────────────────────────

#[allow(clippy::too_many_lines)]
fn collect_fn_decl_hints(
    decl: &hir::FunctionDeclaration,
    ctx: &HintCtx<'_>,
    hints: &mut Vec<InlayHint>,
) {
    let fn_ctx = HintCtx {
        type_table: ctx.type_table,
        pipeline_call_ids: ctx.pipeline_call_ids,
        type_var_names: extend_type_var_names(
            &ctx.type_var_names,
            &decl.all_type_params().cloned().collect::<Vec<_>>(),
        ),
        range: ctx.range,
    };

    // Parameter type hints — show when the user didn't annotate a type.
    for param in &decl.parameters {
        if !param.has_type_annotation {
            let inferred_ty = preferred_decl_hint_ty(
                &param.type_annotation.kind,
                fn_ctx
                    .type_table
                    .get(&param.hir_id)
                    .map(|info| &info.ty.kind),
            );

            if matches!(inferred_ty, TyKind::Unknown) {
                continue;
            }

            let pos = param.name.span.end_lc;
            push_hint(
                hints,
                fn_ctx.range,
                pos,
                format!(": {}", format_ty_kind(inferred_ty, &fn_ctx.type_var_names)),
                InlayHintKind::Type,
            );
        }
    }

    let return_hint_spans: Vec<Span> = if decl.return_hint_spans.is_empty() {
        (!decl.has_return_type || ty_contains_only_vars(&decl.return_type.kind))
            .then_some(decl.params_span)
            .into_iter()
            .collect()
    } else {
        decl.return_hint_spans.clone()
    };

    let has_informative_clause_completion = |arms: &[hir::MatchArm]| {
        arms.iter()
            .filter_map(|arm| arm.block.expr.as_ref())
            .any(|expr| match &expr.kind {
                hir::ExprKind::Path(_) => false,
                hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => !matches!(
                    &call.callee.kind,
                    hir::ExprKind::Path(path) if path.res.hir_id() == Some(decl.hir_id)
                ),
                _ => true,
            })
    };

    // Return type hint — show when no explicit return type was written.
    if !return_hint_spans.is_empty() {
        if let Some(hir::Expr {
            kind: hir::ExprKind::Match(_, arms),
            ..
        }) = decl.body.expr.as_ref()
            && decl.return_hint_spans.len() == decl.return_hint_arm_indices.len()
        {
            if !has_informative_clause_completion(arms) {
                collect_block_hints(&decl.body, &fn_ctx, hints);
                return;
            }

            let clause_returns: Option<Vec<_>> = return_hint_spans
                .iter()
                .zip(decl.return_hint_arm_indices.iter())
                .map(|(span, arm_idx)| {
                    let ret_kind = arms
                        .get(*arm_idx)
                        .and_then(|arm| arm.block.expr.as_ref().map(|e| &e.ty.kind))?;
                    if matches!(ret_kind, TyKind::Unknown) || ty_contains_var(ret_kind) {
                        return None;
                    }
                    Some((*span, ret_kind))
                })
                .collect();

            if let Some(clause_returns) = clause_returns {
                for (span, ret_kind) in clause_returns {
                    push_hint(
                        hints,
                        fn_ctx.range,
                        return_type_hint_position(span),
                        format!(" -> {}", format_ty_kind(ret_kind, &fn_ctx.type_var_names)),
                        InlayHintKind::ReturnType,
                    );
                }
                collect_block_hints(&decl.body, &fn_ctx, hints);
                return;
            }
        }

        // Try to get the inferred return type. Priority:
        // 1. The type checker may have written it back to decl.return_type.
        // 2. The type table entry for this declaration (Fn signature).
        // 3. The body's completion expression type.
        let ret_from_decl = (!matches!(decl.return_type.kind, TyKind::Unknown)
            && !ty_contains_var(&decl.return_type.kind))
        .then_some(&decl.return_type.kind);

        let inferred_ret = ret_from_decl
            .or_else(|| {
                ctx.type_table
                    .get(&decl.hir_id)
                    .and_then(|info| match &info.ty.kind {
                        TyKind::Fn(_, ret) => Some(&ret.kind),
                        _ => None,
                    })
            })
            .or_else(|| decl.body.expr.as_ref().map(|e| &e.ty.kind));

        if let Some(ret_kind) = inferred_ret
            && !matches!(ret_kind, TyKind::Unknown)
            && !ty_contains_var(ret_kind)
        {
            for span in return_hint_spans {
                push_hint(
                    hints,
                    fn_ctx.range,
                    return_type_hint_position(span),
                    format!(" -> {}", format_ty_kind(ret_kind, &fn_ctx.type_var_names)),
                    InlayHintKind::ReturnType,
                );
            }
        }
    }

    // Recurse into the function body.
    collect_block_hints(&decl.body, &fn_ctx, hints);
}

/// Check whether a type consists entirely of unresolved type variables.
/// Used to detect return types that were set by bidirectional inference
/// but not yet resolved to concrete types — these should still get hints.
fn ty_contains_only_vars(ty: &TyKind) -> bool {
    matches!(ty, TyKind::Var(_))
}

/// Determine the position for a return type hint.
///
/// The hint is placed right after the closing `)` of the parameter list,
/// using `params_span.end_lc`.  Combined with a leading-space label
/// (`" -> T"`), the background of the hint starts right after `)` —
/// visually matching the `: type` parameter hints — and the source space
/// (or newline) before `{` is left uncoloured.
///
/// This also handles C-style brace placement cleanly: when `{` is on the
/// next line the hint still lands at the end of the `)` line.
fn return_type_hint_position(params_span: Span) -> LineColumn {
    params_span.end_lc
}

fn push_multiline_chain_hint(
    hints: &mut Vec<InlayHint>,
    ctx: &HintCtx<'_>,
    inner_expr: &hir::Expr,
    outer_expr: &hir::Expr,
) {
    if inner_expr.span.end_lc.line < outer_expr.span.end_lc.line
        && !matches!(inner_expr.ty.kind, TyKind::Unknown)
    {
        push_hint(
            hints,
            ctx.range,
            inner_expr.span.end_lc,
            format!(
                ": {}",
                format_ty_kind(&inner_expr.ty.kind, &ctx.type_var_names)
            ),
            InlayHintKind::ChainedPipeline,
        );
    }
}

// ── Expression traversal (recurse into nested functions / blocks) ──────

#[allow(clippy::too_many_lines)]
fn collect_expr_hints(expr: &hir::Expr, ctx: &HintCtx<'_>, hints: &mut Vec<InlayHint>) {
    match &expr.kind {
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            collect_block_hints(block, ctx, hints);
        }
        hir::ExprKind::FunctionExpression(decl) => {
            collect_fn_decl_hints(decl, ctx, hints);
        }
        hir::ExprKind::IfElse(cond, then_block, else_clauses) => {
            collect_expr_hints(cond, ctx, hints);
            collect_block_hints(then_block, ctx, hints);
            for clause in else_clauses {
                if let Some(cond) = &clause.condition {
                    collect_expr_hints(cond, ctx, hints);
                }
                collect_block_hints(&clause.consequence, ctx, hints);
            }
        }
        hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
            // Pipeline chain hint: if this Call was desugared from `|>` and
            // the piped-in LHS ends on a different line than the overall
            // expression, show the intermediate type at the end of the LHS
            // line so the user can see the value flowing into each step.
            if ctx.pipeline_call_ids.contains(&call.hir_id)
                && let Some(lhs) = call.arguments.first()
                && lhs.span.end_lc.line < expr.span.end_lc.line
                && !matches!(lhs.ty.kind, TyKind::Unknown)
            {
                push_hint(
                    hints,
                    ctx.range,
                    lhs.span.end_lc,
                    format!(": {}", format_ty_kind(&lhs.ty.kind, &ctx.type_var_names)),
                    InlayHintKind::ChainedPipeline,
                );
            }
            collect_expr_hints(&call.callee, ctx, hints);
            for arg in &call.arguments {
                collect_expr_hints(arg, ctx, hints);
            }
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            collect_expr_hints(lhs, ctx, hints);
            collect_expr_hints(rhs, ctx, hints);
        }
        hir::ExprKind::Unary(_, operand) => {
            collect_expr_hints(operand, ctx, hints);
        }
        hir::ExprKind::Let(_, expr) => {
            collect_expr_hints(expr, ctx, hints);
        }
        hir::ExprKind::Cast(expr, _) | hir::ExprKind::TryCast(expr, _) => {
            collect_expr_hints(expr, ctx, hints);
        }
        hir::ExprKind::List(items) => {
            for item in items {
                collect_expr_hints(item, ctx, hints);
            }
        }
        hir::ExprKind::Dict(entries) => {
            for (k, v) in entries {
                collect_expr_hints(k, ctx, hints);
                collect_expr_hints(v, ctx, hints);
            }
        }
        hir::ExprKind::FieldAccess(base, _) => {
            push_multiline_chain_hint(hints, ctx, base, expr);
            collect_expr_hints(base, ctx, hints);
        }
        hir::ExprKind::IndexAccess(base, index) => {
            collect_expr_hints(base, ctx, hints);
            collect_expr_hints(index, ctx, hints);
        }
        hir::ExprKind::Match(scrutinee, arms) => {
            collect_expr_hints(scrutinee, ctx, hints);
            for arm in arms {
                // Emit type hints for each binding introduced by the arm pattern
                // (e.g. `title: string` for `Page { title, alerts, posts }`).
                collect_pat_type_hints(
                    &arm.pat,
                    ctx.type_table,
                    &ctx.type_var_names,
                    ctx.range,
                    hints,
                );
                if let Some(guard) = &arm.guard {
                    collect_expr_hints(guard, ctx, hints);
                }
                collect_block_hints(&arm.block, ctx, hints);
            }
        }
        hir::ExprKind::Break(expr) => {
            if let Some(e) = expr {
                collect_expr_hints(e, ctx, hints);
            }
        }
        hir::ExprKind::Range(range_expr) => {
            collect_expr_hints(&range_expr.start, ctx, hints);
            collect_expr_hints(&range_expr.end, ctx, hints);
        }
        hir::ExprKind::TaggedString { tag, exprs, .. } => {
            collect_expr_hints(tag, ctx, hints);
            for e in exprs {
                collect_expr_hints(e, ctx, hints);
            }
        }
        hir::ExprKind::Implements(expr, _) => {
            collect_expr_hints(expr, ctx, hints);
        }
        // Leaf nodes — no children to recurse into.
        hir::ExprKind::Path(_)
        | hir::ExprKind::Literal(_)
        | hir::ExprKind::Continue
        | hir::ExprKind::Wildcard => {}
    }
}

// ── Pattern type hints ─────────────────────────────────────────────────

/// Emit a type hint for a pattern binding (let/const).
fn collect_pat_type_hints(
    pat: &hir::Pat,
    type_table: &TypeTable,
    type_var_names: &TypeVarNames,
    range: Option<(u32, u32)>,
    hints: &mut Vec<InlayHint>,
) {
    match &pat.kind {
        hir::PatKind::Identifier(hir_id, ident) => {
            // Skip wildcard bindings (`_`) and compiler-generated bindings such
            // as loop temporaries, which carry default spans and are not
            // user-authored source locations.
            if ident.as_str() == "_" || ident.span == Span::default() {
                return;
            }
            let inferred_ty = preferred_pattern_hint_ty(
                &pat.ty.kind,
                type_table.get(hir_id).map(|info| &info.ty.kind),
            );
            if matches!(inferred_ty, TyKind::Unknown) {
                return;
            }
            let pos = ident.span.end_lc;
            push_hint(
                hints,
                range,
                pos,
                format!(": {}", format_ty_kind(inferred_ty, type_var_names)),
                InlayHintKind::Type,
            );
        }
        hir::PatKind::List(pats) => {
            // For list destructuring, emit hints for each sub-pattern.
            for sub_pat in pats {
                collect_pat_type_hints(sub_pat, type_table, type_var_names, range, hints);
            }
        }
        hir::PatKind::Rest(inner) => {
            collect_pat_type_hints(inner, type_table, type_var_names, range, hints);
        }
        hir::PatKind::Enum(_, fields) => {
            for (_, field_pat) in fields {
                collect_pat_type_hints(field_pat, type_table, type_var_names, range, hints);
            }
        }
        // Wildcards and literal patterns don't need hints.
        hir::PatKind::Wildcard | hir::PatKind::Literal(_) => {}
    }
}

fn preferred_pattern_hint_ty<'a>(pat_ty: &'a TyKind, table_ty: Option<&'a TyKind>) -> &'a TyKind {
    match table_ty {
        None => pat_ty,
        Some(TyKind::Unknown) => pat_ty,
        Some(table_ty) if matches!(pat_ty, TyKind::Unknown) => table_ty,
        Some(table_ty) if ty_contains_var(pat_ty) && ty_contains_var(table_ty) => pat_ty,
        Some(table_ty)
            if matches!(
                pat_ty,
                TyKind::Slice(_) | TyKind::Dict(_, _) | TyKind::Fn(_, _)
            ) && matches!(table_ty, TyKind::Path(..)) =>
        {
            pat_ty
        }
        Some(table_ty) => table_ty,
    }
}

fn preferred_decl_hint_ty<'a>(decl_ty: &'a TyKind, table_ty: Option<&'a TyKind>) -> &'a TyKind {
    match table_ty {
        None => decl_ty,
        Some(TyKind::Unknown) => decl_ty,
        Some(table_ty) if matches!(decl_ty, TyKind::Unknown) => table_ty,
        Some(table_ty) if ty_contains_var(decl_ty) && ty_contains_var(table_ty) => decl_ty,
        Some(table_ty) => table_ty,
    }
}

fn extend_type_var_names(parent: &TypeVarNames, type_params: &[hir::TypeParam]) -> TypeVarNames {
    let mut names = parent.clone();
    for type_param in type_params {
        names.insert(type_param.type_var_id, type_param.name.to_string());
    }
    names
}

fn format_ty_kind(ty: &TyKind, type_var_names: &TypeVarNames) -> String {
    match ty {
        TyKind::Unknown => "unknown".to_string(),
        TyKind::Primitive(p) => p.to_string(),
        TyKind::Fn(params, ret) => {
            let params = params
                .iter()
                .map(|param| format_ty_kind(&param.kind, type_var_names))
                .collect::<Vec<_>>()
                .join(", ");
            format!(
                "fn({params}) -> {}",
                format_ty_kind(&ret.kind, type_var_names)
            )
        }
        TyKind::Slice(inner) => format!("List<{}>", format_ty_kind(&inner.kind, type_var_names)),
        TyKind::Dict(key, value) => format!(
            "Dict<{}, {}>",
            format_ty_kind(&key.kind, type_var_names),
            format_ty_kind(&value.kind, type_var_names)
        ),
        TyKind::Never => "never".to_string(),
        TyKind::Var(id) => type_var_names
            .get(id)
            .cloned()
            .unwrap_or_else(|| format!("?{id}")),
        TyKind::Path(path, type_args) => {
            if type_args.is_empty() {
                path.to_string()
            } else {
                format!(
                    "{path}<{}>",
                    type_args
                        .iter()
                        .map(|ty| format_ty_kind(&ty.kind, type_var_names))
                        .collect::<Vec<_>>()
                        .join(", ")
                )
            }
        }
        TyKind::Union(tys) => tys
            .iter()
            .map(|ty| format_ty_kind(&ty.kind, type_var_names))
            .collect::<Vec<_>>()
            .join(" | "),
    }
}

// ── Helpers ────────────────────────────────────────────────────────────

/// Push a hint if it falls within the optional line range.
///
/// Positions coming from the lexer's [`LineColumn`] use a mixed coordinate
/// system: line 0 has 0-based columns, but subsequent lines use 1-based
/// columns (the lexer resets `current_column` to 1 after each `\n`).  This
/// function normalises to the editor-standard 0-based columns before
/// recording the hint.
fn push_hint(
    hints: &mut Vec<InlayHint>,
    range: Option<(u32, u32)>,
    pos: LineColumn,
    label: String,
    kind: InlayHintKind,
) {
    if let Some((start, end)) = range
        && (pos.line < start || pos.line > end)
    {
        return;
    }

    // Normalise to 0-based columns (lexer uses 1-based on lines > 0).
    let column = if pos.line > 0 {
        pos.column.saturating_sub(1)
    } else {
        pos.column
    };

    hints.push(InlayHint {
        line: pos.line,
        character: column,
        label,
        kind,
    });
}

// ── Type lookup by source position ─────────────────────────────────────

/// Look up the inferred type of a symbol whose definition starts at the
/// given (0-based line, 0-based column) position.
///
/// This walks the typed HIR to find let/const pattern bindings, function
/// parameters, and function declarations whose span matches `def_pos`.
/// Returns a human-readable type string (e.g. `"i64"`, `"Vector"`) or
/// `None` if no type information is available at that position.
pub fn type_at_definition(typed_hir: &TypedHir, def_line: u32, def_col: u32) -> Option<String> {
    // Normalise to the lexer's mixed coordinate system: line 0 uses 0-based
    // columns, subsequent lines use 1-based columns.
    let lexer_col = if def_line > 0 { def_col + 1 } else { def_col };

    let mut result: Option<String> = None;
    walk_block_for_type(
        &typed_hir.module.block,
        &typed_hir.type_table,
        &HashMap::new(),
        def_line,
        lexer_col,
        &mut result,
    );
    result
}

fn walk_block_for_type(
    block: &hir::Block,
    type_table: &TypeTable,
    type_var_names: &TypeVarNames,
    line: u32,
    col: u32,
    out: &mut Option<String>,
) {
    for stmt in &block.stmts {
        if out.is_some() {
            return;
        }
        walk_stmt_for_type(stmt, type_table, type_var_names, line, col, out);
    }
    if out.is_none()
        && let Some(expr) = &block.expr
    {
        walk_expr_for_type(expr, type_table, type_var_names, line, col, out);
    }
}

fn walk_stmt_for_type(
    stmt: &hir::Stmt,
    type_table: &TypeTable,
    type_var_names: &TypeVarNames,
    line: u32,
    col: u32,
    out: &mut Option<String>,
) {
    match &stmt.kind {
        hir::StmtKind::Expr(e) => walk_expr_for_type(e, type_table, type_var_names, line, col, out),
        hir::StmtKind::Let(pat, init, _ty) | hir::StmtKind::Const(_, pat, init, _ty) => {
            check_pat_for_type(pat, type_var_names, line, col, out);
            if out.is_none() {
                walk_expr_for_type(init, type_table, type_var_names, line, col, out);
            }
        }
        hir::StmtKind::FunctionDeclaration(decl) => {
            walk_fn_decl_for_type(decl, type_table, type_var_names, line, col, out);
        }
        hir::StmtKind::StructDeclaration(decl) => {
            let name_span = &decl.name.span;
            if name_span.start_lc.line == line && name_span.start_lc.column == col {
                *out = Some(decl.name.to_string());
            }
        }
        hir::StmtKind::ImplBlock(impl_block) => {
            for decl in &impl_block.methods {
                walk_fn_decl_for_type(decl, type_table, type_var_names, line, col, out);
            }
        }
        hir::StmtKind::EnumDeclaration(decl) => {
            let name_span = &decl.name.span;
            if name_span.start_lc.line == line && name_span.start_lc.column == col {
                *out = Some(decl.name.to_string());
            }
        }
        hir::StmtKind::Return(Some(e)) => {
            walk_expr_for_type(e, type_table, type_var_names, line, col, out)
        }
        _ => {}
    }
}

fn walk_fn_decl_for_type(
    decl: &hir::FunctionDeclaration,
    type_table: &TypeTable,
    parent_type_var_names: &TypeVarNames,
    line: u32,
    col: u32,
    out: &mut Option<String>,
) {
    if out.is_some() {
        return;
    }

    let type_var_names = extend_type_var_names(
        parent_type_var_names,
        &decl.all_type_params().cloned().collect::<Vec<_>>(),
    );

    // Build the function signature string, preferring the fully-inferred
    // types from the type table over the (possibly stale) AST annotations.
    let fn_sig = || -> String {
        let decl_sig = || {
            let params: Vec<String> = decl
                .parameters
                .iter()
                .map(|param| {
                    let param_ty = preferred_decl_hint_ty(
                        &param.type_annotation.kind,
                        type_table.get(&param.hir_id).map(|info| &info.ty.kind),
                    );
                    format_ty_kind(param_ty, &type_var_names)
                })
                .collect();
            let ret = preferred_decl_hint_ty(
                &decl.return_type.kind,
                type_table
                    .get(&decl.hir_id)
                    .and_then(|info| match &info.ty.kind {
                        TyKind::Fn(_, ret) => Some(&ret.kind),
                        _ => None,
                    }),
            );
            format!(
                "fn({}) -> {}",
                params.join(", "),
                format_ty_kind(ret, &type_var_names)
            )
        };

        if let Some(info) = type_table.get(&decl.hir_id)
            && let TyKind::Fn(ref param_tys, ref ret_ty) = info.ty.kind
            && (!(decl.owner_type_params.is_empty() && decl.type_params.is_empty())
                && (param_tys.iter().any(|param| ty_contains_var(&param.kind))
                    || ty_contains_var(&ret_ty.kind)))
        {
            decl_sig()
        } else if let Some(info) = type_table.get(&decl.hir_id)
            && let TyKind::Fn(ref param_tys, ref ret_ty) = info.ty.kind
        {
            let params: Vec<String> = param_tys
                .iter()
                .map(|p| format_ty_kind(&p.kind, &type_var_names))
                .collect();
            format!(
                "fn({}) -> {}",
                params.join(", "),
                format_ty_kind(&ret_ty.kind, &type_var_names)
            )
        } else {
            decl_sig()
        }
    };

    // Check function name.
    let name_span = &decl.name.span;
    if name_span.start_lc.line == line && name_span.start_lc.column == col {
        *out = Some(fn_sig());
        return;
    }

    // For closures, also match the `fn` keyword — the declaration span
    // starts at the `fn` token while the anonymous name has a zero span.
    if decl.span.start_lc.line == line && decl.span.start_lc.column == col {
        *out = Some(fn_sig());
        return;
    }

    // Check each parameter.
    for param in &decl.parameters {
        if param.name.span.start_lc.line == line && param.name.span.start_lc.column == col {
            let ty = type_table.get(&param.hir_id).map(|info| &info.ty.kind);
            let ty = preferred_decl_hint_ty(&param.type_annotation.kind, ty);
            if !matches!(ty, TyKind::Unknown) {
                *out = Some(format_ty_kind(ty, &type_var_names));
            }
            return;
        }
    }

    // Recurse into the body.
    walk_block_for_type(&decl.body, type_table, &type_var_names, line, col, out);
}

fn check_pat_for_type(
    pat: &hir::Pat,
    type_var_names: &TypeVarNames,
    line: u32,
    col: u32,
    out: &mut Option<String>,
) {
    match &pat.kind {
        hir::PatKind::Identifier(_, ident)
            if ident.span.start_lc.line == line && ident.span.start_lc.column == col =>
        {
            let ty = &pat.ty.kind;
            if !matches!(ty, TyKind::Unknown) {
                *out = Some(format_ty_kind(ty, type_var_names));
            }
        }
        hir::PatKind::List(pats) => {
            for p in pats {
                check_pat_for_type(p, type_var_names, line, col, out);
                if out.is_some() {
                    return;
                }
            }
        }
        hir::PatKind::Rest(inner) => check_pat_for_type(inner, type_var_names, line, col, out),
        hir::PatKind::Enum(_, fields) => {
            for (_, p) in fields {
                check_pat_for_type(p, type_var_names, line, col, out);
                if out.is_some() {
                    return;
                }
            }
        }
        _ => {}
    }
}

/// Report the type of `expr` into `out` when the cursor is exactly on `span`.
fn report_ty_at(
    expr: &hir::Expr,
    span: &Span,
    type_var_names: &TypeVarNames,
    line: u32,
    col: u32,
    out: &mut Option<String>,
) {
    if span.start_lc.line == line && span.start_lc.column == col {
        let ty = &expr.ty.kind;
        if !matches!(ty, TyKind::Unknown) {
            *out = Some(format_ty_kind(ty, type_var_names));
        }
    }
}

fn walk_expr_for_type(
    expr: &hir::Expr,
    type_table: &TypeTable,
    type_var_names: &TypeVarNames,
    line: u32,
    col: u32,
    out: &mut Option<String>,
) {
    if out.is_some() {
        return;
    }

    match &expr.kind {
        hir::ExprKind::Let(pat, init) => {
            check_pat_for_type(pat, type_var_names, line, col, out);
            if out.is_none() {
                walk_expr_for_type(init, type_table, type_var_names, line, col, out);
            }
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            walk_block_for_type(block, type_table, type_var_names, line, col, out);
        }
        hir::ExprKind::FunctionExpression(decl) => {
            walk_fn_decl_for_type(decl, type_table, type_var_names, line, col, out);
        }
        hir::ExprKind::IfElse(cond, then_block, else_clauses) => {
            walk_expr_for_type(cond, type_table, type_var_names, line, col, out);
            walk_block_for_type(then_block, type_table, type_var_names, line, col, out);
            for clause in else_clauses {
                if let Some(cond) = &clause.condition {
                    walk_expr_for_type(cond, type_table, type_var_names, line, col, out);
                }
                walk_block_for_type(
                    &clause.consequence,
                    type_table,
                    type_var_names,
                    line,
                    col,
                    out,
                );
            }
        }
        hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
            walk_expr_for_type(&call.callee, type_table, type_var_names, line, col, out);
            for arg in &call.arguments {
                walk_expr_for_type(arg, type_table, type_var_names, line, col, out);
            }
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            walk_expr_for_type(lhs, type_table, type_var_names, line, col, out);
            walk_expr_for_type(rhs, type_table, type_var_names, line, col, out);
        }
        hir::ExprKind::Unary(_, operand) => {
            walk_expr_for_type(operand, type_table, type_var_names, line, col, out);
        }
        hir::ExprKind::Match(scrutinee, arms) => {
            walk_expr_for_type(scrutinee, type_table, type_var_names, line, col, out);
            for arm in arms {
                check_pat_for_type(&arm.pat, type_var_names, line, col, out);
                if out.is_some() {
                    return;
                }
                if let Some(guard) = &arm.guard {
                    walk_expr_for_type(guard, type_table, type_var_names, line, col, out);
                }
                walk_block_for_type(&arm.block, type_table, type_var_names, line, col, out);
            }
        }
        hir::ExprKind::Path(path) => {
            report_ty_at(expr, &expr.span, type_var_names, line, col, out);
            for seg in &path.segments {
                report_ty_at(expr, &seg.ident.span, type_var_names, line, col, out);
            }
        }
        hir::ExprKind::FieldAccess(base, ident) => {
            walk_expr_for_type(base, type_table, type_var_names, line, col, out);
            report_ty_at(expr, &ident.span, type_var_names, line, col, out);
        }
        hir::ExprKind::IndexAccess(base, index) => {
            walk_expr_for_type(base, type_table, type_var_names, line, col, out);
            walk_expr_for_type(index, type_table, type_var_names, line, col, out);
        }
        hir::ExprKind::Cast(inner, _) | hir::ExprKind::TryCast(inner, _) => {
            walk_expr_for_type(inner, type_table, type_var_names, line, col, out);
        }
        hir::ExprKind::List(items) => {
            for item in items {
                walk_expr_for_type(item, type_table, type_var_names, line, col, out);
            }
        }
        hir::ExprKind::Dict(entries) => {
            for (k, v) in entries {
                walk_expr_for_type(k, type_table, type_var_names, line, col, out);
                walk_expr_for_type(v, type_table, type_var_names, line, col, out);
            }
        }
        hir::ExprKind::Break(e) => {
            if let Some(e) = e {
                walk_expr_for_type(e, type_table, type_var_names, line, col, out);
            }
        }
        hir::ExprKind::Range(r) => {
            walk_expr_for_type(&r.start, type_table, type_var_names, line, col, out);
            walk_expr_for_type(&r.end, type_table, type_var_names, line, col, out);
        }
        hir::ExprKind::TaggedString { tag, exprs, .. } => {
            walk_expr_for_type(tag, type_table, type_var_names, line, col, out);
            for e in exprs {
                walk_expr_for_type(e, type_table, type_var_names, line, col, out);
            }
        }
        hir::ExprKind::Implements(inner, _) => {
            walk_expr_for_type(inner, type_table, type_var_names, line, col, out);
        }
        hir::ExprKind::Literal(_) | hir::ExprKind::Continue | hir::ExprKind::Wildcard => {}
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: run the full pipeline on source code and collect inlay hints.
    fn hints_for(source: &str) -> Vec<InlayHint> {
        let result = crate::analyze(source, |_| {});
        assert!(
            result.module.is_some(),
            "parsing failed: {:?}",
            result.parse_issues
        );
        assert!(result.parse_meta.is_some(), "parse_meta is None");
        let typed_hir =
            lower_and_typecheck(&result).expect("HIR lowering/typechecking should succeed");
        collect_inlay_hints(&typed_hir, None)
    }

    #[test]
    fn let_binding_with_integer_literal() {
        let hints = hints_for("let x = 42;");
        assert!(
            hints
                .iter()
                .any(|h| h.label == ": i64" && h.kind == InlayHintKind::Type),
            "expected `: i64` hint, got: {hints:?}"
        );
    }

    #[test]
    fn let_binding_with_string_literal() {
        let hints = hints_for("let name = \"hello\";");
        assert!(
            hints
                .iter()
                .any(|h| h.label == ": String" && h.kind == InlayHintKind::Type),
            "expected `: String` hint, got: {hints:?}"
        );
    }

    #[test]
    fn let_binding_with_float_literal() {
        let hints = hints_for("let pi = 3.14;");
        assert!(
            hints
                .iter()
                .any(|h| h.label == ": f64" && h.kind == InlayHintKind::Type),
            "expected `: f64` hint, got: {hints:?}"
        );
    }

    #[test]
    fn let_binding_with_bool_literal() {
        let hints = hints_for("let flag = true;");
        assert!(
            hints
                .iter()
                .any(|h| h.label == ": bool" && h.kind == InlayHintKind::Type),
            "expected `: bool` hint, got: {hints:?}"
        );
    }

    #[test]
    fn const_binding_with_integer_literal() {
        let hints = hints_for("const MAX = 100;");
        assert!(
            hints
                .iter()
                .any(|h| h.label == ": i64" && h.kind == InlayHintKind::Type),
            "expected `: i64` hint for const, got: {hints:?}"
        );
    }

    #[test]
    fn let_binding_with_explicit_annotation_no_hint() {
        let hints = hints_for("let x: i64 = 42;");
        // No type hint should be shown when the user explicitly annotated.
        assert!(
            !hints
                .iter()
                .any(|h| h.kind == InlayHintKind::Type && h.label.contains("i64")),
            "should not show hint for explicitly annotated binding, got: {hints:?}"
        );
    }

    #[test]
    fn function_return_type_hint() {
        let hints = hints_for("fn add(a: i64, b: i64) { a + b }");
        assert!(
            hints
                .iter()
                .any(|h| h.label.contains("-> i64") && h.kind == InlayHintKind::ReturnType),
            "expected `-> i64` return type hint, got: {hints:?}"
        );
    }

    #[test]
    fn function_return_type_hint_for_if_else_enum_paths() {
        let hints = hints_for(
            r#"
enum Tree {
    Empty,
    Node { value: isize, left: Tree, right: Tree },
}

fn insert(flag: bool, value: isize) {
    if flag {
        Tree::Node { value, left: Tree::Empty, right: Tree::Empty }
    } else {
        Tree::Node { value, left: Tree::Empty, right: Tree::Empty }
    }
}
"#,
        );
        assert!(
            hints
                .iter()
                .any(|h| h.label.contains("-> Tree") && h.kind == InlayHintKind::ReturnType),
            "expected `-> Tree` return type hint, got: {hints:?}"
        );
    }

    #[test]
    fn recursive_multi_clause_function_return_type_hint() {
        let hints = hints_for(
            r#"
enum Expr {
    Value(isize),
    Add(Expr, Expr),
    Subtract(Expr, Expr),
    Multiply(Expr, Expr),
    Divide(Expr, Expr),
}

fn evaluate(Expr::Value(val)) { val }
fn evaluate(Expr::Add(left, right)) { evaluate(left) + evaluate(right) }
fn evaluate(Expr::Subtract(left, right)) { evaluate(left) - evaluate(right) }
fn evaluate(Expr::Multiply(left, right)) { evaluate(left) * evaluate(right) }
fn evaluate(Expr::Divide(left, right)) { evaluate(left) / evaluate(right) }
"#,
        );
        let return_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == InlayHintKind::ReturnType && h.label == " -> isize")
            .collect();
        assert_eq!(
            return_hints.len(),
            5,
            "expected one `-> isize` return hint per evaluate clause, got: {hints:?}"
        );
    }

    #[test]
    fn function_with_explicit_return_type_no_hint() {
        let hints = hints_for("fn add(a: i64, b: i64) -> i64 { a + b }");
        assert!(
            !hints.iter().any(|h| h.kind == InlayHintKind::ReturnType),
            "should not show return type hint when explicitly annotated, got: {hints:?}"
        );
    }

    #[test]
    fn function_parameter_unknown_type_no_hint() {
        // Un-annotated parameters with unknown type should not show a hint —
        // only inferred concrete types are useful.
        let hints = hints_for("fn double(x) { x }");
        let param_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == InlayHintKind::Type)
            .collect();
        assert!(
            param_hints.is_empty(),
            "should not show hint for unresolvable parameter type, got: {param_hints:?}"
        );
    }

    #[test]
    fn struct_pattern_param_hint_not_at_origin() {
        // Regression test: struct/enum destructuring parameters like
        // `fn f(Page { title }) { ... }` used to produce a hint at position
        // (0, 0) because `get_param_names` only extracted spans for
        // `PatKind::Identifier` patterns and fell back to `Span::default()`
        // for `PatKind::Enum`.
        let source = "struct Point { x: i64, y: i64 }\nfn f(Point { x, y }) { x + y }";
        let hints = hints_for(source);
        for hint in &hints {
            assert!(
                hint.line != 0 || hint.character != 0,
                "struct pattern param hint must not be at (0, 0), got: {hint:?}"
            );
        }
    }

    #[test]
    fn single_clause_struct_destructuring_no_unknown_hint() {
        // A single-clause function with a struct destructuring parameter should NOT
        // show `: unknown` for the synthesized parameter — the type is inferred from
        // the pattern name and the whole-pattern hint should be suppressed.
        let source = "struct Page { title: String }\nfn render(Page { title }) { title }";
        let hints = hints_for(source);
        assert!(
            !hints.iter().any(|h| h.label == ": unknown"),
            "should not show `: unknown` for struct destructuring param, got: {hints:?}"
        );
    }

    #[test]
    fn single_clause_struct_destructuring_field_hints() {
        // Per-field type hints should be shown for each binding created by
        // struct destructuring in a single-clause function parameter.
        let source = "struct Page { title: String }\nfn render(Page { title }) { title }";
        let hints = hints_for(source);
        assert!(
            hints.iter().any(|h| h.label == ": String"),
            "expected `: String` hint for `title` field binding, got: {hints:?}"
        );
    }

    #[test]
    fn enum_destructuring_field_hints_include_builtin_struct_types() {
        let source = r#"
enum Expense {
    Food(f32, Temporal::PlainDate),
}
fn Expense.date(Expense::Food(_, d)) { d }
"#;
        let hints = hints_for(source);
        assert!(
            hints.iter().any(|h| h.label == ": Temporal::PlainDate"),
            "expected `: Temporal::PlainDate` hint for enum payload binding, got: {hints:?}"
        );
    }

    #[test]
    fn function_parameter_with_annotation_no_hint() {
        let hints = hints_for("fn double(x: i64) { x * 2 }");
        // Only check that no Type hint exists for the parameter position.
        let param_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == InlayHintKind::Type && h.label.contains("i64"))
            .collect();
        assert!(
            param_hints.is_empty(),
            "should not show hint for annotated parameter, got: {param_hints:?}"
        );
    }

    #[test]
    fn nested_function_gets_hints() {
        let hints = hints_for("fn outer() { let x = 42; x }");
        assert!(
            hints
                .iter()
                .any(|h| h.label == ": i64" && h.kind == InlayHintKind::Type),
            "expected hint for let binding inside function, got: {hints:?}"
        );
    }

    #[test]
    fn range_filter_excludes_out_of_range_hints() {
        let source = "let a = 1;\nlet b = 2;\nlet c = 3;";
        let result = crate::analyze(source, |_| {});
        let typed_hir = lower_and_typecheck(&result).expect("lowering should succeed");
        // Only request hints for line 1 (0-based).
        let hints = collect_inlay_hints(&typed_hir, Some((1, 1)));
        assert!(
            hints.iter().all(|h| h.line == 1),
            "all hints should be on line 1, got: {hints:?}"
        );
    }

    #[test]
    fn wildcard_binding_no_hint() {
        let hints = hints_for("let _ = 42;");
        assert!(
            !hints.iter().any(|h| h.kind == InlayHintKind::Type),
            "should not show hint for wildcard binding, got: {hints:?}"
        );
    }

    // ── type_at_definition tests ──────────────────────────────────────

    fn typed_hir_for(source: &str) -> TypedHir {
        let result = crate::analyze(source, |_| {});
        lower_and_typecheck(&result).expect("pipeline should succeed")
    }

    #[test]
    fn type_at_definition_let_binding() {
        let typed_hir = typed_hir_for("let x = 42;");
        // `x` is at line 0, col 4 (0-based editor position)
        let ty = type_at_definition(&typed_hir, 0, 4);
        assert_eq!(ty.as_deref(), Some("i64"));
    }

    #[test]
    fn type_at_definition_function_parameter() {
        let typed_hir = typed_hir_for("fn add(a: i64, b: i64) { a + b }");
        // `a` param at line 0, col 7
        let ty = type_at_definition(&typed_hir, 0, 7);
        assert_eq!(ty.as_deref(), Some("i64"));
    }

    #[test]
    fn type_at_definition_generic_function_name_uses_type_param_names() {
        let typed_hir = typed_hir_for("fn identity<T>(x: T) -> T { x }");
        let ty = type_at_definition(&typed_hir, 0, 3);
        assert_eq!(ty.as_deref(), Some("fn(T) -> T"));
    }

    #[test]
    fn type_at_definition_returns_none_for_whitespace() {
        let typed_hir = typed_hir_for("let x = 42;");
        let ty = type_at_definition(&typed_hir, 0, 0);
        assert!(ty.is_none());
    }

    #[test]
    fn type_at_definition_self_parameter() {
        // `self` in `fn Vector.add(self, ...)` should be typed as `Vector`.
        let source =
            "struct Vector { x: i64 }\nfn Vector.add(self, other: Vector) -> Vector { self }";
        let typed_hir = typed_hir_for(source);
        // `self` is at line 1, col 14 (0-based editor position)
        let ty = type_at_definition(&typed_hir, 1, 14);
        assert_eq!(
            ty.as_deref(),
            Some("Vector"),
            "self parameter should be typed as Vector"
        );
    }

    #[test]
    fn generic_call_return_type_instantiation() {
        // map<T,U>(List<T>, fn(T)->U) -> List<U> called with List<i64>
        // and fn(x) { x ** 2 } should produce List<i64>.
        let source = r#"
fn map<T, U>([]: List<T>, _: fn(T) -> U) -> List<U> { [] }
fn map<T, U>([x, ...xs]: List<T>, f: fn(T) -> U) -> List<U> { [f(x), ...map(xs, f)] }
let result = [1,2,3] |> map(fn (x) { x ** 2 });
"#;
        let hints = hints_for(source);
        // Find the hint for `result` binding specifically.
        let result_hint = hints
            .iter()
            .find(|h| h.line == 3 && h.label.starts_with(": List"));
        assert!(
            result_hint.is_some(),
            "expected a type hint for `result`, got: {hints:?}"
        );
        let label = &result_hint.unwrap().label;
        assert_eq!(
            label, ": List<i64>",
            "map should return List<i64> after generic instantiation"
        );
    }

    #[test]
    fn generic_nominal_binding_hints_preserve_type_arguments() {
        let source = r#"
struct Pair<A, B> { first: A, second: B }
fn Pair::new<A, B>(first: A, second: B) -> Pair<A, B> { Pair { first: first, second: second } }
let p = Pair::new(1, "one");
"#;
        let hints = hints_for(source);
        assert!(
            hints
                .iter()
                .any(|h| h.line == 3 && h.label == ": Pair<i64, String>"),
            "expected `p` to keep instantiated nominal type arguments, got: {hints:?}"
        );
    }

    #[test]
    fn type_at_definition_generic_function_parameter_uses_type_param_names() {
        let typed_hir = typed_hir_for("fn identity<T>(x: T) -> T { x }");
        let ty = type_at_definition(&typed_hir, 0, 15);
        assert_eq!(ty.as_deref(), Some("T"));
    }

    #[test]
    fn multiline_tagged_string_pipeline_hint_anchors_after_expression() {
        let source = r#"
fn html(parts, values) { "" }
fn dedent(s) { s }
fn trim(s) { s }

let page = html"""
    <p>hello</p>
"""
|> dedent()
|> trim();
"#;

        let hints = hints_for(source);
        let chain_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == InlayHintKind::ChainedPipeline)
            .collect();

        assert!(
            chain_hints.iter().any(|h| h.line == 7),
            "expected a chained pipeline hint on the tagged string closing line, got: {chain_hints:?}"
        );
        assert!(
            !chain_hints.iter().any(|h| h.line == 5),
            "tagged string pipeline hint should not anchor to the opening line, got: {chain_hints:?}"
        );
    }

    #[test]
    fn multiline_member_call_hint_anchors_after_wrapped_call() {
        let source = r#"
struct Counter { value: i64 }
fn Counter.inc(self) -> Counter { self }
fn make_counter() -> Counter {
    Counter { value: 0 }
}

let _ = make_counter(
)
    .inc();
"#;

        let hints = hints_for(source);
        let chain_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == InlayHintKind::ChainedPipeline)
            .collect();

        assert!(
            chain_hints.iter().any(|h| h.line == 8),
            "expected a chained member-call hint on the wrapped call closing line, got: {chain_hints:?}"
        );
        assert!(
            !chain_hints.iter().any(|h| h.line == 7),
            "member-call hint should not anchor to the opening call line, got: {chain_hints:?}"
        );
    }

    #[test]
    fn multiline_field_access_hint_anchors_after_wrapped_call() {
        let source = r#"
struct Counter { value: i64 }
fn make_counter() -> Counter {
    Counter { value: 0 }
}

let _ = make_counter(
)
    .value;
"#;

        let hints = hints_for(source);
        let chain_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == InlayHintKind::ChainedPipeline)
            .collect();

        assert!(
            chain_hints.iter().any(|h| h.line == 7),
            "expected a chained field-access hint on the wrapped call closing line, got: {chain_hints:?}"
        );
        assert!(
            !chain_hints.iter().any(|h| h.line == 6),
            "field-access hint should not anchor to the opening call line, got: {chain_hints:?}"
        );
    }

    #[test]
    fn let_binding_hint_from_loop_expression_result() {
        let source = r#"
let sum = for x in [1, 2, 3]; with acc = 0 {
    acc + x
};
"#;
        let hints = hints_for(source);
        let sum_hint = hints
            .iter()
            .find(|h| h.line == 1 && h.kind == InlayHintKind::Type && h.label == ": i64");
        assert!(
            sum_hint.is_some(),
            "expected let-binding hint from loop expression result, got: {hints:?}"
        );
    }

    #[test]
    fn type_at_definition_for_loop_expression_binding() {
        let source = r#"
let sum = for x in [1, 2, 3]; with acc = 0 {
    acc + x
};
"#;
        let typed_hir = typed_hir_for(source);
        let ty = type_at_definition(&typed_hir, 1, 4);
        assert_eq!(ty.as_deref(), Some("i64"));
    }

    #[test]
    fn closure_param_inlay_hint_from_generic_inference() {
        // When a closure is passed to a generic function, the inferred
        // parameter types should show as inlay hints.
        let source = r#"
fn map<T, U>([]: List<T>, _: fn(T) -> U) -> List<U> { [] }
fn map<T, U>([x, ...xs]: List<T>, f: fn(T) -> U) -> List<U> { [f(x), ...map(xs, f)] }
[1,2,3] |> map(fn (x) { x ** 2 });
"#;
        let hints = hints_for(source);
        // The closure `fn (x) { x ** 2 }` — `x` should get `: i64` hint.
        let param_hint = hints
            .iter()
            .find(|h| h.line == 3 && h.kind == InlayHintKind::Type && h.label == ": i64");
        assert!(
            param_hint.is_some(),
            "expected `: i64` hint for closure param `x`, got: {hints:?}"
        );
    }

    #[test]
    fn closure_return_type_inlay_hint() {
        // Closures without explicit return type should show inferred return type.
        let source = r#"
fn map<T, U>([]: List<T>, _: fn(T) -> U) -> List<U> { [] }
fn map<T, U>([x, ...xs]: List<T>, f: fn(T) -> U) -> List<U> { [f(x), ...map(xs, f)] }
[1,2,3] |> map(fn (x) { x ** 2 });
"#;
        let hints = hints_for(source);
        // The closure should get a `-> i64` return type hint.
        let ret_hint = hints.iter().find(|h| {
            h.line == 3 && h.kind == InlayHintKind::ReturnType && h.label.contains("i64")
        });
        assert!(
            ret_hint.is_some(),
            "expected `-> i64` return type hint for closure, got: {hints:?}"
        );
    }

    #[test]
    fn multi_clause_function_shows_return_hint_for_each_clause() {
        let source = r#"
fn render(0) { 1 }
fn render(1) { 2 }
fn render(_) { 3 }
"#;
        let hints = hints_for(source);
        let return_hints: Vec<_> = hints
            .iter()
            .filter(|h| h.kind == InlayHintKind::ReturnType && h.label == " -> i64")
            .collect();

        assert_eq!(
            return_hints.len(),
            3,
            "expected one inferred return-type hint per clause, got: {hints:?}"
        );
        assert!(return_hints.iter().any(|h| h.line == 1));
        assert!(return_hints.iter().any(|h| h.line == 2));
        assert!(return_hints.iter().any(|h| h.line == 3));
    }

    #[test]
    fn multi_clause_function_shows_clause_local_return_hints() {
        let source = r#"
enum Tree {
    Empty,
    Node { value: isize, left: Tree, right: Tree },
}

fn Tree.to_list(Tree::Empty) { [] }
fn Tree.to_list(Tree::Node { value, left, right }) {
    [...left, value, ...right]
}
"#;
        let hints = hints_for(source);

        assert!(
            hints.iter().any(|h| {
                h.kind == InlayHintKind::ReturnType && h.line == 6 && h.label == " -> List"
            }),
            "expected empty-clause return hint `-> List`, got: {hints:?}"
        );
        assert!(
            hints.iter().any(|h| {
                h.kind == InlayHintKind::ReturnType && h.line == 7 && h.label == " -> List<isize>"
            }),
            "expected node-clause return hint `-> List<isize>`, got: {hints:?}"
        );
    }

    #[test]
    fn unconstrained_multi_clause_function_has_no_return_hint() {
        let source = r#"
fn foldl([], acc, _) { acc }
fn foldl([x, ...xs], acc, f) { rec foldl(xs, f(acc, x), f) }
"#;
        let hints = hints_for(source);

        assert!(
            !hints.iter().any(|h| h.kind == InlayHintKind::ReturnType),
            "expected no return-type hints for unconstrained foldl, got: {hints:?}"
        );
    }

    #[test]
    fn synthetic_for_loop_bindings_do_not_emit_origin_hints() {
        let source = r#"
let total = for x in [1,2,3]; with sum = 0 {
    sum + x
};
let evens = for x in [1,2,3]; with acc = [] {
    if x % 2 == 0 { [...acc, x] } else { acc }
};
"#;
        let hints = hints_for(source);

        assert!(
            !hints.iter().any(|h| h.line == 0 && h.character == 0),
            "expected no hints at the file origin from synthetic loop bindings, got: {hints:?}"
        );
    }

    #[test]
    fn loop_binding_hint_comes_from_iterable_rhs() {
        let source = r#"
let total = for x in [1,2,3]; with sum = 0 as isize {
    sum
};
"#;
        let hints = hints_for(source);

        assert!(
            hints.iter().any(|h| h.label == ": i64"),
            "expected loop binding `x` to get `: i64` from the iterable rhs, got: {hints:?}"
        );
    }

    #[test]
    fn loop_binding_hint_ignores_body_and_accumulator_types() {
        let source = r#"
enum Tree {
    Empty,
    Node { value: isize, left: Tree, right: Tree },
}

impl Iterable<isize> for Tree {
    fn iter(self) {
        Iterable::iter([])
    }
}

let total = for x in Tree::Empty; with sum = 0 as isize {
    sum + x
};
"#;
        let hints = hints_for(source);

        assert!(
            hints.iter().any(|h| h.label == ": isize"),
            "expected loop binding `x` to get `: isize` from Iterable<isize>, got: {hints:?}"
        );
        assert!(
            !hints.iter().any(|h| h.label == ": i64"),
            "loop binding should ignore accumulator/body literals, got: {hints:?}"
        );
    }

    #[test]
    fn loop_binding_hint_for_typed_iterable_variable() {
        let source = r#"
enum Tree {
    Empty,
    Node { value: isize, left: Tree, right: Tree },
}

impl Iterable<isize> for Tree {
    fn iter(self) {
        Iterable::iter([])
    }
}

let tree = Tree::Empty;
let total = for x in tree; with sum = 0 as isize {
    sum
};
"#;
        let hints = hints_for(source);
        let x_hint = hints.iter().find(|h| {
            h.line == 13
                && h.character == 17
                && h.label == ": isize"
                && h.kind == InlayHintKind::Type
        });

        assert!(
            x_hint.is_some(),
            "expected the loop item binding `x` to get `: isize`, got: {hints:?}"
        );
    }

    #[test]
    fn loop_result_hint_uses_coerced_numeric_type() {
        let source = r#"
enum Tree {
    Empty,
    Node { value: isize, left: Tree, right: Tree },
}

impl Iterable<isize> for Tree {
    fn iter(self) {
        Iterable::iter([])
    }
}

let tree = Tree::Empty;
let total = for x in tree; with sum = 0 {
    sum + x
};
"#;
        let hints = hints_for(source);
        let total_hint = hints.iter().find(|h| {
            h.line == 13 && h.character == 9 && h.label == ": i64" && h.kind == InlayHintKind::Type
        });

        assert!(
            total_hint.is_some(),
            "expected the loop result binding `total` to get the coerced numeric type, got: {hints:?}"
        );
    }

    #[test]
    fn loop_accumulator_hint_uses_outer_expected_list_type() {
        let source = r#"
enum Tree {
    Empty,
    Node { value: isize, left: Tree, right: Tree },
}

impl Iterable<isize> for Tree {
    fn iter(self) {
        Iterable::iter([])
    }
}

let tree = Tree::Empty;
let evens: List<isize> = for x in tree; with acc = [] {
    if x % 2 == 0 { [...acc, x] } else { acc }
};
"#;
        let hints = hints_for(source);
        let acc_hint = hints
            .iter()
            .find(|h| h.line == 13 && h.label == ": List<isize>" && h.kind == InlayHintKind::Type);

        assert!(
            acc_hint.is_some(),
            "expected the loop accumulator binding `acc` to inherit the annotated result type, got: {hints:?}"
        );
    }

    #[test]
    fn type_at_definition_for_loop_accumulator_uses_outer_expected_list_type() {
        let source = r#"
enum Tree {
    Empty,
    Node { value: isize, left: Tree, right: Tree },
}

impl Iterable<isize> for Tree {
    fn iter(self) {
        Iterable::iter([])
    }
}

let tree = Tree::Empty;
let evens: List<isize> = for x in tree; with acc = [] {
    if x % 2 == 0 { [...acc, x] } else { acc }
};
        "#;
        let typed_hir = typed_hir_for(source);
        let ty = type_at_definition(&typed_hir, 13, 45);
        assert_eq!(ty.as_deref(), Some("List<isize>"));
    }

    #[test]
    fn loop_binding_hint_for_typed_iterable_statement_loop() {
        let source = r#"
enum Tree {
    Empty,
    Node { value: isize, left: Tree, right: Tree },
}

impl Iterable<isize> for Tree {
    fn iter(self) {
        Iterable::iter(self.to_list())
    }
}

fn Tree.to_list(Tree::Empty) { [] }
fn Tree.to_list(Tree::Node { value, left, right }) {
    [...left, value, ...right]
}

let tree = Tree::Empty;
for x in tree {
    x |> log();
};
"#;
        let hints = hints_for(source);
        let x_hint = hints
            .iter()
            .find(|h| h.line == 18 && h.character == 5 && h.label == ": isize");

        assert!(
            x_hint.is_some(),
            "expected bare loop item binding `x` to get `: isize`, got: {hints:?}"
        );
    }

    #[test]
    fn type_at_definition_for_typed_iterable_statement_loop_item() {
        let source = r#"
enum Tree {
    Empty,
    Node { value: isize, left: Tree, right: Tree },
}

impl Iterable<isize> for Tree {
    fn iter(self) {
        Iterable::iter(self.to_list())
    }
}

fn Tree.to_list(Tree::Empty) { [] }
fn Tree.to_list(Tree::Node { value, left, right }) {
    [...left, value, ...right]
}

let tree = Tree::Empty;
for x in tree {
    x |> log();
};
"#;
        let typed_hir = typed_hir_for(source);
        let ty = type_at_definition(&typed_hir, 18, 4);
        assert_eq!(ty.as_deref(), Some("isize"));
    }

    #[test]
    fn hover_on_closure_fn_keyword_shows_signature() {
        // Hovering on the `fn` keyword of a closure should show its full type.
        let source = r#"
fn map<T, U>([]: List<T>, _: fn(T) -> U) -> List<U> { [] }
fn map<T, U>([x, ...xs]: List<T>, f: fn(T) -> U) -> List<U> { [f(x), ...map(xs, f)] }
[1,2,3] |> map(fn (x) { x ** 2 });
"#;
        let typed_hir = typed_hir_for(source);
        // `fn` keyword is at line 3, col 15 (0-based).
        let ty = type_at_definition(&typed_hir, 3, 15);
        assert_eq!(
            ty.as_deref(),
            Some("fn(i64) -> i64"),
            "hover on closure `fn` should show full typed signature"
        );
    }

    #[test]
    fn hover_on_closure_inside_method_body() {
        // Hovering on the `fn` keyword of a closure nested inside a
        // dot-method body should still work.
        let source = r#"
struct Post { title: String }
fn map<T, U>([]: List<T>, _: fn(T) -> U) -> List<U> { [] }
fn map<T, U>([x, ...xs]: List<T>, f: fn(T) -> U) -> List<U> { [f(x), ...map(xs, f)] }
fn Post.render(Post { title }) {
    [title] |> map(fn(s) { s })
}
"#;
        let typed_hir = typed_hir_for(source);
        // Line 5 (0-based): `    [title] |> map(fn(s) { s })`
        // `fn` keyword — find its 0-based column.
        let ty = type_at_definition(&typed_hir, 5, 19);
        assert!(
            ty.is_some(),
            "hover on closure `fn` inside method body should show type, got None"
        );
    }
}
