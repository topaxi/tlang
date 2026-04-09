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

use std::collections::HashMap;
use std::sync::{Arc, RwLock};

use tlang_defs::DefScope;
use tlang_hir as hir;
use tlang_hir::TyKind;
use tlang_span::{LineColumn, NodeId, Span};
use tlang_typeck::TypeChecker;
use tlang_typeck::TypeTable;

use crate::AnalysisResult;

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
}

/// The result of running the full type-checking pipeline: a typed HIR module
/// together with the [`TypeTable`] produced by the type checker.
pub struct TypedHir {
    pub module: hir::Module,
    pub type_table: TypeTable,
}

/// Run the full pipeline (HIR lowering → optimisation → type checking) on an
/// [`AnalysisResult`] and return the typed HIR.
///
/// Returns `None` when the analysis result has no parsed module or when HIR
/// lowering fails.
///
/// # Panics
///
/// Panics if a symbol table `RwLock` is poisoned (should not happen in
/// single-threaded analysis).
pub fn lower_and_typecheck(result: &AnalysisResult) -> Option<TypedHir> {
    let ast = result.module.as_ref()?;
    let parse_meta = result.parse_meta.as_ref()?;

    // Clone symbol tables so the lowering pass can mutate them without
    // corrupting the analyzer's state.
    let symbol_tables: HashMap<NodeId, Arc<RwLock<DefScope>>> = result
        .analyzer
        .symbol_tables()
        .iter()
        .map(|(&k, v)| (k, Arc::new(RwLock::new(v.read().unwrap().clone()))))
        .collect();

    let (mut module, meta) = tlang_ast_lowering::lower_to_hir(
        ast,
        &parse_meta.constant_pool_node_ids,
        result.analyzer.symbol_id_allocator(),
        result.analyzer.root_symbol_table(),
        symbol_tables,
    )
    .ok()?;

    let mut ctx: tlang_hir_opt::hir_opt::HirOptContext = meta.into();

    // Run default optimisations without dead code elimination — DCE would
    // remove unused bindings before the type checker can inspect them.
    let mut optimizer = tlang_hir_opt::hir_opt::HirOptimizer::from(
        tlang_hir_opt::DefaultOptimizations::default().without("DeadCodeElimination"),
    );
    optimizer.optimize_hir(&mut module, &mut ctx).ok()?;

    // Run the type checker.
    let mut tc = TypeChecker::new();
    use tlang_hir_opt::HirPass;
    tc.optimize_hir(&mut module, &mut ctx).ok()?;

    Some(TypedHir {
        module,
        type_table: tc.type_table,
    })
}

/// Collect inlay hints from a type-checked HIR module.
///
/// If `range` is `Some`, only hints within the given line range
/// `[start_line, end_line]` (inclusive, 0-based) are returned.
pub fn collect_inlay_hints(typed_hir: &TypedHir, range: Option<(u32, u32)>) -> Vec<InlayHint> {
    let mut hints = Vec::new();
    collect_block_hints(
        &typed_hir.module.block,
        &typed_hir.type_table,
        range,
        &mut hints,
    );
    hints
}

// ── Block / statement traversal ────────────────────────────────────────

fn collect_block_hints(
    block: &hir::Block,
    type_table: &TypeTable,
    range: Option<(u32, u32)>,
    hints: &mut Vec<InlayHint>,
) {
    for stmt in &block.stmts {
        collect_stmt_hints(stmt, type_table, range, hints);
    }
    if let Some(expr) = &block.expr {
        collect_expr_hints(expr, type_table, range, hints);
    }
}

fn collect_stmt_hints(
    stmt: &hir::Stmt,
    type_table: &TypeTable,
    range: Option<(u32, u32)>,
    hints: &mut Vec<InlayHint>,
) {
    match &stmt.kind {
        hir::StmtKind::Let(pat, expr, ty) => {
            // Recurse into the initialiser expression.
            collect_expr_hints(expr, type_table, range, hints);

            // Show a type hint when no explicit annotation was written.
            if matches!(ty.kind, TyKind::Unknown) && !matches!(pat.ty.kind, TyKind::Unknown) {
                collect_pat_type_hints(pat, range, hints);
            }
        }
        hir::StmtKind::Const(_, pat, expr, ty) => {
            collect_expr_hints(expr, type_table, range, hints);

            if matches!(ty.kind, TyKind::Unknown) && !matches!(pat.ty.kind, TyKind::Unknown) {
                collect_pat_type_hints(pat, range, hints);
            }
        }
        hir::StmtKind::FunctionDeclaration(decl) => {
            collect_fn_decl_hints(decl, type_table, range, hints);
        }
        hir::StmtKind::DynFunctionDeclaration(_) => {
            // Dynamic dispatch wrappers — no hints needed.
        }
        hir::StmtKind::Return(expr) => {
            if let Some(e) = expr {
                collect_expr_hints(e, type_table, range, hints);
            }
        }
        hir::StmtKind::Expr(expr) => {
            collect_expr_hints(expr, type_table, range, hints);
        }
        hir::StmtKind::EnumDeclaration(_)
        | hir::StmtKind::StructDeclaration(_)
        | hir::StmtKind::ProtocolDeclaration(_)
        | hir::StmtKind::ImplBlock(_) => {
            // Type declarations — no hints needed.
        }
    }
}

// ── Function declaration hints ─────────────────────────────────────────

fn collect_fn_decl_hints(
    decl: &hir::FunctionDeclaration,
    type_table: &TypeTable,
    range: Option<(u32, u32)>,
    hints: &mut Vec<InlayHint>,
) {
    // Parameter type hints — show when the user didn't annotate a type.
    for param in &decl.parameters {
        if matches!(param.type_annotation.kind, TyKind::Unknown) {
            // The type checker may not infer parameter types yet, so this
            // shows `: unknown` for now.  As inference improves, concrete
            // types will appear automatically.
            let pos = param.name.span.end_lc;
            push_hint(
                hints,
                range,
                pos,
                format!(": {}", param.type_annotation.kind),
                InlayHintKind::Type,
            );
        }
    }

    // Return type hint — show when no explicit return type was written.
    if matches!(decl.return_type.kind, TyKind::Unknown) {
        // Try to get the inferred return type from the type table.
        let inferred_ret = type_table
            .get(&decl.hir_id)
            .and_then(|info| match &info.ty.kind {
                TyKind::Fn(_, ret) => Some(&ret.kind),
                _ => None,
            })
            .or_else(|| decl.body.expr.as_ref().map(|e| &e.ty.kind));

        if let Some(ret_kind) = inferred_ret
            && !matches!(ret_kind, TyKind::Unknown)
        {
            // Find the position after the closing `)` of the parameter list.
            // Use the span of the last parameter, or the function name span.
            let pos = return_type_hint_position(decl);
            push_hint(
                hints,
                range,
                pos,
                format!(" -> {ret_kind}"),
                InlayHintKind::ReturnType,
            );
        }
    }

    // Recurse into the function body.
    collect_block_hints(&decl.body, type_table, range, hints);
}

/// Determine the position for a return type hint.
///
/// The hint is placed one character before the opening `{` of the body
/// (i.e. on the space that precedes it).  Combined with a leading-space
/// label (`" -> T"`), the background of the hint starts right after `)`
/// — visually matching the `: type` parameter hints — and the uncoloured
/// source space after the hint provides natural separation before `{`.
///
/// For C-style brace placement the hint lands at the end of the closing
/// `)` line, which is equally readable.
fn return_type_hint_position(decl: &hir::FunctionDeclaration) -> LineColumn {
    let lc = decl.body.span.start_lc;
    // Move one character to the left.  The column convention is:
    //   line 0 → 0-based,  line > 0 → 1-based (lexer convention).
    // Subtracting 1 works uniformly: for line 0 it steps back one 0-based
    // column; for line > 0 it steps back one 1-based column (push_hint
    // then normalises to 0-based).
    LineColumn::new(lc.line, lc.column.saturating_sub(1))
}

// ── Expression traversal (recurse into nested functions / blocks) ──────

fn collect_expr_hints(
    expr: &hir::Expr,
    type_table: &TypeTable,
    range: Option<(u32, u32)>,
    hints: &mut Vec<InlayHint>,
) {
    match &expr.kind {
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            collect_block_hints(block, type_table, range, hints);
        }
        hir::ExprKind::FunctionExpression(decl) => {
            collect_fn_decl_hints(decl, type_table, range, hints);
        }
        hir::ExprKind::IfElse(cond, then_block, else_clauses) => {
            collect_expr_hints(cond, type_table, range, hints);
            collect_block_hints(then_block, type_table, range, hints);
            for clause in else_clauses {
                if let Some(cond) = &clause.condition {
                    collect_expr_hints(cond, type_table, range, hints);
                }
                collect_block_hints(&clause.consequence, type_table, range, hints);
            }
        }
        hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
            collect_expr_hints(&call.callee, type_table, range, hints);
            for arg in &call.arguments {
                collect_expr_hints(arg, type_table, range, hints);
            }
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            collect_expr_hints(lhs, type_table, range, hints);
            collect_expr_hints(rhs, type_table, range, hints);
        }
        hir::ExprKind::Unary(_, operand) => {
            collect_expr_hints(operand, type_table, range, hints);
        }
        hir::ExprKind::Let(_, expr) => {
            collect_expr_hints(expr, type_table, range, hints);
        }
        hir::ExprKind::Cast(expr, _) | hir::ExprKind::TryCast(expr, _) => {
            collect_expr_hints(expr, type_table, range, hints);
        }
        hir::ExprKind::List(items) => {
            for item in items {
                collect_expr_hints(item, type_table, range, hints);
            }
        }
        hir::ExprKind::Dict(entries) => {
            for (k, v) in entries {
                collect_expr_hints(k, type_table, range, hints);
                collect_expr_hints(v, type_table, range, hints);
            }
        }
        hir::ExprKind::FieldAccess(base, _) => {
            collect_expr_hints(base, type_table, range, hints);
        }
        hir::ExprKind::IndexAccess(base, index) => {
            collect_expr_hints(base, type_table, range, hints);
            collect_expr_hints(index, type_table, range, hints);
        }
        hir::ExprKind::Match(scrutinee, arms) => {
            collect_expr_hints(scrutinee, type_table, range, hints);
            for arm in arms {
                if let Some(guard) = &arm.guard {
                    collect_expr_hints(guard, type_table, range, hints);
                }
                collect_block_hints(&arm.block, type_table, range, hints);
            }
        }
        hir::ExprKind::Break(expr) => {
            if let Some(e) = expr {
                collect_expr_hints(e, type_table, range, hints);
            }
        }
        hir::ExprKind::Range(range_expr) => {
            collect_expr_hints(&range_expr.start, type_table, range, hints);
            collect_expr_hints(&range_expr.end, type_table, range, hints);
        }
        hir::ExprKind::TaggedString { tag, exprs, .. } => {
            collect_expr_hints(tag, type_table, range, hints);
            for e in exprs {
                collect_expr_hints(e, type_table, range, hints);
            }
        }
        hir::ExprKind::Implements(expr, _) => {
            collect_expr_hints(expr, type_table, range, hints);
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
fn collect_pat_type_hints(pat: &hir::Pat, range: Option<(u32, u32)>, hints: &mut Vec<InlayHint>) {
    match &pat.kind {
        hir::PatKind::Identifier(_, ident) => {
            // Skip wildcard bindings (`_`).
            if ident.as_str() == "_" {
                return;
            }
            let pos = ident.span.end_lc;
            push_hint(
                hints,
                range,
                pos,
                format!(": {}", pat.ty.kind),
                InlayHintKind::Type,
            );
        }
        hir::PatKind::List(pats) => {
            // For list destructuring, emit hints for each sub-pattern.
            for sub_pat in pats {
                if !matches!(sub_pat.ty.kind, TyKind::Unknown) {
                    collect_pat_type_hints(sub_pat, range, hints);
                }
            }
        }
        hir::PatKind::Rest(inner) => {
            if !matches!(inner.ty.kind, TyKind::Unknown) {
                collect_pat_type_hints(inner, range, hints);
            }
        }
        hir::PatKind::Enum(_, fields) => {
            for (_, field_pat) in fields {
                if !matches!(field_pat.ty.kind, TyKind::Unknown) {
                    collect_pat_type_hints(field_pat, range, hints);
                }
            }
        }
        // Wildcards and literal patterns don't need hints.
        hir::PatKind::Wildcard | hir::PatKind::Literal(_) => {}
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
        def_line,
        lexer_col,
        &mut result,
    );
    result
}

fn walk_block_for_type(
    block: &hir::Block,
    type_table: &TypeTable,
    line: u32,
    col: u32,
    out: &mut Option<String>,
) {
    for stmt in &block.stmts {
        if out.is_some() {
            return;
        }
        walk_stmt_for_type(stmt, type_table, line, col, out);
    }
}

fn walk_stmt_for_type(
    stmt: &hir::Stmt,
    type_table: &TypeTable,
    line: u32,
    col: u32,
    out: &mut Option<String>,
) {
    match &stmt.kind {
        hir::StmtKind::Expr(e) => walk_expr_for_type(e, type_table, line, col, out),
        hir::StmtKind::Let(pat, init, _ty) | hir::StmtKind::Const(_, pat, init, _ty) => {
            check_pat_for_type(pat, line, col, out);
            if out.is_none() {
                walk_expr_for_type(init, type_table, line, col, out);
            }
        }
        hir::StmtKind::FunctionDeclaration(decl) => {
            walk_fn_decl_for_type(decl, type_table, line, col, out);
        }
        hir::StmtKind::StructDeclaration(decl) => {
            let name_span = &decl.name.span;
            if name_span.start_lc.line == line && name_span.start_lc.column == col {
                *out = Some(decl.name.to_string());
            }
        }
        hir::StmtKind::ImplBlock(impl_block) => {
            for decl in &impl_block.methods {
                walk_fn_decl_for_type(decl, type_table, line, col, out);
            }
        }
        hir::StmtKind::EnumDeclaration(decl) => {
            let name_span = &decl.name.span;
            if name_span.start_lc.line == line && name_span.start_lc.column == col {
                *out = Some(decl.name.to_string());
            }
        }
        hir::StmtKind::Return(Some(e)) => walk_expr_for_type(e, type_table, line, col, out),
        _ => {}
    }
}

fn walk_fn_decl_for_type(
    decl: &hir::FunctionDeclaration,
    type_table: &TypeTable,
    line: u32,
    col: u32,
    out: &mut Option<String>,
) {
    if out.is_some() {
        return;
    }

    // Check function name.
    let name_span = &decl.name.span;
    if name_span.start_lc.line == line && name_span.start_lc.column == col {
        // Build function type signature from parameters and return type.
        let params: Vec<String> = decl
            .parameters
            .iter()
            .map(|p| format!("{}", p.type_annotation.kind))
            .collect();
        let ret = &decl.return_type.kind;
        *out = Some(format!("fn({}) -> {ret}", params.join(", ")));
        return;
    }

    // Check each parameter.
    for param in &decl.parameters {
        if param.name.span.start_lc.line == line && param.name.span.start_lc.column == col {
            let ty = &param.type_annotation.kind;
            if !matches!(ty, TyKind::Unknown) {
                *out = Some(format!("{ty}"));
            }
            return;
        }
    }

    // Recurse into the body.
    walk_block_for_type(&decl.body, type_table, line, col, out);
}

fn check_pat_for_type(pat: &hir::Pat, line: u32, col: u32, out: &mut Option<String>) {
    match &pat.kind {
        hir::PatKind::Identifier(_, ident)
            if ident.span.start_lc.line == line && ident.span.start_lc.column == col =>
        {
            let ty = &pat.ty.kind;
            if !matches!(ty, TyKind::Unknown) {
                *out = Some(format!("{ty}"));
            }
        }
        hir::PatKind::List(pats) => {
            for p in pats {
                check_pat_for_type(p, line, col, out);
                if out.is_some() {
                    return;
                }
            }
        }
        hir::PatKind::Rest(inner) => check_pat_for_type(inner, line, col, out),
        hir::PatKind::Enum(_, fields) => {
            for (_, p) in fields {
                check_pat_for_type(p, line, col, out);
                if out.is_some() {
                    return;
                }
            }
        }
        _ => {}
    }
}

/// Report the type of `expr` into `out` when the cursor is exactly on `span`.
fn report_ty_at(expr: &hir::Expr, span: &Span, line: u32, col: u32, out: &mut Option<String>) {
    if span.start_lc.line == line && span.start_lc.column == col {
        let ty = &expr.ty.kind;
        if !matches!(ty, TyKind::Unknown) {
            *out = Some(format!("{ty}"));
        }
    }
}

fn walk_expr_for_type(
    expr: &hir::Expr,
    type_table: &TypeTable,
    line: u32,
    col: u32,
    out: &mut Option<String>,
) {
    if out.is_some() {
        return;
    }

    match &expr.kind {
        hir::ExprKind::Let(pat, init) => {
            check_pat_for_type(pat, line, col, out);
            if out.is_none() {
                walk_expr_for_type(init, type_table, line, col, out);
            }
        }
        hir::ExprKind::Block(block) | hir::ExprKind::Loop(block) => {
            walk_block_for_type(block, type_table, line, col, out);
        }
        hir::ExprKind::FunctionExpression(decl) => {
            walk_fn_decl_for_type(decl, type_table, line, col, out);
        }
        hir::ExprKind::IfElse(cond, then_block, else_clauses) => {
            walk_expr_for_type(cond, type_table, line, col, out);
            walk_block_for_type(then_block, type_table, line, col, out);
            for clause in else_clauses {
                if let Some(cond) = &clause.condition {
                    walk_expr_for_type(cond, type_table, line, col, out);
                }
                walk_block_for_type(&clause.consequence, type_table, line, col, out);
            }
        }
        hir::ExprKind::Call(call) | hir::ExprKind::TailCall(call) => {
            walk_expr_for_type(&call.callee, type_table, line, col, out);
            for arg in &call.arguments {
                walk_expr_for_type(arg, type_table, line, col, out);
            }
        }
        hir::ExprKind::Binary(_, lhs, rhs) => {
            walk_expr_for_type(lhs, type_table, line, col, out);
            walk_expr_for_type(rhs, type_table, line, col, out);
        }
        hir::ExprKind::Unary(_, operand) => {
            walk_expr_for_type(operand, type_table, line, col, out);
        }
        hir::ExprKind::Match(scrutinee, arms) => {
            walk_expr_for_type(scrutinee, type_table, line, col, out);
            for arm in arms {
                check_pat_for_type(&arm.pat, line, col, out);
                if out.is_some() {
                    return;
                }
                if let Some(guard) = &arm.guard {
                    walk_expr_for_type(guard, type_table, line, col, out);
                }
                walk_block_for_type(&arm.block, type_table, line, col, out);
            }
        }
        hir::ExprKind::Path(path) => {
            report_ty_at(expr, &expr.span, line, col, out);
            for seg in &path.segments {
                report_ty_at(expr, &seg.ident.span, line, col, out);
            }
        }
        hir::ExprKind::FieldAccess(base, ident) => {
            walk_expr_for_type(base, type_table, line, col, out);
            report_ty_at(expr, &ident.span, line, col, out);
        }
        hir::ExprKind::IndexAccess(base, index) => {
            walk_expr_for_type(base, type_table, line, col, out);
            walk_expr_for_type(index, type_table, line, col, out);
        }
        hir::ExprKind::Cast(inner, _) | hir::ExprKind::TryCast(inner, _) => {
            walk_expr_for_type(inner, type_table, line, col, out);
        }
        hir::ExprKind::List(items) => {
            for item in items {
                walk_expr_for_type(item, type_table, line, col, out);
            }
        }
        hir::ExprKind::Dict(entries) => {
            for (k, v) in entries {
                walk_expr_for_type(k, type_table, line, col, out);
                walk_expr_for_type(v, type_table, line, col, out);
            }
        }
        hir::ExprKind::Break(e) => {
            if let Some(e) = e {
                walk_expr_for_type(e, type_table, line, col, out);
            }
        }
        hir::ExprKind::Range(r) => {
            walk_expr_for_type(&r.start, type_table, line, col, out);
            walk_expr_for_type(&r.end, type_table, line, col, out);
        }
        hir::ExprKind::TaggedString { tag, exprs, .. } => {
            walk_expr_for_type(tag, type_table, line, col, out);
            for e in exprs {
                walk_expr_for_type(e, type_table, line, col, out);
            }
        }
        hir::ExprKind::Implements(inner, _) => {
            walk_expr_for_type(inner, type_table, line, col, out);
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
    fn function_with_explicit_return_type_no_hint() {
        let hints = hints_for("fn add(a: i64, b: i64) -> i64 { a + b }");
        assert!(
            !hints.iter().any(|h| h.kind == InlayHintKind::ReturnType),
            "should not show return type hint when explicitly annotated, got: {hints:?}"
        );
    }

    #[test]
    fn function_parameter_unknown_type_hint() {
        let hints = hints_for("fn double(x) { x }");
        assert!(
            hints
                .iter()
                .any(|h| h.label == ": unknown" && h.kind == InlayHintKind::Type),
            "expected `: unknown` hint for un-annotated parameter, got: {hints:?}"
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
}
