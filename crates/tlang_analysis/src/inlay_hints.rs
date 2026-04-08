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
use tlang_span::{LineColumn, NodeId};
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
pub fn collect_inlay_hints(
    typed_hir: &TypedHir,
    range: Option<(u32, u32)>,
) -> Vec<InlayHint> {
    let mut hints = Vec::new();
    collect_block_hints(&typed_hir.module.block, &typed_hir.type_table, range, &mut hints);
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

        if let Some(ret_kind) = inferred_ret {
            if !matches!(ret_kind, TyKind::Unknown) {
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
    }

    // Recurse into the function body.
    collect_block_hints(&decl.body, type_table, range, hints);
}

/// Determine the position for a return type hint.
///
/// The hint should appear after the closing `)` of the parameter list,
/// which is right before the function body `{`.  We approximate this as
/// the start of the body block's span minus one character (the `{`), but
/// in practice we use the body span start since the hint goes *before*
/// the body.
fn return_type_hint_position(decl: &hir::FunctionDeclaration) -> LineColumn {
    // The body block span starts at `{`. The hint should go just before
    // it. We use body.span.start_lc as a reasonable approximation.
    // The LSP client will render the hint inline at this position.
    decl.body.span.start_lc
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
fn collect_pat_type_hints(
    pat: &hir::Pat,
    range: Option<(u32, u32)>,
    hints: &mut Vec<InlayHint>,
) {
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
fn push_hint(
    hints: &mut Vec<InlayHint>,
    range: Option<(u32, u32)>,
    pos: LineColumn,
    label: String,
    kind: InlayHintKind,
) {
    if let Some((start, end)) = range {
        if pos.line < start || pos.line > end {
            return;
        }
    }
    hints.push(InlayHint {
        line: pos.line,
        character: pos.column,
        label,
        kind,
    });
}

#[cfg(test)]
mod tests {
    use super::*;

    /// Helper: run the full pipeline on source code and collect inlay hints.
    fn hints_for(source: &str) -> Vec<InlayHint> {
        let result = crate::analyze(source, |_| {});
        assert!(result.module.is_some(), "parsing failed: {:?}", result.parse_issues);
        assert!(result.parse_meta.is_some(), "parse_meta is None");
        let typed_hir = lower_and_typecheck(&result).expect("HIR lowering/typechecking should succeed");
        collect_inlay_hints(&typed_hir, None)
    }

    #[test]
    fn let_binding_with_integer_literal() {
        let hints = hints_for("let x = 42;");
        assert!(
            hints.iter().any(|h| h.label == ": i64" && h.kind == InlayHintKind::Type),
            "expected `: i64` hint, got: {hints:?}"
        );
    }

    #[test]
    fn let_binding_with_string_literal() {
        let hints = hints_for("let name = \"hello\";");
        assert!(
            hints.iter().any(|h| h.label == ": String" && h.kind == InlayHintKind::Type),
            "expected `: String` hint, got: {hints:?}"
        );
    }

    #[test]
    fn let_binding_with_float_literal() {
        let hints = hints_for("let pi = 3.14;");
        assert!(
            hints.iter().any(|h| h.label == ": f64" && h.kind == InlayHintKind::Type),
            "expected `: f64` hint, got: {hints:?}"
        );
    }

    #[test]
    fn let_binding_with_bool_literal() {
        let hints = hints_for("let flag = true;");
        assert!(
            hints.iter().any(|h| h.label == ": bool" && h.kind == InlayHintKind::Type),
            "expected `: bool` hint, got: {hints:?}"
        );
    }

    #[test]
    fn const_binding_with_integer_literal() {
        let hints = hints_for("const MAX = 100;");
        assert!(
            hints.iter().any(|h| h.label == ": i64" && h.kind == InlayHintKind::Type),
            "expected `: i64` hint for const, got: {hints:?}"
        );
    }

    #[test]
    fn let_binding_with_explicit_annotation_no_hint() {
        let hints = hints_for("let x: i64 = 42;");
        // No type hint should be shown when the user explicitly annotated.
        assert!(
            !hints.iter().any(|h| h.kind == InlayHintKind::Type && h.label.contains("i64")),
            "should not show hint for explicitly annotated binding, got: {hints:?}"
        );
    }

    #[test]
    fn function_return_type_hint() {
        let hints = hints_for("fn add(a: i64, b: i64) { a + b }");
        assert!(
            hints.iter().any(|h| h.label.contains("-> i64") && h.kind == InlayHintKind::ReturnType),
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
            hints.iter().any(|h| h.label == ": unknown" && h.kind == InlayHintKind::Type),
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
            hints.iter().any(|h| h.label == ": i64" && h.kind == InlayHintKind::Type),
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
}
