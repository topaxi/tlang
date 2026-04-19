//! Typed HIR: the result of running the full type-checking pipeline.
//!
//! This is the foundational data type that hover, completion, signature help,
//! inlay hints, and member resolution all depend on.  It bundles a typed HIR
//! module together with the [`TypeTable`] produced by the type checker.

use std::collections::{HashMap, HashSet};
use std::sync::{Arc, RwLock};

use tlang_defs::DefScope;
use tlang_diagnostics::Diagnostic;
use tlang_hir as hir;
use tlang_hir_opt::HirPass;
use tlang_span::{HirId, NodeId};
use tlang_typeck::TypeTable;
use tlang_typeck::typecheck_module;

use crate::AnalysisResult;

/// The result of running the full type-checking pipeline: a typed HIR module
/// together with the [`TypeTable`] produced by the type checker.
pub struct TypedHir {
    pub module: hir::Module,
    pub type_table: TypeTable,
    /// HirIds of `CallExpression` nodes that were desugared from `|>`.
    pub pipeline_call_ids: HashSet<HirId>,
    /// Diagnostics (errors and warnings) produced by both the type checker
    /// and post-type-check HIR passes (e.g. unused-symbol detection).
    pub diagnostics: Vec<Diagnostic>,
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

    // Extract pipeline provenance before converting meta into HirOptContext.
    let pipeline_call_ids = meta.pipeline_call_ids.clone();

    let mut ctx: tlang_hir_opt::hir_opt::HirOptContext = meta.into();

    // Run default optimisations without dead code elimination — DCE would
    // remove unused bindings before the type checker can inspect them.
    let mut optimizer = tlang_hir_opt::hir_opt::HirOptimizer::from(
        tlang_hir_opt::DefaultOptimizations::default().without("DeadCodeElimination"),
    );
    optimizer.optimize_hir(&mut module, &mut ctx).ok()?;

    // Run the type checker. Type-aware HIR passes should run after this phase.
    // Editor features still consume partially typed HIR even when diagnostics
    // are present; executable entry points gate on type errors separately.
    let typecheck_result = typecheck_module(&mut module, &mut ctx).ok()?;

    // Preserve typecheck diagnostics alongside post-typecheck HIR-pass diagnostics.
    let mut diagnostics = typecheck_result.errors;
    diagnostics.extend(typecheck_result.warnings);

    // Run unused-symbol detection now that type information is available.
    // This detects unused struct fields, dot-methods, and struct method
    // aliases that the AST-level VariableUsageValidator cannot resolve.
    let mut unused_detector = tlang_hir_opt::UnusedSymbolDetector::default();
    unused_detector.optimize_hir(&mut module, &mut ctx).ok()?;

    diagnostics.append(&mut ctx.diagnostics);

    Some(TypedHir {
        module,
        type_table: typecheck_result.type_table,
        pipeline_call_ids,
        diagnostics,
    })
}
