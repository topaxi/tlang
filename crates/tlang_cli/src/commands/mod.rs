pub mod build;
pub mod compile;
pub mod error;
pub mod run;

use tlang_diagnostics::{Diagnostic, render_diagnostics};
use tlang_hir as hir;
use tlang_hir_opt::HirPass;
use tlang_hir_opt::hir_opt::{HirOptContext, HirOptError};
use tlang_typeck::TypecheckDiagnostics;

pub fn render_source_diagnostics(
    source_name: &str,
    source: &str,
    diagnostics: &[Diagnostic],
) -> String {
    render_diagnostics(
        source_name,
        source,
        diagnostics,
        std::io::IsTerminal::is_terminal(&std::io::stderr()),
    )
}

pub fn print_source_diagnostics(source_name: &str, source: &str, diagnostics: &[Diagnostic]) {
    if diagnostics.is_empty() {
        return;
    }

    eprint!(
        "{}",
        render_source_diagnostics(source_name, source, diagnostics)
    );
}

pub fn optimize_and_typecheck<P: HirPass>(
    optimizer: &mut P,
    module: &mut hir::Module,
    ctx: &mut HirOptContext,
) -> Result<TypecheckDiagnostics, HirOptError> {
    optimizer.optimize_hir(module, ctx)?;
    tlang_typeck::typecheck_module(module, ctx)
}
