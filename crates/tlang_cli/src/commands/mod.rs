pub mod build;
pub mod compile;
pub mod error;
pub mod run;

use tlang_diagnostics::{Diagnostic, render_diagnostics};
use tlang_hir as hir;
use tlang_hir_opt::HirPass;
use tlang_hir_opt::hir_opt::{HirOptContext, HirOptError};

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

pub fn run_hir_passes<P: HirPass>(
    optimizer: &mut P,
    module: &mut hir::Module,
    ctx: &mut HirOptContext,
) -> Result<(), HirOptError> {
    optimizer.optimize_hir(module, ctx)?;
    Ok(())
}
