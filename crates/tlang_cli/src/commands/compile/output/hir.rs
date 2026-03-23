use tlang_hir_pretty::HirPretty;

use super::CompileTarget;
use tlang_diagnostics::Diagnostic;

pub struct HirTarget;

impl CompileTarget for HirTarget {
    fn compile(
        &self,
        _source: &str,
        module: &mut tlang_hir::Module,
    ) -> Result<String, Vec<Diagnostic>> {
        let mut printer = HirPretty::default();
        printer.print_module(module);
        Ok(printer.output().to_string())
    }
}
