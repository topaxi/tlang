use tlang_hir_pretty::HirPretty;

use super::CompileTarget;
use crate::error::ParserError;

pub struct HirTarget;

impl CompileTarget for HirTarget {
    fn compile(
        &self,
        _source: &str,
        module: &mut tlang_hir::Module,
    ) -> Result<String, ParserError> {
        let mut printer = HirPretty::default();
        printer.print_module(module);
        Ok(printer.output().to_string())
    }
}
