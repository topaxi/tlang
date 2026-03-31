pub mod ast;
pub mod hir;
pub mod hir_raw;
pub mod js;

use tlang_codegen_js::{CodegenError, CodegenWarning};
use tlang_diagnostics::{Diagnostic, Severity};

pub trait CompileTarget {
    fn compile(
        &self,
        source: &str,
        module: &mut tlang_hir::Module,
    ) -> Result<(String, Vec<Diagnostic>), Vec<Diagnostic>>;
}

/// Convert a list of [`CodegenError`]s into [`Diagnostic`]s.
pub(crate) fn codegen_errors_to_diagnostics(errors: Vec<CodegenError>) -> Vec<Diagnostic> {
    errors
        .into_iter()
        .map(|e| Diagnostic::new(Severity::Error, e.message, e.span))
        .collect()
}

/// Convert a list of [`CodegenWarning`]s into [`Diagnostic`]s.
pub(crate) fn codegen_warnings_to_diagnostics(warnings: Vec<CodegenWarning>) -> Vec<Diagnostic> {
    warnings
        .into_iter()
        .map(|w| Diagnostic::new(Severity::Warning, w.message, w.span))
        .collect()
}
