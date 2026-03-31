use super::{CompileTarget, codegen_errors_to_diagnostics, codegen_warnings_to_diagnostics};
use tlang_codegen_js::generator::CodegenJS;
use tlang_diagnostics::Diagnostic;

pub struct JsTarget;

impl CompileTarget for JsTarget {
    fn compile(
        &self,
        _source: &str,
        module: &mut tlang_hir::Module,
    ) -> Result<(String, Vec<Diagnostic>), Vec<Diagnostic>> {
        let mut generator = CodegenJS::default();
        generator
            .generate_code(module)
            .map_err(codegen_errors_to_diagnostics)?;
        let warnings = codegen_warnings_to_diagnostics(generator.get_warnings().to_vec());
        Ok((generator.get_output().to_string(), warnings))
    }
}
