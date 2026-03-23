use super::CompileTarget;
use tlang_codegen_js::generator::CodegenJS;
use tlang_diagnostics::Diagnostic;

pub struct JsTarget;

impl CompileTarget for JsTarget {
    fn compile(
        &self,
        _source: &str,
        module: &mut tlang_hir::Module,
    ) -> Result<String, Vec<Diagnostic>> {
        let mut generator = CodegenJS::default();
        generator.generate_code(module);
        Ok(generator.get_output().to_string())
    }
}
