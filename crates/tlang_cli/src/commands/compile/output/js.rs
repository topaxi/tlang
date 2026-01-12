use super::CompileTarget;
use crate::error::ParserError;
use tlang_codegen_js::generator::CodegenJS;

pub struct JsTarget;

impl CompileTarget for JsTarget {
    fn compile(
        &self,
        _source: &str,
        module: &mut tlang_hir::hir::Module,
    ) -> Result<String, ParserError> {
        let mut generator = CodegenJS::default();
        generator.generate_code(module);
        Ok(generator.get_output().to_string())
    }
}
