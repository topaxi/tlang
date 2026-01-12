use super::CompileTarget;
use crate::error::ParserError;

pub struct HirTarget;

impl CompileTarget for HirTarget {
    fn compile(
        &self,
        _source: &str,
        module: &mut tlang_hir::hir::Module,
    ) -> Result<String, ParserError> {
        Ok(ron::ser::to_string_pretty(module, ron::ser::PrettyConfig::default()).unwrap())
    }
}
