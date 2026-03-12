use super::CompileTarget;
use crate::error::ParserError;

pub struct HirRawTarget;

impl CompileTarget for HirRawTarget {
    fn compile(
        &self,
        _source: &str,
        module: &mut tlang_hir::Module,
    ) -> Result<String, ParserError> {
        Ok(ron::ser::to_string_pretty(module, ron::ser::PrettyConfig::default()).unwrap())
    }
}
