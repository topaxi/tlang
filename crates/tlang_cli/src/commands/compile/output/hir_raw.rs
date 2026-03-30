use super::CompileTarget;
use tlang_diagnostics::Diagnostic;

pub struct HirRawTarget;

impl CompileTarget for HirRawTarget {
    fn compile(
        &self,
        _source: &str,
        module: &mut tlang_hir::Module,
    ) -> Result<(String, Vec<Diagnostic>), Vec<Diagnostic>> {
        Ok((
            ron::ser::to_string_pretty(module, ron::ser::PrettyConfig::default()).unwrap(),
            Vec::new(),
        ))
    }
}
