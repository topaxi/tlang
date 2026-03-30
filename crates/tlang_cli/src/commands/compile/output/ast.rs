use super::CompileTarget;
use tlang_diagnostics::{Diagnostic, diagnostics_from_parse_error};

pub struct AstTarget;

impl CompileTarget for AstTarget {
    fn compile(
        &self,
        source: &str,
        _module: &mut tlang_hir::Module,
    ) -> Result<(String, Vec<Diagnostic>), Vec<Diagnostic>> {
        let mut parser = tlang_parser::Parser::from_source(source);
        let (ast, _) = parser
            .parse()
            .map_err(|e| diagnostics_from_parse_error(&e))?;
        Ok((
            ron::ser::to_string_pretty(&ast, ron::ser::PrettyConfig::default()).unwrap(),
            Vec::new(),
        ))
    }
}
