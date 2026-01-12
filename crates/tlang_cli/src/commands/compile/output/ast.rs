use super::CompileTarget;
use crate::error::ParserError;

pub struct AstTarget;

impl CompileTarget for AstTarget {
    fn compile(
        &self,
        source: &str,
        _module: &mut tlang_hir::hir::Module,
    ) -> Result<String, ParserError> {
        let mut parser = tlang_parser::Parser::from_source(source);
        let ast = parser.parse()?;
        Ok(ron::ser::to_string_pretty(&ast, ron::ser::PrettyConfig::default()).unwrap())
    }
}
