pub mod ast;
pub mod hir;
pub mod hir_raw;
pub mod js;

use crate::error::ParserError;

pub trait CompileTarget {
    fn compile(&self, source: &str, module: &mut tlang_hir::Module) -> Result<String, ParserError>;
}
