pub mod ast;
pub mod hir;
pub mod js;

use crate::error::ParserError;
use tlang_hir::hir as tlang_hir;

pub trait CompileTarget {
    fn compile(&self, source: &str, module: &mut tlang_hir::Module) -> Result<String, ParserError>;
}
