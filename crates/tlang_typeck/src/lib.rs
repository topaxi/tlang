pub mod builtin_methods;
pub mod builtin_protocols;
pub mod builtin_types;
pub mod builtins;
mod local_inference;
mod type_checker;
mod type_error;
mod type_table;
mod typing_context;
pub mod unification;

pub use type_checker::TypeChecker;
pub use type_error::TypeError;
pub use type_table::{
    EnumInfo, ImplInfo, ProtocolInfo, ProtocolMethodInfo, StructInfo, TypeInfo, TypeTable,
    VariantInfo,
};
pub use typing_context::TypingContext;
pub use unification::{OccursCheckError, UnificationError, UnificationTable};

use tlang_diagnostics::Diagnostic;
use tlang_hir as hir;
use tlang_hir_opt::HirPass;
use tlang_hir_opt::hir_opt::{HirOptContext, HirOptError};

#[derive(Debug, Default)]
pub struct TypecheckResult {
    pub type_table: TypeTable,
    pub errors: Vec<Diagnostic>,
    pub warnings: Vec<Diagnostic>,
}

impl TypecheckResult {
    pub fn has_errors(&self) -> bool {
        !self.errors.is_empty()
    }
}

pub fn typecheck_module(
    module: &mut hir::Module,
    ctx: &mut HirOptContext,
) -> Result<TypecheckResult, HirOptError> {
    let mut type_checker = TypeChecker::new();
    type_checker.optimize_hir(module, ctx)?;

    let (errors, warnings) = std::mem::take(&mut ctx.diagnostics)
        .into_iter()
        .partition(|diagnostic| diagnostic.is_error());

    Ok(TypecheckResult {
        type_table: type_checker.type_table,
        errors,
        warnings,
    })
}
