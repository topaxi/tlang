pub mod builtin_methods;
pub mod builtin_protocols;
pub mod builtin_types;
pub mod builtins;
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
pub use unification::{OccursCheckError, UnificationTable};
