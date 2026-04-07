pub mod builtins;
mod type_checker;
mod type_error;
mod type_table;
mod typing_context;

pub use type_checker::TypeChecker;
pub use type_error::TypeError;
pub use type_table::{TypeInfo, TypeTable};
pub use typing_context::TypingContext;
