pub mod analyzer;
mod declarations;
pub mod diagnostic;
mod string_literal;
pub mod variable_usage;

pub use analyzer::{
    SemanticAnalysisContext, SemanticAnalysisGroup, SemanticAnalysisPass, SemanticAnalyzer,
};
pub use declarations::DeclarationAnalyzer;
pub use string_literal::StringLiteralValidator;
pub use variable_usage::VariableUsageValidator;
