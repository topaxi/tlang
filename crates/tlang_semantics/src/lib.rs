pub mod analyzer;
pub mod diagnostic;
pub mod passes;

pub use analyzer::{
    SemanticAnalysisContext, SemanticAnalysisGroup, SemanticAnalysisPass, SemanticAnalyzer,
};
pub use passes::{DeclarationAnalyzer, StringLiteralValidator, VariableUsageValidator};
