pub mod analyzer;
mod declarations;

pub use analyzer::SemanticAnalyzer;

#[cfg(test)]
mod analyzer_tests;
