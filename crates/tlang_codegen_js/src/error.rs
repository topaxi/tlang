use std::fmt;

use tlang_span::Span;

/// An error produced by the JavaScript code generator.
#[derive(Debug, Clone)]
pub struct CodegenError {
    /// Human-readable description of the error.
    pub message: String,
    /// Source span of the node that triggered the error.
    pub span: Span,
}

impl CodegenError {
    /// Construct an error for a language feature that is not yet supported by
    /// the JavaScript backend.
    pub fn unsupported(feature: &str, span: Span) -> Self {
        Self {
            message: format!("`{feature}` is not yet supported in the JavaScript backend"),
            span,
        }
    }
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.message)
    }
}

impl std::error::Error for CodegenError {}
