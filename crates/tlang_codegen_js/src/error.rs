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

    /// Construct an error for an identifier that could not be resolved in the
    /// current scope.  This indicates a compiler bug: the identifier was not
    /// registered before it was referenced.
    pub fn unresolved_identifier(name: &str, span: Span) -> Self {
        Self {
            message: format!(
                "unresolved identifier `{name}`: not found in the codegen scope (compiler bug)"
            ),
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
