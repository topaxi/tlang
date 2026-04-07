use tlang_diagnostics::{Diagnostic, Severity};
use tlang_span::Span;

/// A type-checking error with source location and diagnostic information.
///
/// `TypeError` provides structured error information that can be converted
/// into the project's `Diagnostic` type for unified reporting.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TypeError {
    /// A type mismatch between expected and actual types.
    TypeMismatch {
        expected: String,
        actual: String,
        span: Span,
    },
    /// An attempted cast on an `unknown` type, which is not permitted.
    CastOnUnknown { span: Span },
    /// No `From` implementation exists for an infallible `as` cast.
    NoFromImpl {
        from_ty: String,
        to_ty: String,
        span: Span,
    },
}

impl TypeError {
    /// The primary span associated with this error.
    pub fn span(&self) -> Span {
        match self {
            TypeError::TypeMismatch { span, .. }
            | TypeError::CastOnUnknown { span }
            | TypeError::NoFromImpl { span, .. } => *span,
        }
    }

    /// A human-readable message describing the error.
    pub fn message(&self) -> String {
        match self {
            TypeError::TypeMismatch {
                expected, actual, ..
            } => format!("type mismatch: expected `{expected}`, found `{actual}`"),
            TypeError::CastOnUnknown { .. } => {
                "cannot use `as` cast on `unknown` type; use `as?` instead".to_string()
            }
            TypeError::NoFromImpl { from_ty, to_ty, .. } => {
                format!("no `From<{from_ty}>` implementation for `{to_ty}`")
            }
        }
    }
}

impl From<&TypeError> for Diagnostic {
    fn from(err: &TypeError) -> Self {
        Diagnostic::new(Severity::Error, err.message(), err.span())
    }
}

impl From<TypeError> for Diagnostic {
    fn from(err: TypeError) -> Self {
        Diagnostic::from(&err)
    }
}
