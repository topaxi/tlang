use std::fmt::{Display, Formatter};

#[cfg(feature = "serde")]
use serde::Serialize;
use tlang_ast::span::Span;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Severity {
    Error,
    Warning,
}

impl Display for Severity {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Severity::Error => write!(f, "error"),
            Severity::Warning => write!(f, "warning"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Diagnostic {
    /// The message to display to the user.
    message: String,
    /// The location of the error.
    span: Span,
    /// The severity of the error.
    severity: Severity,
}

impl Diagnostic {
    pub fn new(message: String, severity: Severity, span: Span) -> Self {
        Diagnostic {
            message,
            span,
            severity,
        }
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn severity(&self) -> Severity {
        self.severity
    }

    pub fn span(&self) -> Span {
        self.span
    }

    pub fn is_error(&self) -> bool {
        self.severity == Severity::Error
    }

    pub fn is_warning(&self) -> bool {
        self.severity == Severity::Warning
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}: {} on line {}:{}",
            self.severity.to_string().to_uppercase(),
            self.message,
            self.span.start.line + 1,
            self.span.start.column + 1
        )
    }
}
