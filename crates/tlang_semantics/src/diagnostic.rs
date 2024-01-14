use std::fmt::{Display, Formatter};

#[derive(Debug, Clone, PartialEq)]
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
pub struct Diagnostic {
    /// The message to display to the user.
    message: String,
    /// The location of the error.
    //span: Span,
    /// The severity of the error.
    severity: Severity,
}

impl Diagnostic {
    pub fn new(message: String, severity: Severity) -> Self {
        Diagnostic {
            message,
            // span,
            severity,
        }
    }

    pub fn message(&self) -> &str {
        &self.message
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
            "{}: {}",
            self.severity.to_string().to_uppercase(),
            self.message
        )
    }
}
