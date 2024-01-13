#[derive(Debug, Clone, PartialEq)]
pub enum Severity {
    Error,
    Warning,
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