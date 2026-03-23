// Re-export all diagnostic types from tlang_diagnostics for backward compatibility.
// New code should import directly from `tlang_diagnostics`.
pub use tlang_diagnostics::{Diagnostic, DiagnosticLabel, Severity};
pub use tlang_diagnostics::{error_at, warn_at};
