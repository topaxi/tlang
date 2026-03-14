use serde::{Deserialize, Serialize};
use tlang_parser::error::ParseIssue;
use tlang_semantics::diagnostic::{Diagnostic, Severity};
use tsify::Tsify;

#[derive(Clone, Copy, Serialize, Deserialize, Tsify)]
#[serde(rename_all = "lowercase")]
pub enum CodemirrorSeverity {
    Hint,
    Info,
    Warning,
    Error,
}

impl From<Severity> for CodemirrorSeverity {
    fn from(severity: Severity) -> Self {
        match severity {
            Severity::Error => CodemirrorSeverity::Error,
            Severity::Warning => CodemirrorSeverity::Warning,
        }
    }
}

#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct CodemirrorDiagnostic {
    pub message: String,
    pub severity: CodemirrorSeverity,
    /// Relative position in the document, our diagnostics are line/column based
    pub from: u32,
    /// Relative position in the document, our diagnostics are line/column based
    pub to: u32,
}

impl CodemirrorDiagnostic {
    pub(crate) fn new(message: &str, severity: CodemirrorSeverity, from: u32, to: u32) -> Self {
        CodemirrorDiagnostic {
            message: message.to_string(),
            severity,
            from,
            to,
        }
    }
}

pub fn from_parse_issue(_src: &str, error: &ParseIssue) -> CodemirrorDiagnostic {
    CodemirrorDiagnostic::new(
        &error.msg,
        CodemirrorSeverity::Error,
        error.span.start,
        error.span.end,
    )
}

pub fn from_tlang_diagnostic(_src: &str, diagnostic: &Diagnostic) -> CodemirrorDiagnostic {
    CodemirrorDiagnostic::new(
        diagnostic.message(),
        diagnostic.severity().into(),
        diagnostic.span().start,
        diagnostic.span().end,
    )
}
