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

pub fn from_parse_issue(src: &str, error: &ParseIssue) -> CodemirrorDiagnostic {
    CodemirrorDiagnostic::new(
        &error.msg,
        CodemirrorSeverity::Error,
        line_column_to_offset(src, error.span.start.line, error.span.start.column),
        line_column_to_offset(src, error.span.end.line, error.span.end.column),
    )
}

pub fn from_tlang_diagnostic(src: &str, diagnostic: &Diagnostic) -> CodemirrorDiagnostic {
    CodemirrorDiagnostic::new(
        diagnostic.message(),
        diagnostic.severity().into(),
        line_column_to_offset(
            src,
            diagnostic.span().start.line,
            diagnostic.span().start.column.saturating_sub(1),
        ),
        line_column_to_offset(
            src,
            diagnostic.span().end.line,
            diagnostic.span().end.column.saturating_sub(1),
        ),
    )
}

fn line_column_to_offset(src: &str, line: u32, column: u32) -> u32 {
    let mut offset = 0;
    let lines = src.lines();

    for (i, l) in lines.enumerate() {
        if i as u32 == line {
            offset += column;
            break;
        }
        offset += l.len() as u32 + 1;
    }

    offset
}
