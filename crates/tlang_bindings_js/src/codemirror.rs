use serde::{Deserialize, Serialize};
use tlang_parser::error::ParseIssue;
use tlang_semantics::diagnostic::{Diagnostic, Severity};
use tsify::Tsify;

/// Convert a byte offset in `source` to a UTF-16 code unit count from the
/// start of `source`. CodeMirror 6 positions are UTF-16 code unit indices
/// (JavaScript string positions), while tlang's `Span::start`/`end` are byte
/// offsets. For ASCII-only sources these are identical, but multi-byte UTF-8
/// characters (e.g. `•`, `—`, `─`) in comments would otherwise shift the
/// highlighted range by the difference in byte length vs UTF-16 length.
fn byte_offset_to_utf16(source: &str, byte_offset: u32) -> u32 {
    let byte_offset = (byte_offset as usize).min(source.len());
    source[..byte_offset]
        .chars()
        .map(|c| c.len_utf16() as u32)
        .sum()
}

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
    /// UTF-16 code unit offset (JavaScript string position) of the start of this diagnostic.
    pub from: u32,
    /// UTF-16 code unit offset (JavaScript string position) of the end of this diagnostic.
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

pub fn from_parse_issue(error: &ParseIssue, source: &str) -> CodemirrorDiagnostic {
    CodemirrorDiagnostic::new(
        &error.msg,
        CodemirrorSeverity::Error,
        byte_offset_to_utf16(source, error.span.start),
        byte_offset_to_utf16(source, error.span.end),
    )
}

pub fn from_tlang_diagnostic(diagnostic: &Diagnostic, source: &str) -> CodemirrorDiagnostic {
    CodemirrorDiagnostic::new(
        diagnostic.message(),
        diagnostic.severity().into(),
        byte_offset_to_utf16(source, diagnostic.span().start),
        byte_offset_to_utf16(source, diagnostic.span().end),
    )
}
