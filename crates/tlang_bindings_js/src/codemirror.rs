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
pub(crate) fn byte_offset_to_utf16(source: &str, byte_offset: u32) -> u32 {
    let byte_offset = (byte_offset as usize).min(source.len());
    source[..byte_offset]
        .chars()
        .map(|c| c.len_utf16() as u32)
        .sum()
}

/// Convert a UTF-16 code unit offset to a byte offset in `source`.
///
/// This is the inverse of [`byte_offset_to_utf16`].
pub(crate) fn utf16_to_byte_offset(source: &str, utf16_offset: u32) -> u32 {
    let mut utf16_count = 0u32;
    let mut byte_pos = 0usize;

    for ch in source.chars() {
        if utf16_count >= utf16_offset {
            break;
        }
        utf16_count += ch.len_utf16() as u32;
        byte_pos += ch.len_utf8();
    }

    byte_pos as u32
}

/// Convert a byte offset in `source` to a 0-based `(line, column)` pair.
///
/// Both `line` and `column` are 0-based.  This is the common editor convention
/// (LSP, CodeMirror).  Callers that need the lexer's coordinate system should
/// use [`tlang_analysis::query::resolve_symbol`] which adjusts internally.
pub(crate) fn byte_offset_to_line_column(source: &str, byte_offset: u32) -> (u32, u32) {
    let byte_offset = (byte_offset as usize).min(source.len());
    let mut line = 0u32;
    let mut col = 0u32;

    for (i, ch) in source.char_indices() {
        if i >= byte_offset {
            break;
        }
        if ch == '\n' {
            line += 1;
            col = 0;
        } else {
            col += ch.len_utf16() as u32;
        }
    }

    (line, col)
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

/// Hover information formatted for CodeMirror 6.
///
/// Positions are UTF-16 code unit offsets matching JavaScript string positions.
#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct CodemirrorHoverInfo {
    /// The hover text to display (e.g. `"(function) add/2"`).
    pub text: String,
    /// UTF-16 code unit offset of the start of the hovered identifier.
    pub from: u32,
    /// UTF-16 code unit offset of the end of the hovered identifier.
    pub to: u32,
}

/// Definition location formatted for CodeMirror 6.
///
/// Positions are UTF-16 code unit offsets matching JavaScript string positions.
#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct CodemirrorDefinitionLocation {
    /// UTF-16 code unit offset of the start of the definition.
    pub from: u32,
    /// UTF-16 code unit offset of the end of the definition.
    pub to: u32,
}
