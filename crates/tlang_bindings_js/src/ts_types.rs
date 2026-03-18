// Wrappers to convert Rust types to TypeScript-compatible types for wasm-bindgen
use serde::{Deserialize, Serialize};
use tsify::Tsify;
use wasm_bindgen::prelude::*;

use tlang_parser::error::{ParseIssue, ParseIssueKind};
use tlang_semantics::diagnostic::{Diagnostic, Severity};
use tlang_span::{LineColumn, Span};

#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct JsLineColumn {
    pub line: u32,
    pub column: u32,
}

impl From<LineColumn> for JsLineColumn {
    fn from(line_column: LineColumn) -> Self {
        Self {
            line: line_column.line,
            column: line_column.column,
        }
    }
}

#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct JsSpan {
    pub start: u32,
    pub end: u32,
    pub start_lc: JsLineColumn,
    pub end_lc: JsLineColumn,
}

impl From<Span> for JsSpan {
    fn from(span: Span) -> Self {
        Self {
            start: span.start,
            end: span.end,
            start_lc: span.start_lc.into(),
            end_lc: span.end_lc.into(),
        }
    }
}

#[derive(Serialize, Deserialize, Tsify)]
#[serde(rename_all = "lowercase")]
#[tsify(into_wasm_abi)]
pub enum JsSeverity {
    Error,
    Warning,
}

impl From<Severity> for JsSeverity {
    fn from(severity: Severity) -> Self {
        match severity {
            Severity::Error => Self::Error,
            Severity::Warning => Self::Warning,
        }
    }
}

#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct JsDiagnostic {
    pub message: String,
    pub span: JsSpan,
    pub severity: JsSeverity,
}

impl From<Diagnostic> for JsDiagnostic {
    fn from(diagnostic: Diagnostic) -> Self {
        Self {
            message: diagnostic.message().to_string(),
            span: (*diagnostic.span()).into(),
            severity: diagnostic.severity().into(),
        }
    }
}

#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct JsParseIssue {
    pub msg: String,
    pub kind: JsParseIssueKind,
    pub span: JsSpan,
}

impl From<ParseIssue> for JsParseIssue {
    fn from(issue: ParseIssue) -> Self {
        Self {
            msg: issue.msg,
            kind: issue.kind.into(),
            span: issue.span.into(),
        }
    }
}

#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub enum JsParseIssueKind {
    /// Debug representation of the unexpected token kind.
    UnexpectedToken(String),
    UnexpectedEof,
}

impl From<ParseIssueKind> for JsParseIssueKind {
    fn from(kind: ParseIssueKind) -> Self {
        match kind {
            ParseIssueKind::UnexpectedToken(desc) => Self::UnexpectedToken(desc),
            ParseIssueKind::UnexpectedEof => Self::UnexpectedEof,
        }
    }
}
