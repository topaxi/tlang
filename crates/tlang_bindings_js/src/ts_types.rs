// Wrappers to convert Rust types to TypeScript-compatible types for wasm-bindgen
use serde::{Deserialize, Serialize};
use tsify::Tsify;
use wasm_bindgen::prelude::*;

use tlang_ast::token::Token;
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
    pub start: JsLineColumn,
    pub end: JsLineColumn,
}

impl From<Span> for JsSpan {
    fn from(span: Span) -> Self {
        Self {
            start: span.start.into(),
            end: span.end.into(),
        }
    }
}

#[derive(Serialize, Deserialize, Tsify)]
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
            severity: diagnostic.severity().clone().into(),
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
    UnexpectedToken(JsToken),
    UnexpectedEof,
}

impl From<ParseIssueKind> for JsParseIssueKind {
    fn from(kind: ParseIssueKind) -> Self {
        match kind {
            ParseIssueKind::UnexpectedToken(token) => Self::UnexpectedToken(token.into()),
            ParseIssueKind::UnexpectedEof => Self::UnexpectedEof,
        }
    }
}

#[derive(Serialize, Deserialize, Tsify)]
#[tsify(into_wasm_abi)]
pub struct JsToken {
    pub kind: String, // Simplified to string representation for now to avoid huge enum mirror
    pub span: JsSpan,
}

impl From<Token> for JsToken {
    fn from(token: Token) -> Self {
        Self {
            kind: format!("{:?}", token.kind),
            span: token.span.into(),
        }
    }
}
