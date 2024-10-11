use tlang_parser::error::ParseError;
use tlang_semantics::diagnostic::Diagnostic;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
pub struct CodemirrorDiagnostic {
    #[wasm_bindgen(readonly, getter_with_clone)]
    pub message: String,

    #[wasm_bindgen(readonly, getter_with_clone)]
    pub severity: String,

    /// Relative position in the document, our diagnostics are line/column based
    #[wasm_bindgen(readonly)]
    pub from: u32,

    /// Relative position in the document, our diagnostics are line/column based
    #[wasm_bindgen(readonly)]
    pub to: u32,
}

#[wasm_bindgen]
impl CodemirrorDiagnostic {
    pub(crate) fn new(message: &str, severity: &str, from: u32, to: u32) -> Self {
        CodemirrorDiagnostic {
            message: message.to_string(),
            severity: severity.to_string(),
            from,
            to,
        }
    }
}

pub fn from_parse_error(src: &str, error: &ParseError) -> CodemirrorDiagnostic {
    CodemirrorDiagnostic::new(
        &error.msg,
        "error",
        line_column_to_offset(src, error.span.start.line, error.span.start.column),
        line_column_to_offset(src, error.span.end.line, error.span.end.column),
    )
}

pub fn from_tlang_diagnostic(src: &str, diagnostic: &Diagnostic) -> CodemirrorDiagnostic {
    CodemirrorDiagnostic::new(
        diagnostic.message(),
        &diagnostic.severity().to_string(),
        line_column_to_offset(
            src,
            diagnostic.span().start.line,
            diagnostic.span().start.column - 1,
        ),
        line_column_to_offset(
            src,
            diagnostic.span().end.line,
            diagnostic.span().end.column - 1,
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
