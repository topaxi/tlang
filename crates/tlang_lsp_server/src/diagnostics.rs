use lsp_types::{DiagnosticSeverity, Position, Range, Url};
use tlang_diagnostics::Diagnostic;
use tlang_parser::error::ParseIssue;
use tlang_span::Span;

/// Convert a tlang `Span` to an LSP `Range`.
///
/// `Span.start_lc` / `end_lc` are 0-indexed line+column. The LSP protocol
/// uses 0-indexed line + UTF-16 character offset. For ASCII-only identifiers
/// and keywords (the common case) these are identical.
pub fn span_to_range(span: &Span) -> Range {
    Range {
        start: Position {
            line: span.start_lc.line,
            character: span.start_lc.column,
        },
        end: Position {
            line: span.end_lc.line,
            character: span.end_lc.column,
        },
    }
}

/// Convert a tlang parse issue into an LSP diagnostic.
pub fn from_parse_issue(issue: &ParseIssue) -> lsp_types::Diagnostic {
    lsp_types::Diagnostic {
        range: span_to_range(&issue.span),
        severity: Some(DiagnosticSeverity::ERROR),
        source: Some("tlang".into()),
        message: issue.msg.clone(),
        ..Default::default()
    }
}

/// Convert a tlang semantic diagnostic into an LSP diagnostic.
///
/// `uri` is the document URI used for `relatedInformation` locations, since
/// tlang diagnostic labels reference spans within the same file.
pub fn from_tlang_diagnostic(diagnostic: &Diagnostic, uri: &Url) -> lsp_types::Diagnostic {
    let severity = match diagnostic.severity() {
        tlang_diagnostics::Severity::Error => DiagnosticSeverity::ERROR,
        tlang_diagnostics::Severity::Warning => DiagnosticSeverity::WARNING,
    };

    let related_information = if diagnostic.labels().is_empty() {
        None
    } else {
        Some(
            diagnostic
                .labels()
                .iter()
                .map(|label| lsp_types::DiagnosticRelatedInformation {
                    location: lsp_types::Location {
                        uri: uri.clone(),
                        range: span_to_range(&label.span),
                    },
                    message: label.message.clone(),
                })
                .collect(),
        )
    };

    lsp_types::Diagnostic {
        range: span_to_range(diagnostic.span()),
        severity: Some(severity),
        source: Some("tlang".into()),
        message: diagnostic.message().to_string(),
        related_information,
        ..Default::default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tlang_span::{LineColumn, Span};

    fn make_span(start_line: u32, start_col: u32, end_line: u32, end_col: u32) -> Span {
        Span {
            start: 0,
            end: 10,
            start_lc: LineColumn {
                line: start_line,
                column: start_col,
            },
            end_lc: LineColumn {
                line: end_line,
                column: end_col,
            },
        }
    }

    fn test_uri() -> Url {
        Url::parse("file:///test/example.tlang").unwrap()
    }

    #[test]
    fn parse_issue_to_lsp_diagnostic() {
        let issue = ParseIssue {
            msg: "unexpected token".into(),
            kind: tlang_parser::error::ParseIssueKind::UnexpectedToken("foo".into()),
            span: make_span(0, 5, 0, 10),
        };

        let diag = from_parse_issue(&issue);
        assert_eq!(diag.message, "unexpected token");
        assert_eq!(diag.severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(diag.range.start.line, 0);
        assert_eq!(diag.range.start.character, 5);
        assert_eq!(diag.range.end.line, 0);
        assert_eq!(diag.range.end.character, 10);
        assert_eq!(diag.source.as_deref(), Some("tlang"));
    }

    #[test]
    fn semantic_error_to_lsp_diagnostic() {
        let span = make_span(2, 0, 2, 5);
        let diagnostic = Diagnostic::error("undefined variable", span);

        let diag = from_tlang_diagnostic(&diagnostic, &test_uri());
        assert_eq!(diag.message, "undefined variable");
        assert_eq!(diag.severity, Some(DiagnosticSeverity::ERROR));
        assert_eq!(diag.range.start.line, 2);
    }

    #[test]
    fn semantic_warning_to_lsp_diagnostic() {
        let span = make_span(1, 0, 1, 3);
        let diagnostic = Diagnostic::warn("unused variable", span);

        let diag = from_tlang_diagnostic(&diagnostic, &test_uri());
        assert_eq!(diag.message, "unused variable");
        assert_eq!(diag.severity, Some(DiagnosticSeverity::WARNING));
    }

    #[test]
    fn diagnostic_with_labels_has_related_information() {
        let uri = test_uri();
        let span = make_span(0, 0, 0, 5);
        let label_span = make_span(1, 0, 1, 5);
        let diagnostic =
            Diagnostic::error("type mismatch", span).with_label("expected here", label_span);

        let diag = from_tlang_diagnostic(&diagnostic, &uri);
        let related = diag.related_information.unwrap();
        assert_eq!(related.len(), 1);
        assert_eq!(related[0].message, "expected here");
        assert_eq!(related[0].location.uri, uri);
    }
}
