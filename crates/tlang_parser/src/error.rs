use std::error::Error;
use std::fmt::{Display, Formatter};

#[cfg(feature = "serde")]
use serde::Serialize;
use tlang_span::Span;

#[derive(Debug, Clone)]
pub struct ParseError {
    issues: Vec<ParseIssue>,
}

impl ParseError {
    pub fn new(issues: Vec<ParseIssue>) -> Self {
        Self { issues }
    }

    pub fn issues(&self) -> &[ParseIssue] {
        &self.issues
    }
}

impl Error for ParseError {}

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}",
            self.issues
                .iter()
                .map(|issue| issue.to_string())
                .collect::<Vec<String>>()
                .join("\n")
        )
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct ParseIssue {
    pub msg: String,
    pub kind: ParseIssueKind,
    pub span: Span,
}

impl Error for ParseIssue {}

impl Display for ParseIssue {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "ParseError: {} at {}:{}",
            self.msg, self.span.start_lc.line, self.span.start_lc.column
        )
    }
}

#[derive(Debug, Clone)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum ParseIssueKind {
    /// Debug representation of the unexpected token kind.
    UnexpectedToken(String),
    UnexpectedEof,
}

#[cfg(test)]
mod tests {
    use tlang_span::{LineColumn, Span};

    use super::*;

    fn make_span(line: u32, col: u32) -> Span {
        Span::lc(LineColumn::new(line, col), LineColumn::new(line, col))
    }

    fn make_issue(msg: &str, kind: ParseIssueKind, line: u32, col: u32) -> ParseIssue {
        ParseIssue {
            msg: msg.to_string(),
            kind,
            span: make_span(line, col),
        }
    }

    #[test]
    fn test_parse_error_issues_returns_slice() {
        let issues = vec![
            make_issue("unexpected token", ParseIssueKind::UnexpectedEof, 1, 5),
            make_issue("expected }", ParseIssueKind::UnexpectedEof, 2, 1),
        ];
        let err = ParseError::new(issues.clone());
        assert_eq!(err.issues().len(), 2);
        assert_eq!(err.issues()[0].msg, "unexpected token");
    }

    #[test]
    fn test_parse_error_display_single_issue() {
        let issue = make_issue("unexpected `}`", ParseIssueKind::UnexpectedEof, 3, 7);
        let err = ParseError::new(vec![issue]);
        let s = err.to_string();
        assert!(s.contains("ParseError:"));
        assert!(s.contains("unexpected `}`"));
        assert!(s.contains("3:7"));
    }

    #[test]
    fn test_parse_error_display_multiple_issues() {
        let issues = vec![
            make_issue("first error", ParseIssueKind::UnexpectedEof, 1, 1),
            make_issue("second error", ParseIssueKind::UnexpectedEof, 2, 5),
        ];
        let err = ParseError::new(issues);
        let s = err.to_string();
        assert!(s.contains("first error"));
        assert!(s.contains("second error"));
        // Multiple issues are joined with newlines
        assert!(s.contains('\n'));
    }

    #[test]
    fn test_parse_issue_display() {
        let issue = make_issue(
            "unexpected token",
            ParseIssueKind::UnexpectedToken("Ident".to_string()),
            4,
            10,
        );
        let s = issue.to_string();
        assert_eq!(s, "ParseError: unexpected token at 4:10");
    }

    #[test]
    fn test_parse_issue_kind_unexpected_token() {
        let issue = make_issue(
            "unexpected",
            ParseIssueKind::UnexpectedToken("Plus".to_string()),
            1,
            1,
        );
        assert!(matches!(issue.kind, ParseIssueKind::UnexpectedToken(_)));
    }

    #[test]
    fn test_parse_issue_kind_unexpected_eof() {
        let issue = make_issue("unexpected end", ParseIssueKind::UnexpectedEof, 5, 0);
        assert!(matches!(issue.kind, ParseIssueKind::UnexpectedEof));
    }

    #[test]
    fn test_parse_error_implements_error_trait() {
        let err = ParseError::new(vec![make_issue("msg", ParseIssueKind::UnexpectedEof, 1, 0)]);
        // Verify it implements std::error::Error (compile-time check via cast)
        let _: &dyn std::error::Error = &err;
    }

    #[test]
    fn test_parse_issue_implements_error_trait() {
        let issue = make_issue("msg", ParseIssueKind::UnexpectedEof, 1, 0);
        let _: &dyn std::error::Error = &issue;
    }
}
