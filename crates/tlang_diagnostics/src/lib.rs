use std::fmt::{Display, Formatter};
use std::ops::Range;

use codespan_reporting::{
    diagnostic::{Diagnostic as ReportDiagnostic, Label, Severity as ReportSeverity},
    files::SimpleFiles,
    term::{self, Config, termcolor::Buffer},
};
use tlang_parser::error::{ParseError, ParseIssue};
use tlang_span::Span;

#[cfg(feature = "serde")]
use serde::Serialize;

/// The severity of a diagnostic.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub enum Severity {
    Error,
    Warning,
}

impl Display for Severity {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        match self {
            Severity::Error => write!(f, "error"),
            Severity::Warning => write!(f, "warning"),
        }
    }
}

/// A secondary labeled span attached to a [`Diagnostic`].
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct DiagnosticLabel {
    pub message: String,
    pub span: Span,
}

/// A structured diagnostic message with severity, primary span, and optional
/// secondary labeled spans.
#[derive(Debug, Clone, PartialEq, Eq)]
#[cfg_attr(feature = "serde", derive(Serialize))]
pub struct Diagnostic {
    /// The message to display to the user.
    message: String,
    /// The primary location of the error.
    span: Span,
    /// The severity of the error.
    severity: Severity,
    /// Optional secondary labeled spans (e.g. "defined here" pointers).
    labels: Vec<DiagnosticLabel>,
}

impl Diagnostic {
    pub fn new(severity: Severity, message: String, span: Span) -> Self {
        Diagnostic {
            message,
            span,
            severity,
            labels: Vec::new(),
        }
    }

    pub fn warn(message: &str, span: Span) -> Self {
        Diagnostic::new(Severity::Warning, message.to_string(), span)
    }

    pub fn error(message: &str, span: Span) -> Self {
        Diagnostic::new(Severity::Error, message.to_string(), span)
    }

    /// Attach a secondary labeled span to this diagnostic (builder-style).
    pub fn with_label(mut self, message: impl Into<String>, span: Span) -> Self {
        self.labels.push(DiagnosticLabel {
            message: message.into(),
            span,
        });
        self
    }

    pub fn message(&self) -> &str {
        &self.message
    }

    pub fn severity(&self) -> Severity {
        self.severity
    }

    pub fn span(&self) -> &Span {
        &self.span
    }

    pub fn labels(&self) -> &[DiagnosticLabel] {
        &self.labels
    }

    pub fn is_error(&self) -> bool {
        self.severity == Severity::Error
    }

    pub fn is_warning(&self) -> bool {
        self.severity == Severity::Warning
    }
}

impl Display for Diagnostic {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(
            f,
            "{}: {} on line {}:{}",
            self.severity.to_string().to_uppercase(),
            self.message,
            self.span.start_lc.line + 1,
            self.span.start_lc.column + 1
        )
    }
}

impl From<ParseIssue> for Diagnostic {
    fn from(issue: ParseIssue) -> Self {
        Diagnostic::error(&issue.msg, issue.span)
    }
}

impl From<&ParseIssue> for Diagnostic {
    fn from(issue: &ParseIssue) -> Self {
        Diagnostic::error(&issue.msg, issue.span)
    }
}

/// Convert a [`ParseError`] (which may contain multiple parse issues) into a
/// `Vec<Diagnostic>`, one per issue.
pub fn diagnostics_from_parse_error(err: &ParseError) -> Vec<Diagnostic> {
    err.issues().iter().map(Diagnostic::from).collect()
}

#[macro_export]
macro_rules! warn_at {
    ($span:expr, $fmt:expr, $($arg:tt)*) => {{
        let msg = format!($fmt, $($arg)*);
        $crate::Diagnostic::warn(&msg, $span)
    }};
}

#[macro_export]
macro_rules! error_at {
    ($span:expr, $fmt:expr, $($arg:tt)*) => {{
        let msg = format!($fmt, $($arg)*);
        $crate::Diagnostic::error(&msg, $span)
    }};
}

fn label_range(span: &Span, source_len: usize) -> Range<usize> {
    let mut start = usize::min(span.start as usize, source_len);
    let mut end = usize::min(span.end as usize, source_len);

    // Special-case spans that point exactly at EOF (start == end == source_len):
    // back up one byte so the diagnostic still points at a visible location.
    if start == source_len && source_len > 0 {
        start = source_len - 1;
        end = source_len;
    } else if end <= start && start < source_len {
        // For non-EOF spans, ensure the range is non-empty by extending it by one byte.
        end = start + 1;
    }

    start..end
}

fn diagnostic_report(
    file_id: usize,
    source_len: usize,
    diagnostic: &Diagnostic,
) -> ReportDiagnostic<usize> {
    let severity = match diagnostic.severity() {
        Severity::Error => ReportSeverity::Error,
        Severity::Warning => ReportSeverity::Warning,
    };

    let mut labels = vec![Label::primary(
        file_id,
        label_range(diagnostic.span(), source_len),
    )];

    for label in diagnostic.labels() {
        labels.push(
            Label::secondary(file_id, label_range(&label.span, source_len))
                .with_message(&label.message),
        );
    }

    ReportDiagnostic::new(severity)
        .with_message(diagnostic.message())
        .with_labels(labels)
}

/// Render diagnostics into a human-readable, source-aware string using
/// `codespan_reporting`. This is the unified render entry point for all
/// diagnostic types.
///
/// Set `ansi` to `true` to emit ANSI color/style escape sequences (e.g. for
/// terminals or downstream ANSI-to-HTML conversion).
///
/// # Panics
///
/// Panics if writing to the internal buffer fails, which cannot happen with
/// the in-memory [`Buffer`] used internally.
pub fn render_diagnostics(
    source_name: &str,
    source: &str,
    diagnostics: &[Diagnostic],
    ansi: bool,
) -> String {
    let mut files = SimpleFiles::new();
    let file_id = files.add(source_name, source);
    let mut writer = if ansi {
        Buffer::ansi()
    } else {
        Buffer::no_color()
    };
    let config = Config::default();

    for diagnostic in diagnostics {
        let report = diagnostic_report(file_id, source.len(), diagnostic);
        // SimpleFiles never returns an error; the expect is unreachable.
        term::emit_to_write_style(&mut writer, &config, &files, &report)
            .expect("codespan diagnostic rendering should succeed");
    }

    String::from_utf8_lossy(writer.as_slice()).into_owned()
}

/// Render parse issues from a failed parse into a human-readable, source-aware
/// string using `codespan_reporting`.
///
/// Set `ansi` to `true` to emit ANSI color/style escape sequences (e.g. for
/// terminals or downstream ANSI-to-HTML conversion).
///
/// # Panics
///
/// Panics if writing to the internal buffer fails, which cannot happen with
/// the in-memory [`Buffer`] used internally.
pub fn render_parse_issues(
    source_name: &str,
    source: &str,
    issues: &[ParseIssue],
    ansi: bool,
) -> String {
    let diagnostics: Vec<Diagnostic> = issues.iter().map(Diagnostic::from).collect();
    render_diagnostics(source_name, source, &diagnostics, ansi)
}

/// Render semantic diagnostics into a human-readable, source-aware string using
/// `codespan_reporting`.
///
/// Set `ansi` to `true` to emit ANSI color/style escape sequences (e.g. for
/// terminals or downstream ANSI-to-HTML conversion).
///
/// # Panics
///
/// Panics if writing to the internal buffer fails, which cannot happen with
/// the in-memory [`Buffer`] used internally.
pub fn render_semantic_diagnostics(
    source_name: &str,
    source: &str,
    diagnostics: &[Diagnostic],
    ansi: bool,
) -> String {
    render_diagnostics(source_name, source, diagnostics, ansi)
}

/// Render an internal compiler error (ICE) message without source context.
///
/// ICEs indicate a compiler bug rather than a user error, so no source
/// location is shown. The message is followed by a link to file an issue.
pub fn render_ice(err: &dyn std::error::Error) -> String {
    format!(
        "error: internal compiler error: {err}\n\nThis is a compiler bug. Please file an issue at https://github.com/topaxi/tlang/issues\n"
    )
}

/// Render a simple error message with source context at a given span.
///
/// This is useful for module-level errors (import errors, visibility errors, etc.)
/// that have a span but are not full semantic diagnostics.
///
/// Set `ansi` to `true` to emit ANSI color/style escape sequences.
///
/// # Panics
///
/// Panics if writing to the internal buffer fails.
pub fn render_error_at_span(
    source_name: &str,
    source: &str,
    message: &str,
    span: &Span,
    ansi: bool,
) -> String {
    let diagnostic = Diagnostic::error(message, *span);
    render_diagnostics(source_name, source, &[diagnostic], ansi)
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use insta::assert_snapshot;
    use tlang_parser::error::{ParseIssue, ParseIssueKind};
    use tlang_span::{LineColumn, Span};

    use super::{
        Diagnostic, diagnostics_from_parse_error, render_diagnostics, render_parse_issues,
        render_semantic_diagnostics,
    };

    /// Build a `ParseIssue` with explicit byte + line/column info, without
    /// needing to drive the parser in recoverable mode.
    fn make_parse_issue(
        msg: &str,
        start: u32,
        end: u32,
        sl: u32,
        sc: u32,
        el: u32,
        ec: u32,
    ) -> ParseIssue {
        ParseIssue {
            msg: msg.to_string(),
            kind: ParseIssueKind::UnexpectedEof,
            span: Span::new(start, end, LineColumn::new(sl, sc), LineColumn::new(el, ec)),
        }
    }

    fn span(start: u32, end: u32, sl: u32, sc: u32, el: u32, ec: u32) -> Span {
        Span::new(start, end, LineColumn::new(sl, sc), LineColumn::new(el, ec))
    }

    #[test]
    fn renders_parse_issues_with_source_context() {
        // source: "let x = ;"
        //          0123456789
        // ';' is at byte 8, line 0 col 8 (0-indexed) → displayed as 1:9
        let source = "let x = ;";
        let issues = vec![make_parse_issue(
            "Expected expression, found Semicolon",
            8,
            9,
            0,
            8,
            0,
            9,
        )];

        assert_snapshot!(render_parse_issues("test.tlang", source, &issues, false), @r"
        error: Expected expression, found Semicolon
          ┌─ test.tlang:1:9
          │
        1 │ let x = ;
          │         ^
        ");
    }

    #[test]
    fn renders_multiple_parse_issues() {
        // source with two problems: missing expression and extra token
        // We synthesise two issues to test that the renderer emits both.
        let source = "let a = ; let b = ;";
        let issues = vec![
            make_parse_issue("Expected expression, found Semicolon", 8, 9, 0, 8, 0, 9),
            make_parse_issue("Expected expression, found Semicolon", 18, 19, 0, 18, 0, 19),
        ];

        assert_snapshot!(render_parse_issues("test.tlang", source, &issues, false), @r"
        error: Expected expression, found Semicolon
          ┌─ test.tlang:1:9
          │
        1 │ let a = ; let b = ;
          │         ^

        error: Expected expression, found Semicolon
          ┌─ test.tlang:1:19
          │
        1 │ let a = ; let b = ;
          │                   ^
        ");
    }

    #[test]
    fn renders_semantic_diagnostics_with_source_context() {
        // Simulate diagnostics similar to what SemanticAnalyzer would produce.
        // Source: "fn unused() {\n    missing;\n}\n"
        // Byte offsets (0-indexed):
        //   "fn " = 0-2, "unused" = 3-8, "() {\n" = 9-13
        //   "    " = 14-17, "missing" = 18-24, ";\n" = 25-26, "}\n" = 27-28
        let source = indoc! {"
            fn unused() {
                missing;
            }
        "};

        // error: undeclared `missing` at line 1 (0-indexed), cols 4-11
        let error_diag = Diagnostic::error(
            "Use of undeclared variable `missing`",
            span(18, 25, 1, 4, 1, 11),
        );
        // warning: unused function `unused` at line 0, cols 3-9
        let warn_diag = Diagnostic::warn("Unused function `unused/0`", span(3, 9, 0, 3, 0, 9));

        assert_snapshot!(render_semantic_diagnostics(
            "test.tlang",
            source,
            &[error_diag, warn_diag],
            false,
        ), @r"
        error: Use of undeclared variable `missing`
          ┌─ test.tlang:2:5
          │
        2 │     missing;
          │     ^^^^^^^

        warning: Unused function `unused/0`
          ┌─ test.tlang:1:4
          │
        1 │ fn unused() {
          │    ^^^^^^
        ");
    }

    #[test]
    fn renders_only_warnings() {
        // Source: "let unused_var = 42;\n"
        // "unused_var" at bytes 4-13 (line 0, cols 4-13)
        let source = indoc! {"
            let unused_var = 42;
        "};
        let warning = Diagnostic::warn(
            "Unused variable `unused_var`, if this is intentional, prefix the name with an underscore: `_unused_var`",
            span(4, 14, 0, 4, 0, 14),
        );

        assert_snapshot!(render_semantic_diagnostics("test.tlang", source, &[warning], false), @r"
        warning: Unused variable `unused_var`, if this is intentional, prefix the name with an underscore: `_unused_var`
          ┌─ test.tlang:1:5
          │
        1 │ let unused_var = 42;
          │     ^^^^^^^^^^
        ");
    }

    #[test]
    fn renders_did_you_mean_with_secondary_label() {
        // Source: "fn evaluate(val) { va }"
        // "va" is at bytes 19-21 (line 0, cols 19-21)
        // "val" is at bytes 12-15 (line 0, cols 12-15)
        let source = indoc! {"
            fn evaluate(val) { va }
        "};
        let error = Diagnostic::error(
            "Use of undeclared variable `va`, did you mean the parameter `val`?",
            span(19, 21, 0, 19, 0, 21),
        )
        .with_label(
            "parameter `val` is defined here",
            span(12, 15, 0, 12, 0, 15),
        );

        assert_snapshot!(render_semantic_diagnostics("test.tlang", source, &[error], false), @r"
        error: Use of undeclared variable `va`, did you mean the parameter `val`?
          ┌─ test.tlang:1:20
          │
        1 │ fn evaluate(val) { va }
          │             ---    ^^
          │             │       
          │             parameter `val` is defined here
        ");
    }

    #[test]
    fn renders_error_at_span() {
        use super::render_error_at_span;

        let source = "use math::secret;";
        let sp = Span::new(10, 16, LineColumn::new(0, 10), LineColumn::new(0, 16));

        assert_snapshot!(render_error_at_span("lib.tlang", source, "symbol `secret` is not public", &sp, false), @r"
        error: symbol `secret` is not public
          ┌─ lib.tlang:1:11
          │
        1 │ use math::secret;
          │           ^^^^^^
        ");
    }

    #[test]
    fn render_diagnostics_unifies_parse_and_semantic() {
        // Verify that parse issues convert to diagnostics and render identically
        // to using render_parse_issues directly.
        let source = "let x = ;";
        let issues = vec![make_parse_issue(
            "Expected expression, found Semicolon",
            8,
            9,
            0,
            8,
            0,
            9,
        )];
        let diagnostics: Vec<Diagnostic> = issues.iter().map(Diagnostic::from).collect();

        let via_parse_issues = render_parse_issues("test.tlang", source, &issues, false);
        let via_diagnostics = render_diagnostics("test.tlang", source, &diagnostics, false);
        assert_eq!(via_parse_issues, via_diagnostics);

        // Also verify diagnostics_from_parse_error helper
        let err = tlang_parser::error::ParseError::new(issues);
        let converted = diagnostics_from_parse_error(&err);
        let via_converted = render_diagnostics("test.tlang", source, &converted, false);
        assert_eq!(via_parse_issues, via_converted);
    }
}
