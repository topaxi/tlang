use std::ops::Range;

use codespan_reporting::{
    diagnostic::{Diagnostic as ReportDiagnostic, Label, Severity as ReportSeverity},
    files::SimpleFiles,
    term::{self, Config, termcolor::Buffer},
};
use tlang_parser::error::ParseIssue;
use tlang_semantics::diagnostic::{Diagnostic, Severity};
use tlang_span::Span;

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

fn parse_issue_report(
    file_id: usize,
    source_len: usize,
    issue: &ParseIssue,
) -> ReportDiagnostic<usize> {
    ReportDiagnostic::new(ReportSeverity::Error)
        .with_message(&issue.msg)
        .with_labels(vec![Label::primary(
            file_id,
            label_range(&issue.span, source_len),
        )])
}

/// Render parse issues from a failed parse into a human-readable, source-aware
/// string using `codespan_reporting`.
///
/// # Panics
///
/// Panics if writing to the internal buffer fails, which cannot happen with
/// the in-memory [`Buffer`] used internally.
pub fn render_parse_issues(source_name: &str, source: &str, issues: &[ParseIssue]) -> String {
    let mut files = SimpleFiles::new();
    let file_id = files.add(source_name, source);
    let mut writer = Buffer::no_color();
    let config = Config::default();

    for issue in issues {
        let report = parse_issue_report(file_id, source.len(), issue);
        // SimpleFiles never returns an error; the expect is unreachable.
        term::emit_to_write_style(&mut writer, &config, &files, &report)
            .expect("codespan diagnostic rendering should succeed");
    }

    String::from_utf8_lossy(writer.as_slice()).into_owned()
}

/// Render semantic diagnostics into a human-readable, source-aware string using
/// `codespan_reporting`.
///
/// # Panics
///
/// Panics if writing to the internal buffer fails, which cannot happen with
/// the in-memory [`Buffer`] used internally.
pub fn render_semantic_diagnostics(
    source_name: &str,
    source: &str,
    diagnostics: &[Diagnostic],
) -> String {
    let mut files = SimpleFiles::new();
    let file_id = files.add(source_name, source);
    let mut writer = Buffer::no_color();
    let config = Config::default();

    for diagnostic in diagnostics {
        let report = diagnostic_report(file_id, source.len(), diagnostic);
        // SimpleFiles never returns an error; the expect is unreachable.
        term::emit_to_write_style(&mut writer, &config, &files, &report)
            .expect("codespan diagnostic rendering should succeed");
    }

    String::from_utf8_lossy(writer.as_slice()).into_owned()
}

#[cfg(test)]
mod tests {
    use indoc::indoc;
    use insta::assert_snapshot;
    use tlang_parser::Parser;
    use tlang_parser::error::{ParseIssue, ParseIssueKind};
    use tlang_semantics::SemanticAnalyzer;
    use tlang_span::{LineColumn, Span};

    use super::{render_parse_issues, render_semantic_diagnostics};

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

        assert_snapshot!(render_parse_issues("test.tlang", source, &issues), @r"
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

        assert_snapshot!(render_parse_issues("test.tlang", source, &issues), @r"
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
        let source = indoc! {"
            fn unused() {
                missing;
            }
        "};
        let mut parser = Parser::from_source(source);
        let (mut ast, _) = parser.parse().expect("source should parse");
        let mut analyzer = SemanticAnalyzer::default();
        let _ = analyzer.analyze(&mut ast);

        assert_snapshot!(render_semantic_diagnostics(
            "test.tlang",
            source,
            &analyzer.get_diagnostics(),
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
        let source = indoc! {"
            let unused_var = 42;
        "};
        let mut parser = Parser::from_source(source);
        let (mut ast, _) = parser.parse().expect("source should parse");
        let mut analyzer = SemanticAnalyzer::default();
        let _ = analyzer.analyze(&mut ast);
        let warnings: Vec<_> = analyzer
            .get_diagnostics()
            .into_iter()
            .filter(|d| d.is_warning())
            .collect();

        assert_snapshot!(render_semantic_diagnostics("test.tlang", source, &warnings), @r"
        warning: Unused variable `unused_var`, if this is intentional, prefix the name with an underscore: `_unused_var`
          ┌─ test.tlang:1:5
          │
        1 │ let unused_var = 42;
          │     ^^^^^^^^^^
        ");
    }

    #[test]
    fn renders_did_you_mean_with_secondary_label() {
        let source = indoc! {"
            fn evaluate(val) { va }
        "};
        let mut parser = Parser::from_source(source);
        let (mut ast, _) = parser.parse().expect("source should parse");
        let mut analyzer = SemanticAnalyzer::default();
        let _ = analyzer.analyze(&mut ast);
        let errors: Vec<_> = analyzer
            .get_diagnostics()
            .into_iter()
            .filter(|d| d.is_error())
            .collect();

        assert_snapshot!(render_semantic_diagnostics("test.tlang", source, &errors), @r"
        error: Use of undeclared variable `va`, did you mean the parameter `val`?
          ┌─ test.tlang:1:20
          │
        1 │ fn evaluate(val) { va }
          │             ---    ^^
          │             │       
          │             parameter `val` is defined here
        ");
    }
}
