use indoc::indoc;
use tlang_parser::Parser;
use tlang_semantics::analyzer::SemanticAnalyzer;

mod common;

macro_rules! analyze_diag {
    ($source:expr) => {{
        let mut parser = Parser::from_source($source);
        let (mut ast, _) = parser.parse().unwrap();
        let mut analyzer = SemanticAnalyzer::default();
        let _ = analyzer.analyze(&mut ast);
        analyzer.get_diagnostics().to_owned()
    }};
}

// ── self parameter ───────────────────────────────────────────────────────────

#[test]
fn error_when_self_missing_in_impl() {
    let diagnostics = analyze_diag!(indoc! {"
        protocol Eq {
            fn equals(self, other) -> bool
        }

        struct Thing {}

        impl Eq for Thing {
            fn equals(a, b) -> bool { true }
        }
    "});

    let proto_impl_errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.message().contains("missing required `self` parameter"))
        .collect();
    assert_eq!(
        proto_impl_errors.len(),
        1,
        "expected exactly one 'missing self' error, got {diagnostics:#?}"
    );
    assert!(proto_impl_errors[0].is_error());
    assert!(
        proto_impl_errors[0].message().contains("Eq::equals"),
        "error should name the protocol and method"
    );
}

#[test]
fn error_when_impl_has_unexpected_self() {
    let diagnostics = analyze_diag!(indoc! {"
        protocol Add {
            fn add(a, b)
        }

        struct Counter {}

        impl Add for Counter {
            fn add(self, b) { 0 }
        }
    "});

    let errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.message().contains("unexpected `self`"))
        .collect();
    assert_eq!(
        errors.len(),
        1,
        "expected one 'unexpected self' error, got {diagnostics:#?}"
    );
    assert!(errors[0].is_error());
    assert!(errors[0].message().contains("Add::add"));
}

#[test]
fn no_error_when_self_matches() {
    let diagnostics = analyze_diag!(indoc! {"
        protocol Greet {
            fn greet(self)
        }

        struct Person {}

        impl Greet for Person {
            fn greet(self) { 0 }
        }
    "});

    let proto_errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.message().contains("missing required `self` parameter")
                || d.message().contains("unexpected `self`")
        })
        .collect();
    assert!(
        proto_errors.is_empty(),
        "expected no self-related errors, got {proto_errors:#?}"
    );
}

#[test]
fn no_error_when_neither_has_self() {
    let diagnostics = analyze_diag!(indoc! {"
        protocol Math {
            fn add(a, b)
        }

        struct Nums {}

        impl Math for Nums {
            fn add(x, y) { 0 }
        }
    "});

    let proto_errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| {
            d.message().contains("missing required `self` parameter")
                || d.message().contains("unexpected `self`")
                || d.message().contains("parameter(s)")
        })
        .collect();
    assert!(
        proto_errors.is_empty(),
        "expected no self/count errors, got {proto_errors:#?}"
    );
}

// ── parameter count ───────────────────────────────────────────────────────────

#[test]
fn error_on_parameter_count_mismatch() {
    let diagnostics = analyze_diag!(indoc! {"
        protocol Cmp {
            fn compare(self, other, extra)
        }

        struct Item {}

        impl Cmp for Item {
            fn compare(self, other) { 0 }
        }
    "});

    let count_errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.message().contains("parameter(s)"))
        .collect();
    assert_eq!(
        count_errors.len(),
        1,
        "expected one parameter count error, got {diagnostics:#?}"
    );
    assert!(count_errors[0].is_error());
    assert!(count_errors[0].message().contains("Cmp::compare"));
}

#[test]
fn no_error_when_parameter_counts_match() {
    let diagnostics = analyze_diag!(indoc! {"
        protocol Cmp {
            fn compare(self, other)
        }

        struct Item {}

        impl Cmp for Item {
            fn compare(self, other) { 0 }
        }
    "});

    let count_errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.message().contains("parameter(s)"))
        .collect();
    assert!(
        count_errors.is_empty(),
        "expected no count errors, got {count_errors:#?}"
    );
}

// ── diagnostic label points to protocol definition ───────────────────────────

#[test]
fn diagnostic_has_label_pointing_to_protocol() {
    let diagnostics = analyze_diag!(indoc! {"
        protocol Eq {
            fn equals(self, other) -> bool
        }

        struct Val {}

        impl Eq for Val {
            fn equals(a, b) -> bool { true }
        }
    "});

    let self_errors: Vec<_> = diagnostics
        .iter()
        .filter(|d| d.message().contains("missing required `self` parameter"))
        .collect();
    assert_eq!(self_errors.len(), 1);

    let labels = self_errors[0].labels();
    assert!(
        !labels.is_empty(),
        "expected a 'defined here' label on the diagnostic"
    );
    assert!(
        labels[0].message.contains("Eq::equals"),
        "label should identify the protocol method"
    );
}
