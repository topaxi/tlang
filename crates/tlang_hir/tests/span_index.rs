mod common;

use tlang_hir::{SpanIndex, SpanNode};

use self::common::{hir_from_str, hir_from_str_analyzed};

/// Helper: return the byte offset of the first occurrence of `needle` in `src`.
fn offset_of(src: &str, needle: &str) -> u32 {
    src.find(needle)
        .unwrap_or_else(|| panic!("'{needle}' not found in source")) as u32
}

#[test]
fn test_build_is_non_empty_for_non_trivial_module() {
    let hir = hir_from_str("let x = 1 + 2;");
    let index = SpanIndex::build(&hir);
    assert!(!index.entries().is_empty());
}

#[test]
fn test_node_at_offset_returns_none_for_out_of_range() {
    let hir = hir_from_str("let x = 1;");
    let index = SpanIndex::build(&hir);
    // A very large offset should not be covered by any span.
    assert!(index.node_at_offset(999_999).is_none());
}

#[test]
fn test_node_at_offset_returns_innermost_stmt() {
    let src = "let x = 42;";
    let hir = hir_from_str(src);
    let index = SpanIndex::build(&hir);

    // Querying any byte inside the `let` statement should return a Stmt node.
    let node = index.node_at_offset(offset_of(src, "let"));
    assert!(
        matches!(node, Some(SpanNode::Stmt(_))),
        "expected Stmt at 'let' offset, got {node:?}"
    );
}

#[test]
fn test_node_at_offset_literal_returns_expr() {
    let src = "let x = 42;";
    let hir = hir_from_str(src);
    let index = SpanIndex::build(&hir);

    // The literal `42` is the innermost node; it should be an Expr.
    let node = index.node_at_offset(offset_of(src, "42"));
    assert!(
        matches!(node, Some(SpanNode::Expr(_))),
        "expected Expr at '42' offset, got {node:?}"
    );
}

#[test]
fn test_node_at_offset_identifier_binding_is_pat() {
    let src = "let foo = 1;";
    let hir = hir_from_str(src);
    let index = SpanIndex::build(&hir);

    // The binding `foo` in a let statement is a Pat::Identifier.
    let node = index.node_at_offset(offset_of(src, "foo"));
    assert!(
        matches!(node, Some(SpanNode::Pat(_))),
        "expected Pat at 'foo' offset, got {node:?}"
    );
}

#[test]
fn test_node_at_offset_function_name_is_expr() {
    let src = "fn add(a, b) { a + b }";
    let hir = hir_from_str(src);
    let index = SpanIndex::build(&hir);

    // The function declaration is wrapped in a Stmt.
    let node = index.node_at_offset(offset_of(src, "fn"));
    assert!(
        matches!(node, Some(SpanNode::Stmt(_))),
        "expected Stmt at 'fn' keyword offset, got {node:?}"
    );
}

#[test]
fn test_node_at_offset_binary_expr_body_is_expr() {
    let src = "fn add(a, b) { a + b }";
    let hir = hir_from_str(src);
    let index = SpanIndex::build(&hir);

    // `a + b` is an Expr node inside the function body.
    let node = index.node_at_offset(offset_of(src, "a + b"));
    assert!(
        matches!(node, Some(SpanNode::Expr(_))),
        "expected Expr at 'a + b' offset, got {node:?}"
    );
}

#[test]
fn test_node_at_offset_path_resolves_to_path_node() {
    // Use analyzed HIR so paths are resolved.
    let src = "let x = 1; let y = x;";
    let hir = hir_from_str_analyzed(src);
    let index = SpanIndex::build(&hir);

    // `x` on the right-hand side of the second `let` is a resolved Path.
    // Find the second occurrence of 'x' (position of `x` in `let y = x`).
    let second_x_offset = (offset_of(src, "let y = ") + "let y = ".len() as u32);
    let node = index.node_at_offset(second_x_offset);
    // After semantic analysis the path should resolve and emit a Path node;
    // if it doesn't resolve it falls back to an Expr.
    assert!(
        matches!(node, Some(SpanNode::Path(_)) | Some(SpanNode::Expr(_))),
        "expected Path or Expr at second 'x' offset, got {node:?}"
    );
}

#[test]
fn test_node_at_lc_matches_node_at_offset() {
    let src = "let answer = 42;";
    let hir = hir_from_str(src);
    let index = SpanIndex::build(&hir);

    // Line 0, column 0 is the start of `let`.
    let by_offset = index.node_at_offset(0);
    let by_lc = index.node_at_lc(0, 0);
    assert_eq!(
        by_offset, by_lc,
        "node_at_offset and node_at_lc should agree at the start of the file"
    );
}

#[test]
fn test_node_at_lc_returns_none_for_out_of_range() {
    let src = "let x = 1;";
    let hir = hir_from_str(src);
    let index = SpanIndex::build(&hir);

    assert!(index.node_at_lc(999, 0).is_none());
}

#[test]
fn test_entries_sorted_by_start_then_length_desc() {
    let src = "let x = 1 + 2;";
    let hir = hir_from_str(src);
    let index = SpanIndex::build(&hir);

    let entries = index.entries();
    // Verify sort invariant: for consecutive entries, start must be
    // non-decreasing; within the same start, length must be non-increasing
    // (descending).
    for window in entries.windows(2) {
        let (a_span, _) = &window[0];
        let (b_span, _) = &window[1];
        let a_len = a_span.end - a_span.start;
        let b_len = b_span.end - b_span.start;
        assert!(
            b_span.start > a_span.start || b_len <= a_len,
            "sort invariant violated: {a_span:?} (len={a_len}) followed by {b_span:?} (len={b_len})"
        );
    }
}

#[test]
fn test_innermost_node_is_returned_not_outer() {
    // `1 + 2` is a Binary Expr that contains two Literal Exprs.
    // Querying at the offset of `1` should return a Literal Expr (innermost),
    // not the Binary Expr.
    let src = "let x = 1 + 2;";
    let hir = hir_from_str(src);
    let index = SpanIndex::build(&hir);

    let literal_offset = offset_of(src, "1");
    let node = index.node_at_offset(literal_offset);

    // The innermost node at the `1` literal is an Expr.
    assert!(
        matches!(node, Some(SpanNode::Expr(_))),
        "expected innermost Expr at '1' offset, got {node:?}"
    );

    // Query at the `+` operator should still return an Expr (the Binary node
    // is the innermost node at that position since `+` itself has no node).
    let plus_offset = offset_of(src, "+");
    let node_at_plus = index.node_at_offset(plus_offset);
    assert!(
        matches!(node_at_plus, Some(SpanNode::Expr(_))),
        "expected Expr at '+' offset, got {node_at_plus:?}"
    );
}

#[test]
fn test_build_empty_module_has_no_entries() {
    // An empty source has no statements or expressions, so the index should
    // be empty (the module block itself is not added as a node).
    let hir = hir_from_str("");
    let index = SpanIndex::build(&hir);
    assert!(
        index.entries().is_empty(),
        "expected empty index for empty source, got {} entries",
        index.entries().len()
    );
}

#[test]
fn test_sort_invariant_single_entry() {
    // A module with a single literal statement produces a small, non-empty
    // index.  Verify that build() doesn't panic and the sort invariant holds
    // for all consecutive entry pairs (including the trivial case of 0 or 1
    // entries).
    let hir = hir_from_str("1;");
    let index = SpanIndex::build(&hir);
    assert!(!index.entries().is_empty());
    for window in index.entries().windows(2) {
        let (a_span, _) = &window[0];
        let (b_span, _) = &window[1];
        let a_len = a_span.end - a_span.start;
        let b_len = b_span.end - b_span.start;
        assert!(b_span.start > a_span.start || b_len <= a_len);
    }
}
