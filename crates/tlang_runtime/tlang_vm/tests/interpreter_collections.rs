#![cfg(feature = "binary")]

use tlang_memory::prelude::*;

mod common;

// ── len() ─────────────────────────────────────────────────────────────────────

#[test]
fn test_len_on_list() {
    let value = common::eval("len([1, 2, 3])");
    assert_eq!(value, TlangValue::U64(3));
}

#[test]
fn test_len_on_empty_list() {
    let value = common::eval("len([])");
    assert_eq!(value, TlangValue::U64(0));
}

#[test]
fn test_len_on_string() {
    let value = common::eval(r#"len("hello")"#);
    assert_eq!(value, TlangValue::U64(5));
}

// ── Functor::map on lists ─────────────────────────────────────────────────────

#[test]
fn test_list_functor_map() {
    let s = common::eval_to_string("Functor::map([1, 2, 3], fn(x) { x * 2 })");
    assert_eq!(s, "[2, 4, 6]");
}

#[test]
fn test_list_functor_map_empty() {
    let s = common::eval_to_string("Functor::map([], fn(x) { x * 2 })");
    assert_eq!(s, "[]");
}

#[test]
fn test_list_functor_map_pipeline() {
    let s = common::eval_to_string("[10, 20, 30] |> Functor::map(fn(x) { x + 1 })");
    assert_eq!(s, "[11, 21, 31]");
}

// ── Functor::map on Option ────────────────────────────────────────────────────

#[test]
fn test_option_some_functor_map() {
    let s = common::eval_to_string("Functor::map(Option::Some(5), fn(x) { x * 3 })");
    assert_eq!(s, "Option::Some(0: 15)");
}

#[test]
fn test_option_none_functor_map() {
    let s = common::eval_to_string("Functor::map(Option::None, fn(x) { x * 3 })");
    assert_eq!(s, "Option::None");
}

// ── Functor::map on strings ───────────────────────────────────────────────────

#[test]
fn test_string_functor_map_identity() {
    // map each character through an identity fn → same string
    let s = common::eval_to_string(r#"Functor::map("abc", fn(ch) { ch })"#);
    assert_eq!(s, "abc");
}

#[test]
fn test_string_functor_map_empty() {
    let s = common::eval_to_string(r#"Functor::map("", fn(ch) { ch })"#);
    assert_eq!(s, "");
}

// ── List.slice ────────────────────────────────────────────────────────────────

// Note: slices are displayed with a `&` prefix, e.g. `&[2, 3, 4]`

#[test]
fn test_list_slice_mid_range() {
    let s = common::eval_to_string("let xs = [1, 2, 3, 4, 5]; xs.slice(1, 4)");
    assert_eq!(s, "&[2, 3, 4]");
}

#[test]
fn test_list_slice_to_end() {
    let s = common::eval_to_string("let xs = [1, 2, 3, 4, 5]; xs.slice(2, 5)");
    assert_eq!(s, "&[3, 4, 5]");
}

#[test]
fn test_list_slice_from_start() {
    let s = common::eval_to_string("let xs = [10, 20, 30]; xs.slice(0, 2)");
    assert_eq!(s, "&[10, 20]");
}

// ── Iterable::iter — for-in over a list ───────────────────────────────────────

#[test]
fn test_list_iterable_for_in() {
    let value = common::eval(
        "let total = for x in [1, 2, 3, 4, 5]; with sum = 0 {
             sum + x
         };
         total",
    );
    assert_eq!(value, TlangValue::U64(15));
}

// ── Functor::map on slices ────────────────────────────────────────────────────

#[test]
fn test_slice_functor_map() {
    // Functor::map on a slice returns a regular list
    let s = common::eval_to_string(
        "let xs = [1, 2, 3, 4, 5];
         let s = xs.slice(0, 3);
         Functor::map(s, fn(x) { x * 10 })",
    );
    assert_eq!(s, "[10, 20, 30]");
}

// ── Iterable::iter — for-in over a slice ─────────────────────────────────────

#[test]
fn test_slice_iterable_for_in() {
    let value = common::eval(
        "let xs = [1, 2, 3, 4, 5];
         let s = xs.slice(1, 4);
         let total = for x in s; with sum = 0 {
             sum + x
         };
         total",
    );
    // slice indices 1..4 → elements 2, 3, 4 → sum 9
    assert_eq!(value, TlangValue::U64(9));
}

// ── Iterable::iter — for-in over a string ────────────────────────────────────

#[test]
fn test_string_iterable_for_in() {
    let s = common::eval_to_string(
        r#"let result = for ch in "abc"; with acc = [] {
               [...acc, ch]
           };
           result"#,
    );
    assert_eq!(s, "[a, b, c]");
}

// ── ListIterator explicit iteration ──────────────────────────────────────────

#[test]
fn test_list_iterator_next() {
    // Explicitly call Iterator::next to verify the iterator protocol
    let s = common::eval_to_string(
        "let it = Iterable::iter([10, 20]);
         let first = Iterator::next(it);
         let second = Iterator::next(it);
         let third = Iterator::next(it);
         [first, second, third]",
    );
    assert_eq!(
        s,
        "[Option::Some(0: 10), Option::Some(0: 20), Option::None]"
    );
}
