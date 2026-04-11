use indoc::indoc;
use pretty_assertions::assert_eq;

mod common;

// ── Protocol declaration ─────────────────────────────────────────────────────

#[test]
fn test_protocol_declaration() {
    let output = compile!(indoc! {"
        protocol Showable {
            fn show(self)
        }
    "});
    let expected = indoc! {"
        const $Showable = $protocol({ show: null });
    "};
    assert_eq!(output, expected);
}

#[test]
fn test_protocol_multiple_methods() {
    let output = compile!(indoc! {"
        protocol Container {
            fn empty(self)
            fn insert(self, value)
            fn contains(self, value)
        }
    "});
    assert!(output.contains("const $Container = $protocol({"));
    assert!(output.contains("empty: null"));
    assert!(output.contains("insert: null"));
    assert!(output.contains("contains: null"));
}

// ── Protocol impl block ───────────────────────────────────────────────────────

#[test]
fn test_impl_block_for_struct() {
    let output = compile!(indoc! {"
        protocol Describable {
            fn describe(self)
        }

        struct Point { x: isize, y: isize }

        impl Describable for Point {
            fn describe(self) { self.x }
        }
    "});
    assert!(output.contains("const $Describable = $protocol({ describe: null });"));
    assert!(output.contains("$impl($Describable, Point,"));
    assert!(output.contains("describe:"));
}

#[test]
fn test_impl_block_method_body() {
    let output = compile!(indoc! {"
        protocol Scalable {
            fn scale(self, factor)
        }

        struct Vec2 { x: isize, y: isize }

        impl Scalable for Vec2 {
            fn scale(self, factor) {
                Vec2 { x: self.x * factor, y: self.y * factor }
            }
        }
    "});
    assert!(output.contains("$impl($Scalable, Vec2,"));
    assert!(output.contains("Vec2"));
}

/// Verifies that the builtin `List` type maps to JavaScript's native `Array`
/// constructor in `$impl` calls, using the centralized `BUILTIN_TYPE_JS_CONSTRUCTORS`
/// registry and HirId-based builtin detection (`hir_id().is_none()`).
#[test]
fn test_impl_block_for_builtin_list_uses_array_constructor() {
    let output = compile!(indoc! {"
        protocol Countable {
            fn count(self)
        }

        impl Countable for List {
            fn count(self) { len(self) }
        }
    "});
    // Builtin `List` → JS `Array` constructor.
    assert!(
        output.contains("$impl($Countable, Array,"),
        "expected `$impl($Countable, Array, ...)` but got:\n{output}"
    );
}

/// A user-defined enum named `List` has a HirId and must NOT be remapped to
/// `Array` — it keeps its own JS class name.
#[test]
fn test_impl_block_for_user_defined_list_enum_keeps_name() {
    let output = compile!(indoc! {"
        protocol Countable {
            fn count(self)
        }

        enum List { Nil, Cons(isize) }

        impl Countable for List {
            fn count(self) { 0 }
        }
    "});
    // User-defined `List` keeps its own name — must NOT become `Array`.
    assert!(
        output.contains("$impl($Countable, List,"),
        "expected `$impl($Countable, List, ...)` but got:\n{output}"
    );
    assert!(
        !output.contains("$impl($Countable, Array,"),
        "user-defined List must not be remapped to Array, but got:\n{output}"
    );
}

// ── Protocol path expressions ─────────────────────────────────────────────────

#[test]
fn test_protocol_method_call_via_path() {
    let output = compile!(indoc! {"
        protocol Countable {
            fn count(self)
        }

        impl Countable for List {
            fn count(self) { len(self) }
        }

        fn total(xs) { Countable::count(xs) }
    "});
    assert!(output.contains("$Countable.count(xs)"));
}

/// `self.method(args)` inside a protocol default implementation is rewritten
/// to `Protocol::method(self, args)` during lowering, and the JS codegen must
/// emit a proper `$Protocol.method(self, args)` call rather than a
/// field-access call.
#[test]
fn test_protocol_default_impl_self_dispatch() {
    let output = compile!(indoc! {"
        protocol Displayable {
            fn display(self)
            fn display_with_prefix(self, prefix) {
                prefix + self.display()
            }
        }

        impl Displayable for List {
            fn display(self) { log(self) }
        }
    "});
    // The rewritten call must use protocol dispatch, not a raw field access.
    assert!(
        output.contains("$Displayable.display(self)"),
        "expected `$Displayable.display(self)` in output:\n{output}"
    );
}

/// Calls to methods that are NOT part of the protocol should not be rewritten.
#[test]
fn test_protocol_default_impl_non_protocol_method_unchanged() {
    let output = compile!(indoc! {"
        protocol Greet {
            fn greet(self)
            fn annotated_greet(self) {
                self.greet()
            }
        }

        impl Greet for List {
            fn greet(self) { log(self) }
        }
    "});
    // The `greet` call on `self` must be rewritten to qualified dispatch.
    assert!(
        output.contains("$Greet.greet(self)"),
        "expected `$Greet.greet(self)` in output:\n{output}"
    );
}

#[test]
fn test_protocol_used_as_value() {
    // Assigning a protocol method to a variable should use the $-prefixed name.
    let output = compile!(indoc! {"
        protocol Printable {
            fn print(self)
        }

        impl Printable for List {
            fn print(self) { log(self) }
        }

        let p = Printable::print;
    "});
    assert!(output.contains("let p = $Printable.print;"));
}

#[test]
fn test_implements_operator() {
    let output = compile!(indoc! {"
        protocol Drawable {
            fn draw(self)
        }

        fn check(w) { w implements Drawable }
    "});
    assert!(output.contains("$Drawable.$implements(w)"));
}

// ── Blanket impl blocks ───────────────────────────────────────────────────────

/// A blanket impl (`impl<T> Protocol for T`) should pass `null` as the
/// Type argument to `$impl`, indicating it's a wildcard/default impl.
#[test]
fn test_blanket_impl_passes_null_type() {
    let output = compile!(indoc! {"
        protocol Showable {
            fn show(self)
        }

        impl<T> Showable for T {
            fn show(self) { self }
        }
    "});
    assert!(
        output.contains("$impl($Showable, null,"),
        "blanket impl should pass null as Type, but got:\n{output}"
    );
    assert!(output.contains("show:"));
}

/// A blanket impl with where clause constraints should pass the constraint
/// protocol references as an array argument to `$impl`.
#[test]
fn test_blanket_impl_with_where_clause_passes_constraints() {
    let output = compile!(indoc! {"
        protocol Iterable {
            fn iter(self)
        }

        protocol Mappable {
            fn map(self, f)
        }

        impl<I> Mappable for I
        where
            I: Iterable
        {
            fn map(self, f) { f(self) }
        }
    "});
    assert!(
        output.contains("$impl($Mappable, null,"),
        "blanket impl should pass null as Type, but got:\n{output}"
    );
    // Should include constraints array with $Iterable reference
    assert!(
        output.contains("$Iterable"),
        "blanket impl with where clause should reference $Iterable, but got:\n{output}"
    );
}

/// Concrete impls should still generate normal `$impl` calls with the
/// actual Type constructor (no null).
#[test]
fn test_concrete_impl_still_uses_type_constructor() {
    let output = compile!(indoc! {"
        protocol Showable {
            fn show(self)
        }

        struct Point { x: isize, y: isize }

        impl Showable for Point {
            fn show(self) { self.x }
        }
    "});
    assert!(
        output.contains("$impl($Showable, Point,"),
        "concrete impl should pass actual Type, but got:\n{output}"
    );
}

/// Generic impls on concrete target types must still pass the concrete
/// Type constructor instead of being treated like blanket impls.
#[test]
fn test_generic_impl_on_concrete_type_still_uses_type_constructor() {
    let output = compile!(indoc! {"
        protocol Into<T> {
            fn into(self)
        }

        struct Text { value: isize }

        impl<T> Into<T> for Text {
            fn into(self) { self.value }
        }
    "});
    assert!(
        output.contains("$impl($Into, Text,"),
        "generic impl for concrete type should pass actual Type, but got:\n{output}"
    );
}

// ── Variable declaration with list destructuring (rest pattern) ────────────────

#[test]
fn test_let_list_pattern_with_rest() {
    let output = compile!("let [head, ...tail] = [1, 2, 3];");
    assert_eq!(
        output,
        "let [head, ...tail] = [\n    1,\n    2,\n    3\n];\n"
    );
}

// ── Dict expressions ──────────────────────────────────────────────────────────

#[test]
fn test_dict_literal() {
    // Dict literal must be in value position (e.g. rhs of let) to avoid
    // ambiguity with block expressions.
    let output = compile!(indoc! {"
        fn make() {
            let d = { key: 42 };
            d
        }
    "});
    assert!(output.contains("key: 42"));
}

// ── Field access ──────────────────────────────────────────────────────────────

#[test]
fn test_field_access() {
    let output = compile!(indoc! {"
        struct Pair { first: isize, second: isize }
        fn swap(p) { Pair { first: p.second, second: p.first } }
    "});
    assert!(output.contains("p.second"));
    assert!(output.contains("p.first"));
}

// ── Index access ──────────────────────────────────────────────────────────────

#[test]
fn test_index_access() {
    let output = compile!("fn head(xs) { xs[0] }");
    assert_eq!(output, "function head(xs) {\n    return xs[0];\n}\n");
}

// ── Unary operators ───────────────────────────────────────────────────────────

#[test]
fn test_unary_not() {
    let output = compile!("fn negate(b) { !b }");
    assert_eq!(output, "function negate(b) {\n    return !b;\n}\n");
}

#[test]
fn test_unary_neg() {
    let output = compile!("fn neg(x) { -x }");
    assert_eq!(output, "function neg(x) {\n    return -x;\n}\n");
}
