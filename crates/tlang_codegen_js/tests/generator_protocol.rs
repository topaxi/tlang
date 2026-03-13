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
        const $Showable = {};
        $Showable.show = function(self, ...args) {
            const __type = Array.isArray(self) ? \"List\" : self?.constructor?.name ?? typeof self;
            return $Showable[__type].show(self, ...args);
        };
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
    assert!(output.contains("const $Container = {};"));
    assert!(output.contains("$Container.empty = function(self, ...args)"));
    assert!(output.contains("$Container.insert = function(self, ...args)"));
    assert!(output.contains("$Container.contains = function(self, ...args)"));
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
    assert!(output.contains("const $Describable = {};"));
    assert!(output.contains("$Describable.Point = {};"));
    assert!(output.contains("$Describable.Point.describe = "));
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
    assert!(output.contains("$Scalable.Vec2.scale = "));
    assert!(output.contains("Vec2"));
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

// ── Variable declaration with list destructuring (rest pattern) ────────────────

#[test]
fn test_let_list_pattern_with_rest() {
    let output = compile!("let [head, ...tail] = [1, 2, 3];");
    assert_eq!(output, "let [head, ...tail] = [1, 2, 3];\n");
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
