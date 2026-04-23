use insta::assert_snapshot;

mod common;

use self::common::{hir_from_str, pretty_print};

/// Calling a protocol method on `self` inside a default implementation is
/// rewritten to a fully-qualified `Protocol::method(self, …)` call during
/// lowering.
#[test]
fn test_protocol_self_dispatch_basic() {
    let hir = hir_from_str(
        r#"
            protocol Displayable {
                fn display(self)
                fn display_with_prefix(self, prefix) {
                    prefix + ": " + self.display()
                }
            }
        "#,
    );

    assert_snapshot!(pretty_print(&hir), @r#"
    protocol Displayable {
        fn display(self)
        fn display_with_prefix(self, prefix) {
            ((prefix + ": ") + Displayable::display(self))
        }
    }
    "#);
}

/// Multiple self-dispatch calls in the same default body are all rewritten.
#[test]
fn test_protocol_self_dispatch_multiple_calls() {
    let hir = hir_from_str(
        r#"
            protocol Foldable {
                fn foldl(self, init, f)
                fn sum(self) { self.foldl(0, fn (acc, x) { acc + x }) }
                fn length(self) { self.foldl(0, fn (acc, _) { acc + 1 }) }
            }
        "#,
    );

    assert_snapshot!(pretty_print(&hir), @r#"
    protocol Foldable {
        fn foldl(self, init, f)
        fn sum(self) {
            Foldable::foldl(self, 0, fn anonymous(acc: unknown, x: unknown) -> unknown {
                (acc + x)
            })
        }
        fn length(self) {
            Foldable::foldl(self, 0, fn anonymous(acc: unknown, _: unknown) -> unknown {
                (acc + 1)
            })
        }
    }
    "#);
}

/// Calls to methods NOT defined in the protocol are left untouched.
#[test]
fn test_protocol_self_dispatch_non_protocol_method_unchanged() {
    let hir = hir_from_str(
        r#"
            protocol Greet {
                fn greet(self)
                fn greet_loudly(self) {
                    self.greet()
                }
            }
        "#,
    );

    let output = pretty_print(&hir);
    // The `greet` call on `self` inside `greet_loudly` should be rewritten.
    assert!(
        output.contains("Greet::greet(self)"),
        "expected Greet::greet(self) in output:\n{output}"
    );
}

/// A call on `self` where the method is NOT in the protocol should not be
/// rewritten (it remains a field-access call).
#[test]
fn test_protocol_self_dispatch_only_own_methods_rewritten() {
    let hir = hir_from_str(
        r#"
            protocol Greet {
                fn greet(self)
                fn describe(self) {
                    self.some_other_method()
                }
            }
        "#,
    );

    let output = pretty_print(&hir);
    // `some_other_method` is not in the protocol, so it must remain a field access.
    assert!(
        !output.contains("Greet::some_other_method"),
        "non-protocol method should not be rewritten:\n{output}"
    );
}

/// Nested function expressions inside a default body do NOT inherit the
/// protocol dispatch context: `self.method()` inside the nested function
/// refers to the *nested* function's own `self`, not the outer protocol's.
#[test]
fn test_protocol_self_dispatch_not_applied_in_nested_fn() {
    let hir = hir_from_str(
        r#"
            protocol P {
                fn method(self)
                fn wrapper(self) {
                    let f = fn(x) { x.method() };
                    f(self)
                }
            }
        "#,
    );

    let output = pretty_print(&hir);
    // The `x.method()` inside the closure uses `x`, not `self`, so it must NOT
    // be rewritten to `P::method(x)`.
    assert!(
        !output.contains("P::method(x)"),
        "nested fn x.method() should not be rewritten:\n{output}"
    );
    // But `f(self)` at the outer level passes `self` as a plain argument
    // (not a method call), which should also be untouched.
    assert!(
        output.contains("f(self)"),
        "f(self) should remain as-is:\n{output}"
    );
}

#[test]
fn test_protocol_dispatch_on_constrained_generic_parameter() {
    let hir = hir_from_str(
        r#"
            fn print<T: Display>(value: T) {
                log(value.to_string())
            }
        "#,
    );

    let output = pretty_print(&hir);
    assert!(
        output.contains("Display::to_string(value)"),
        "expected constrained generic call to lower to protocol dispatch:\n{output}"
    );
    assert!(
        !output.contains("value.to_string"),
        "expected dot call to be rewritten during lowering:\n{output}"
    );
}
