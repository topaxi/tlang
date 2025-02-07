mod common;

use pretty_assertions::assert_eq;
use tlang_hir::hir::HirScope;

use self::common::hir_from_str;

#[test]
fn test_enum_variant_slots() {
    let hir = hir_from_str(
        r#"
            enum Foo {
                Bar,
                Baz,
                Qux,
            }
        "#,
    );

    assert_eq!(hir.locals(), 3);
}

// Tagged variants are functions/constructed and do not have a slot themselves.
#[test]
fn test_enum_tagged_variant_slots() {
    let hir = hir_from_str(
        r#"
            enum Foo {
                Bar,
                Baz(i32),
                Qux(i32, i32),
            }
        "#,
    );

    assert_eq!(hir.locals(), 1);
}
