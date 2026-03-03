use insta::assert_snapshot;

mod common;

use self::common::{hir_from_str_analyzed, pretty_print};

#[test]
fn test_enum_param_type_inferred_from_variants() {
    let hir = hir_from_str_analyzed(
        r#"
            enum LinkedList {
                Empty,
                Node(int, LinkedList),
            }

            fn sum_list(LinkedList::Empty) { 0 }
            fn sum_list(LinkedList::Node(value, rest)) {
                value + sum_list(rest)
            }
        "#,
    );

    assert_snapshot!(pretty_print(&hir), @r###"
    enum LinkedList {
        Empty {
        }
        Node {
            0: int,
            1: LinkedList,
        }
    }
    fn sum_list(linkedlist: LinkedList) -> unknown {
        match linkedlist {
            LinkedList::Empty => {
                0
            },
            LinkedList::Node { 0: value, 1: rest } => {
                (value + sum_list(rest))
            },
        }
    }
    "###);
}

#[test]
fn test_enum_param_type_not_inferred_when_mixed() {
    // If different declarations at the same position use different enum types,
    // no inference should occur and the type stays `unknown`.
    let hir = hir_from_str_analyzed(
        r#"
            enum Foo { A }
            enum Bar { B }

            fn f(Foo::A) { 1 }
            fn f(Bar::B) { 2 }
        "#,
    );

    let output = pretty_print(&hir);
    assert!(
        output.contains("f(arg0: unknown)"),
        "Expected unknown type when enum types conflict, got:\n{output}"
    );
}
