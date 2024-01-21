use indoc::indoc;

mod common;

#[test]
fn test_enums() {
    assert_parser_snapshot!(indoc! {"
        enum Option {
            Some(x),
            None,
        }

        let x = Option::Some(42);
    "});
}

#[test]
fn test_enums_with_fields() {
    assert_parser_snapshot!(indoc! {"
        enum Option {
            Some { x },
            None,
        }
        let x = Option::Some { x: 42 };
    "});
}

#[test]
fn test_enum_tree_max_depth() {
    assert_parser_snapshot!(indoc! {"
        enum Tree {
            Leaf(value),
            Node { left, right },
        }

        fn maximum_depth(Tree::Leaf(_)) { 1 }
        fn maximum_depth(Tree::Node { left, right }) { 1 + max(maximum_depth(left), maximum_depth(right)) }

        fn main() {
            let x = Tree::Node {
                left: Tree::Leaf(1),
                right: Tree::Node {
                    left: Tree::Leaf(2),
                    right: Tree::Leaf(3),
                },
            };
        }
    "});
}

#[test]
fn test_enum_extraction() {
    assert_parser_snapshot!(indoc! {"
        enum Option {
            Some(value),
            None,
        }

        fn unwrap(Option::None) { panic(\"Cannot unwrap None\") }
        fn unwrap(Option::Some(value)) { value }
    "});
}
