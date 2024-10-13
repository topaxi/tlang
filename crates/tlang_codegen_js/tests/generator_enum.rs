use indoc::indoc;
use pretty_assertions::assert_eq;

mod common;

#[test]
fn test_enums() {
    let output = compile!(indoc! {"
        enum Option {
            Some(x),
            None,
        }

        fn main() {
            let x = Option::Some(42);
        }
    "});
    let expected_output = indoc! {"
        const Option = {
            Some(x) {
                return {
                    tag: \"Some\",
                    [0]: x,
                };
            },
            None: { tag: \"None\" },
        };
        function main() {
            let x = Option.Some(42);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_enums_with_fields() {
    let output = compile!(indoc! {"
        enum Node {
            BinaryOperation {
                operator,
                left,
                right,
            },
        }

        fn main() {
            let x = Node::BinaryOperation {
                operator: \"+\",
                left: 1,
                right: 2,
            };
        }
    "});
    let expected_output = indoc! {"
        const Node = {
            BinaryOperation({ operator, left, right }) {
                return {
                    tag: \"BinaryOperation\",
                    operator,
                    left,
                    right,
                };
            },
        };
        function main() {
            let x = Node.BinaryOperation({
                operator: \"+\",
                left: 1,
                right: 2,
            });
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_enums_tree_implementation() {
    let output = compile!(indoc! {"
        enum Tree {
            Leaf(x),
            Node {
                left,
                right,
            },
        }

        fn main() {
            let x = Tree::Node {
                left: Tree::Leaf(1),
                right: Tree::Leaf(2),
            };
        }
    "});
    let expected_output = indoc! {"
        const Tree = {
            Leaf(x) {
                return {
                    tag: \"Leaf\",
                    [0]: x,
                };
            },
            Node({ left, right }) {
                return {
                    tag: \"Node\",
                    left,
                    right,
                };
            },
        };
        function main() {
            let x = Tree.Node({
                left: Tree.Leaf(1),
                right: Tree.Leaf(2),
            });
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_maximum_depth_tree() {
    let output = compile!(indoc! {"
        enum Tree {
            Leaf(x),
            Node {
                left,
                right,
            },
        }

        fn maximum_depth(Tree::Leaf(_)) { 1 }
        fn maximum_depth(Tree::Node { left, right }) { 1 + math::max(maximum_depth(left), maximum_depth(right)) }

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
    let expected_output = indoc! {"
        const Tree = {
            Leaf(x) {
                return {
                    tag: \"Leaf\",
                    [0]: x,
                };
            },
            Node({ left, right }) {
                return {
                    tag: \"Node\",
                    left,
                    right,
                };
            },
        };
        function maximum_depth(tree) {
            if (tree.tag === \"Leaf\") {
                return 1;
            } else if (tree.tag === \"Node\") {
                let left = tree.left;
                let right = tree.right;
                return 1 + Math.max(maximum_depth(left), maximum_depth(right));
            }
        }
        function main() {
            let x = Tree.Node({
                left: Tree.Leaf(1),
                right: Tree.Node({
                    left: Tree.Leaf(2),
                    right: Tree.Leaf(3),
                }),
            });
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_maximum_depth_tree_positional() {
    let output = compile!(indoc! {"
        enum Tree {
            Leaf(x),
            Node(left, right),
        }

        fn maximum_depth(Tree::Leaf(_)) { 1 }
        fn maximum_depth(Tree::Node(left, right)) { 1 + math::max(maximum_depth(left), maximum_depth(right)) }
    "});
    let expected_output = indoc! {"
        const Tree = {
            Leaf(x) {
                return {
                    tag: \"Leaf\",
                    [0]: x,
                };
            },
            Node(left, right) {
                return {
                    tag: \"Node\",
                    [0]: left,
                    [1]: right,
                };
            },
        };
        function maximum_depth(tree) {
            if (tree.tag === \"Leaf\") {
                return 1;
            } else if (tree.tag === \"Node\") {
                let left = tree[0];
                let right = tree[1];
                return 1 + Math.max(maximum_depth(left), maximum_depth(right));
            }
        }
    "};
    assert_eq!(output, expected_output);
}
