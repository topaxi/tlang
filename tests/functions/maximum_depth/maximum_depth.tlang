enum Tree {
    Leaf(unknown),
    Node { left, right },
}

// maximum_depth(Tree) -> Int
fn maximum_depth(Tree::Leaf(_)) { 1 }
fn maximum_depth(Tree::Node { left, right }) {
    1 + math::max(maximum_depth(left), maximum_depth(right))
}

let x = Tree::Node {
    left: Tree::Leaf(1),
    right: Tree::Node {
        left: Tree::Leaf(2),
        right: Tree::Node {
            left: Tree::Node {
                left: Tree::Leaf(4),
                right: Tree::Leaf(5),
            },
            right: Tree::Leaf(6),
        },
    },
};

x
|> maximum_depth()
|> log();
