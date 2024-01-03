use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_ast::symbols::SymbolType;
use tlang_codegen_js::generator::CodegenJS;
use tlang_parser::parser::Parser;
use tlang_semantics::SemanticAnalyzer;

macro_rules! compile {
    ($source:expr) => {{
        let mut parser = Parser::from_source($source);
        let mut ast = parser.parse();
        let mut semantic_analyzer = SemanticAnalyzer::default();
        semantic_analyzer.add_builtin_symbols(vec![
            ("log", SymbolType::Function),
            ("max", SymbolType::Function),
            ("min", SymbolType::Function),
        ]);
        semantic_analyzer.analyze(&mut ast);
        let mut codegen = CodegenJS::default();
        codegen.generate_code(&ast);
        codegen.get_output().to_string()
    }};
}

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
                    \"0\": x,
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
                    \"0\": x,
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
    let expected_output = indoc! {"
        const Tree = {
            Leaf(x) {
                return {
                    tag: \"Leaf\",
                    \"0\": x,
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
        function maximum_depth(...args) {
            if (args[0].tag === \"Leaf\") {
                return 1;
            } else if (args[0].tag === \"Node\") {
                let left = args[0].left;
                let right = args[0].right;
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
        fn maximum_depth(Tree::Node(left, right)) { 1 + max(maximum_depth(left), maximum_depth(right)) }
    "});
    let expected_output = indoc! {"
        const Tree = {
            Leaf(x) {
                return {
                    tag: \"Leaf\",
                    \"0\": x,
                };
            },
            Node(left, right) {
                return {
                    tag: \"Node\",
                    \"0\": left,
                    \"1\": right,
                };
            },
        };
        function maximum_depth(...args) {
            if (args[0].tag === \"Leaf\") {
                return 1;
            } else if (args[0].tag === \"Node\") {
                let left = args[0][0];
                let right = args[0][1];
                return 1 + Math.max(maximum_depth(left), maximum_depth(right));
            }
        }
    "};
    assert_eq!(output, expected_output);
}
