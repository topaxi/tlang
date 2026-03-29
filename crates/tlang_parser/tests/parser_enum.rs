use indoc::indoc;
use tlang_parser::Parser;

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

#[test]
fn test_incomplete_enum_does_not_crash() {
    let mut parser = Parser::from_source("enum ");
    let result = parser.parse();
    assert!(
        result.is_err(),
        "Incomplete enum should produce a parse error, not crash"
    );
}

#[test]
fn test_incomplete_enum_with_name_does_not_crash() {
    let mut parser = Parser::from_source("enum Foo");
    let result = parser.parse();
    assert!(
        result.is_err(),
        "Enum without body should produce a parse error, not crash"
    );
}

#[test]
fn test_incomplete_enum_with_open_brace_does_not_crash() {
    let mut parser = Parser::from_source("enum Foo {");
    let result = parser.parse();
    assert!(
        result.is_err(),
        "Enum with unclosed brace should produce a parse error, not crash"
    );
}

#[test]
fn test_incomplete_enum_variant_does_not_crash() {
    let mut parser = Parser::from_source("enum Foo { Bar(");
    let result = parser.parse();
    assert!(
        result.is_err(),
        "Enum with unclosed variant should produce a parse error, not crash"
    );
}
