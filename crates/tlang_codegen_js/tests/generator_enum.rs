use indoc::indoc;
use pretty_assertions::assert_eq;

mod common;

#[test]
fn test_enums() {
    let output = compile!(indoc! {"
        enum Option {
            Some(unknown),
            None,
        }

        fn main() {
            let x = Option::Some(42);
        }
    "});
    let expected_output = indoc! {"
        class Option {
            tag = this;
            [0];
            static Some = (arg) => Object.assign(new this(), {
                tag: this.Some,
                0: arg
            });
            static None = new this();
        }
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
        class Node {
            tag = this;
            operator;
            left;
            right;
            static BinaryOperation = ({ operator, left, right }) => Object.assign(new this(), {
                tag: this.BinaryOperation,
                operator,
                left,
                right
            });
        }
        function main() {
            let x = Node.BinaryOperation({
                operator: \"+\",
                left: 1,
                right: 2
            });
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_enums_tree_implementation() {
    let output = compile!(indoc! {"
        enum Tree {
            Leaf(unknown),
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
        class Tree {
            tag = this;
            [0];
            left;
            right;
            static Leaf = (arg) => Object.assign(new this(), {
                tag: this.Leaf,
                0: arg
            });
            static Node = ({ left, right }) => Object.assign(new this(), {
                tag: this.Node,
                left,
                right
            });
        }
        function main() {
            let x = Tree.Node({
                left: Tree.Leaf(1),
                right: Tree.Leaf(2)
            });
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_maximum_depth_tree() {
    let output = compile!(indoc! {"
        enum Tree {
            Leaf(unknown),
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
        class Tree {
            tag = this;
            [0];
            left;
            right;
            static Leaf = (arg) => Object.assign(new this(), {
                tag: this.Leaf,
                0: arg
            });
            static Node = ({ left, right }) => Object.assign(new this(), {
                tag: this.Node,
                left,
                right
            });
        }
        function maximum_depth(tree) {
            let left, right;
            if (tree.tag === Tree.Leaf) {
                return 1;
            } else if (tree.tag === Tree.Node && (left = tree.left, true) && (right = tree.right, true)) {
                return 1 + Math.max(maximum_depth(left), maximum_depth(right));
            }
        }
        function main() {
            let x = Tree.Node({
                left: Tree.Leaf(1),
                right: Tree.Node({
                    left: Tree.Leaf(2),
                    right: Tree.Leaf(3)
                })
            });
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_maximum_depth_tree_positional() {
    let output = compile!(indoc! {"
        enum Tree {
            Leaf(unknown),
            Node(unknown, unknown),
        }

        fn maximum_depth(Tree::Leaf(_)) { 1 }
        fn maximum_depth(Tree::Node(left, right)) { 1 + math::max(maximum_depth(left), maximum_depth(right)) }
    "});
    let expected_output = indoc! {"
        class Tree {
            tag = this;
            [0];
            [1];
            static Leaf = (arg) => Object.assign(new this(), {
                tag: this.Leaf,
                0: arg
            });
            static Node = (arg, arg$0) => Object.assign(new this(), {
                tag: this.Node,
                0: arg,
                1: arg$0
            });
        }
        function maximum_depth(tree) {
            let left, right;
            if (tree.tag === Tree.Leaf) {
                return 1;
            } else if (tree.tag === Tree.Node && (left = tree[0], true) && (right = tree[1], true)) {
                return 1 + Math.max(maximum_depth(left), maximum_depth(right));
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_enum_is_variant_boolean_return_simplification() {
    let output = compile!(indoc! {"
        enum Color { Red, Blue, Green }
        fn Color.is_red(Color::Red) { true }
        fn Color.is_red(_) { false }
    "});
    let expected_output = indoc! {"
        class Color {
            tag = this;
            static Red = new this();
            static Blue = new this();
            static Green = new this();
        }
        Color.prototype.is_red = function is_red() {
            return this.tag === Color.Red;
        };
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_enum_is_variant_negated_boolean_return_simplification() {
    let output = compile!(indoc! {"
        enum Color { Red, Blue, Green }
        fn Color.is_not_red(Color::Red) { false }
        fn Color.is_not_red(_) { true }
    "});
    let expected_output = indoc! {"
        class Color {
            tag = this;
            static Red = new this();
            static Blue = new this();
            static Green = new this();
        }
        Color.prototype.is_not_red = function is_not_red() {
            return !(this.tag === Color.Red);
        };
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_enum_is_variant_with_positional_fields() {
    let output = compile!(indoc! {"
        enum Shape { Circle(unknown), Rectangle(unknown, unknown) }
        fn Shape.is_circle(Shape::Circle(_)) { true }
        fn Shape.is_circle(_) { false }
    "});
    let expected_output = indoc! {"
        class Shape {
            tag = this;
            [0];
            [1];
            static Circle = (arg) => Object.assign(new this(), {
                tag: this.Circle,
                0: arg
            });
            static Rectangle = (arg, arg$0) => Object.assign(new this(), {
                tag: this.Rectangle,
                0: arg,
                1: arg$0
            });
        }
        Shape.prototype.is_circle = function is_circle() {
            return this.tag === Shape.Circle;
        };
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_enum_accessor_match_collapses_to_direct_index_access() {
    let output = compile!(indoc! {"
        enum Expense {
            Food(i64, String),
            Transport(i64, String),
            Entertainment(i64, String),
        }

        fn Expense.amount(Expense::Food(amount, _)) { amount }
        fn Expense.amount(Expense::Transport(amount, _)) { amount }
        fn Expense.amount(Expense::Entertainment(amount, _)) { amount }

        fn Expense.date(Expense::Food(_, date)) { date }
        fn Expense.date(Expense::Transport(_, date)) { date }
        fn Expense.date(Expense::Entertainment(_, date)) { date }
    "});
    let expected_output = indoc! {"
        class Expense {
            tag = this;
            [0];
            [1];
            static Food = (arg, arg$0) => Object.assign(new this(), {
                tag: this.Food,
                0: arg,
                1: arg$0
            });
            static Transport = (arg, arg$0) => Object.assign(new this(), {
                tag: this.Transport,
                0: arg,
                1: arg$0
            });
            static Entertainment = (arg, arg$0) => Object.assign(new this(), {
                tag: this.Entertainment,
                0: arg,
                1: arg$0
            });
        }
        Expense.prototype.amount = function amount() {
            return this[0];
        };
        Expense.prototype.date = function date() {
            return this[1];
        };
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_discriminant_enum() {
    let output = compile!(indoc! {"
        enum HttpStatus {
            Ok = 200,
            Created = 201,
            NotFound = 404,
        }
    "});
    let expected_output = indoc! {"
        const HttpStatus = {
            Ok: 200,
            Created: 201,
            NotFound: 404
        };
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_discriminant_enum_pattern_matching() {
    let output = compile!(indoc! {"
        enum HttpStatus {
            Ok = 200,
            NotFound = 404,
        }

        fn describe(HttpStatus::Ok) { \"OK\" }
        fn describe(HttpStatus::NotFound) { \"Not Found\" }
    "});
    let expected_output = indoc! {"
        const HttpStatus = {
            Ok: 200,
            NotFound: 404
        };
        function describe(httpstatus) {
            if (httpstatus === HttpStatus.Ok) {
                return \"OK\";
            } else if (httpstatus === HttpStatus.NotFound) {
                return \"Not Found\";
            }
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_discriminant_enum_no_warning_single_enum() {
    let (_output, warnings) = compile_with_warnings!(indoc! {"
        enum HttpStatus {
            Ok = 200,
            NotFound = 404,
        }

        fn describe(status) {
            match status {
                HttpStatus::Ok => \"OK\",
                HttpStatus::NotFound => \"Not Found\",
                _ => \"Unknown\",
            }
        }
    "});
    assert!(
        warnings.is_empty(),
        "Expected no warnings for single discriminant enum match, got: {warnings:?}"
    );
}

#[test]
fn test_discriminant_enum_no_warning_disjoint_values() {
    // Two enums with disjoint values should NOT produce a warning.
    let (_output, warnings) = compile_with_warnings!(indoc! {"
        enum A {
            a = 1,
            b = 2,
        }

        enum B {
            c = 10,
            d = 20,
        }

        fn describe(x) {
            match x {
                A::a => \"A::a\",
                A::b => \"A::b\",
                B::c => \"B::c\",
                B::d => \"B::d\",
                _ => \"Unknown\",
            }
        }
    "});
    assert!(
        warnings.is_empty(),
        "Expected no warnings for disjoint discriminant enum values, got: {warnings:?}"
    );
}

#[test]
fn test_discriminant_enum_warning_overlapping_values() {
    // Two enums with overlapping values SHOULD produce a warning.
    let (_output, warnings) = compile_with_warnings!(indoc! {"
        enum A {
            a = 1,
            b = 2,
        }

        enum B {
            c = 1,
            d = 3,
        }

        fn describe(x) {
            match x {
                A::a => \"A::a\",
                A::b => \"A::b\",
                B::c => \"B::c\",
                B::d => \"B::d\",
                _ => \"Unknown\",
            }
        }
    "});
    assert_eq!(
        warnings.len(),
        1,
        "Expected exactly one warning for overlapping discriminant enum values"
    );
    let msg = &warnings[0].message;
    assert!(
        msg.contains("A") && msg.contains("B"),
        "Warning should mention both enum names, got: {msg}"
    );
    assert!(
        msg.contains("A::a") && msg.contains("B::c"),
        "Warning should mention overlapping variants, got: {msg}"
    );
}

#[test]
fn test_discriminant_enum_warning_single_enum_duplicate_values() {
    // A single enum with duplicate discriminant values used in a match SHOULD produce a warning.
    // Note: The semantic analysis pass will emit a warning about the duplicate definition;
    // this test verifies the codegen-level match-arm overlap check independently
    // (which operates on the HIR after semantic analysis).
    let (_output, warnings) = compile_with_warnings!(indoc! {"
        enum Status {
            Ok = 1,
            Success = 1,
        }

        fn describe(x) {
            match x {
                Status::Ok => \"Ok\",
                Status::Success => \"Success\",
                _ => \"Unknown\",
            }
        }
    "});
    // Codegen should warn about the overlap in the match expression.
    assert!(
        !warnings.is_empty(),
        "Expected a warning for duplicate discriminant values in single enum match"
    );
    let msg = &warnings[0].message;
    assert!(
        msg.contains("Status"),
        "Warning should mention the enum name, got: {msg}"
    );
    assert!(
        msg.contains("Status::Ok") && msg.contains("Status::Success"),
        "Warning should mention both variants with duplicate values, got: {msg}"
    );
}
