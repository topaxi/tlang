use indoc::indoc;
use pretty_assertions::assert_eq;
use tlang_ast::symbols::SymbolType;

mod common;

#[test]
fn test_pipeline_operator() {
    let output = compile!("fn main() { 1 |> log; }");
    let expected_output = indoc! {"
        function main() {
            console.log(1);
        }
    "};
    assert_eq!(output, expected_output);

    let output = compile!("fn main() { 1 |> math::min |> math::max; }");
    let expected_output = indoc! {"
        function main() {
            Math.max(Math.min(1));
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_pipeline_operator_with_call_parenthesis() {
    let output = compile!("fn main() { 1 |> math::max(); }");
    let expected_output = indoc! {"
        function main() {
            Math.max(1);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_pipeline_operator_with_call_parenthesis_and_arguments() {
    let output = compile!("fn main() { 1 |> log(2); }");
    let expected_output = indoc! {"
        function main() {
            console.log(1, 2);
        }
    "};
    assert_eq!(output, expected_output);

    let output = compile!("fn main() { 1 |> log(2, 3); }");
    let expected_output = indoc! {"
        function main() {
            console.log(1, 2, 3);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_pipeline_operator_to_function_call_with_wildcards() {
    let output = compile!("fn main() { 1 |> log(2, _); }");
    let expected_output = indoc! {"
        function main() {
            console.log(2, 1);
        }
    "};
    assert_eq!(output, expected_output);
}

#[test]
fn test_pipeline_operator_long_chaining() {
    let output = compile!(
        indoc! {"
        [1,2,3]
        |> map(fn (x) { x ** 2 })
        |> filter(fn (x) { x % 2 == 0 })
        |> foldl(fn (acc, x) { acc + x }, 0)
        |> log();
    "},
        &[
            ("map", SymbolType::Function),
            ("filter", SymbolType::Function),
            ("foldl", SymbolType::Function),
            ("log", SymbolType::Function),
        ]
    );
    let expected_output = indoc! {"
        console.log(foldl(filter(map([1, 2, 3], (x) => x ** 2), (x) => x % 2 === 0), (acc, x) => acc + x, 0));
    "};
    assert_eq!(output, expected_output);
}
