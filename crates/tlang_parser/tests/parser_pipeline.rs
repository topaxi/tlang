use indoc::indoc;

mod common;

#[test]
fn test_pipeline_operator_to_identifier() {
    assert_parses!("1 |> foo;");
}

#[test]
fn test_pipeline_operator() {
    assert_parses!("1 |> foo();");
    assert_parser_snapshot!("1 |> foo |> bar();");
}

#[test]
fn test_pipeline_operator_precedence() {
    assert_parser_snapshot!("1 + 2 |> foo();");
}

#[test]
fn test_pipeline_operator_to_function_call_with_arguments() {
    assert_parser_snapshot!("1 |> foo(2);");
}

#[test]
fn test_pipeline_operator_to_function_call_with_wildcards() {
    assert_parser_snapshot!("1 |> foo(2, _);");
}

#[test]
fn test_pipeline_operator_long_chaining() {
    assert_parser_snapshot!(indoc! {"
        [1,2,3]
        |> map(fn (x) { x ** 2 })
        |> filter(fn (x) { x % 2 == 0 })
        |> foldl(fn (acc, x) { acc + x }, 0)
        |> log();
    "});
}
