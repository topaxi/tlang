use indoc::indoc;

mod common;

#[test]
fn test_function_declaration() {
    assert_parser_snapshot!("fn foo() { bar(); 1 + 2; }");
}

#[test]
fn test_function_declaration_with_parameters() {
    assert_parses!("fn foo(x, y) { 1 + 2; }");
}

#[test]
fn test_nameless_function_expressions() {
    assert_parses!("let x = fn() { 1 + 2; };");
    assert_parses!("let x = fn(x, y) { 1 + 2; };");
}

#[test]
fn test_function_expression_without_name_no_argument_parenthesis() {
    assert_parses!("let x = fn { 1 + 2; };");
}

#[test]
fn test_function_expressions() {
    assert_parses!("let x = fn foo() { 1 + 2; };");
    assert_parses!("let x = fn foo(x, y) { 1 + 2; };");
}

#[test]
fn test_explicit_return_statements() {
    assert_parses!("fn foo() { return 1 + 2; }");
    assert_parses!("fn foo() { return; }");
    assert_parses!("let x = fn() { return 1 + 2; };");
}

#[test]
fn test_implicit_return_expressions() {
    assert_parses!("fn foo() { 1 + 2 }");
    assert_parses!("let x = fn() { 1 + 2 };");
    assert_parser_snapshot!("fn foo() { let x = 1; x }");
}

#[test]
fn test_recursive_factorial_functional_definition() {
    assert_parser_snapshot!(indoc! {"
        fn factorial(0) { return 1; }
        fn factorial(n) { return n * factorial(n - 1); }
    "});
}

#[test]
fn test_recursive_sum() {
    assert_parser_snapshot!(indoc! {"
        fn sum([]) { 0 }
        fn sum([x, ...xs]) { x + sum(xs) }
    "});
}

#[test]
fn test_functional_function_declaration_with_comments_inbetween() {
    assert_parser_snapshot!(indoc! {"
        fn foo(1) { 1 }
        // comment
        fn foo(n) { n * 2 }
    "});
}

#[test]
fn test_explicit_tail_recursive_call() {
    assert_parser_snapshot!(indoc! {"
        fn factorial(n) { factorial(n, 1) }
        fn factorial(0, acc) { acc }
        fn factorial(n, acc) { rec factorial(n - 1, n * acc) }
    "});
}

#[test]
fn test_foldl_impl() {
    assert_parser_snapshot!(indoc! {"
        fn foldl([], acc, _) { acc }
        fn foldl([x, ...xs], acc, f) { rec foldl(xs, f(acc, x), f) }
    "});
}

#[test]
fn test_function_declarations_with_guard() {
    assert_parser_snapshot!(indoc! {"
        fn filter([], f) { [] }
        fn filter([x, ...xs], f) if f(x) { [x, ...filter(xs, f)] }
        fn filter([x, ...xs], f) { rec filter(xs, f) }
    "});
}

#[test]
fn test_function_declarations_with_let_guard() {
    assert_parser_snapshot!(indoc! {"
        fn filter_map([], f) { [] }
        fn filter_map([x, ...xs], f) if let y = f(x) { [y, ...filter_map(xs, f)] }
        fn filter_map([x, ...xs], f) { rec filter_map(xs, f) }
    "});
}

#[test]
#[should_panic]
fn test_parameter_list_mandatory_comma() {
    parse!("fn foo(x y) {}");
}

#[test]
fn test_list_matching_wildcard() {
    assert_parser_snapshot!("fn tail([_, ...xs]) { xs }");
}

#[test]
fn test_fn_expression_in_function_completion_position() {
    assert_parser_snapshot!("fn foo() { fn bar() {} }");
}
