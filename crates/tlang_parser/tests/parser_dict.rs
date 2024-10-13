mod common;

#[test]
fn test_dictionary_literal() {
    assert_parser_snapshot!(r"let x = { foo: 1, bar: 2 };");
}

#[test]
fn test_dictionary_literal_trailing_comma() {
    assert_parser_snapshot!(r"let x = { foo: 1, bar: 2, };");
}

#[test]
fn test_dictionary_literal_shorthand() {
    assert_parser_snapshot!(r"let x = { foo, bar };");
}

#[test]
fn test_dictionary_literal_shorthand_trailing_comma() {
    assert_parser_snapshot!(r"let x = { foo, bar, };");
}

#[test]
fn test_function_call_with_dictionary_no_parens() {
    // TODO: Alternatively use ruby like syntax for calling functions with dictionaries
    //       and omit the braces instead of the parens.
    //       As this syntax here might be more desirable for creating dictionaries or structs.
    assert_parser_snapshot!(r"foo { foo: 1, bar: 2 };");
}

#[test]
#[ignore] // TODO: Decide if we want to use this instead of the above syntax.
fn test_fn_call_expression_with_dictionary_argument() {
    // Similar to ruby syntax, we might want to allow omitting curly braces
    // for the last argument if it is a dictionary (or struct).
    // Creation of the struct/dictionary could/should be infered from the function signature.
    assert_parser_snapshot!("bar(x: 1, y: 2)");
}
