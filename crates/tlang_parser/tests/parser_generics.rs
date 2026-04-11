#[macro_use]
mod common;

#[test]
fn test_generic_function_declaration() {
    assert_parser_snapshot!(indoc::indoc! {"
        fn identity<T>(x: T) -> T { x }
    "});
}

#[test]
fn test_generic_function_multiple_type_params() {
    assert_parser_snapshot!(indoc::indoc! {"
        fn pair<T, U>(a: T, b: U) -> T {
            a
        }
    "});
}

#[test]
fn test_generic_struct_declaration() {
    assert_parser_snapshot!(indoc::indoc! {"
        struct Pair<T, U> {
            first: T,
            second: U,
        }
    "});
}

#[test]
fn test_generic_enum_declaration() {
    assert_parser_snapshot!(indoc::indoc! {"
        enum Result<T, E> {
            Ok(T),
            Err(E),
        }
    "});
}

#[test]
fn test_generic_protocol_declaration() {
    assert_parser_snapshot!(indoc::indoc! {"
        protocol Into<T> {
            fn into(self) -> T
        }
    "});
}
