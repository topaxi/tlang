use indoc::indoc;

mod common;

#[test]
fn test_pub_function_declaration() {
    assert_parser_snapshot!("pub fn foo() { 1 }");
}

#[test]
fn test_pub_multi_clause_function_declaration() {
    assert_parser_snapshot!(indoc! {"
        pub fn factorial(0) { 1 }
        pub fn factorial(n) { n * factorial(n - 1) }
    "});
}

#[test]
fn test_pub_enum_declaration() {
    assert_parser_snapshot!(indoc! {"
        pub enum Option {
            Some(value),
            None,
        }
    "});
}

#[test]
fn test_pub_struct_declaration() {
    assert_parser_snapshot!("pub struct Point { x: Int, y: Int }");
}

#[test]
fn test_pub_protocol_declaration() {
    assert_parser_snapshot!(indoc! {"
        pub protocol Functor {
            fn map(self, f)
        }
    "});
}

#[test]
fn test_private_function_declaration_default() {
    // Without pub, visibility should be Private
    assert_parser_snapshot!("fn bar() { 2 }");
}
