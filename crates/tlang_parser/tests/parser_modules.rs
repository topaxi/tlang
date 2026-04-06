use indoc::indoc;

mod common;

// === use declarations ===

#[test]
fn test_use_single_import() {
    assert_parser_snapshot!("use string::parse::from_char_code;");
}

#[test]
fn test_use_single_import_with_alias() {
    assert_parser_snapshot!("use string::parse::from_char_code as fcc;");
}

#[test]
fn test_use_grouped_imports() {
    assert_parser_snapshot!("use string::{from_char_code, char_code_at};");
}

#[test]
fn test_use_grouped_imports_with_alias() {
    assert_parser_snapshot!("use string::{from_char_code as fcc, char_code_at};");
}

#[test]
fn test_use_short_path() {
    assert_parser_snapshot!("use math::pi;");
}

#[test]
fn test_use_deep_path() {
    assert_parser_snapshot!("use a::b::c::d::e;");
}

#[test]
fn test_use_grouped_deep_path() {
    assert_parser_snapshot!("use a::b::c::{d, e};");
}

// === mod declarations ===

#[test]
fn test_pub_mod_single() {
    assert_parser_snapshot!("pub mod parse;");
}

#[test]
fn test_pub_mod_multiple() {
    assert_parser_snapshot!("pub mod parse, utils;");
}

#[test]
fn test_private_mod_single() {
    assert_parser_snapshot!("mod internal;");
}

#[test]
fn test_private_mod_multiple() {
    assert_parser_snapshot!("mod internal, helpers;");
}

// === combined ===

#[test]
fn test_mod_and_use_combined() {
    assert_parser_snapshot!(indoc! {"
        pub mod string;
        use string::from_char_code;

        pub fn main() { from_char_code(65) }
    "});
}

// === pub use (re-exports) ===

#[test]
fn test_pub_use_single_import() {
    assert_parser_snapshot!("pub use math::add;");
}

#[test]
fn test_pub_use_single_import_with_alias() {
    assert_parser_snapshot!("pub use math::add as sum;");
}

#[test]
fn test_pub_use_grouped_imports() {
    assert_parser_snapshot!("pub use math::{add, subtract};");
}

#[test]
fn test_pub_use_deep_path() {
    assert_parser_snapshot!("pub use a::b::c::add;");
}

#[test]
fn test_pub_use_combined_with_mod() {
    assert_parser_snapshot!(indoc! {"
        pub mod math;
        pub use math::add;
    "});
}
