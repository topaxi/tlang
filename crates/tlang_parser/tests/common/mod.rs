#[macro_export]
macro_rules! parse {
    ($source:expr) => {{
        use tlang_parser::parser::Parser;

        let mut parser = Parser::from_source($source);
        parser.parse().expect("failed to parse")
    }};
}

#[macro_export]
macro_rules! assert_parses {
    ($source:expr) => {{
        use tlang_parser::parser::Parser;

        let mut parser = Parser::from_source($source);
        let _ = parser.parse().expect("failed to parse");
    }};
}

#[macro_export]
macro_rules! assert_parser_snapshot {
    ($ast:expr) => {{
        use insta::assert_ron_snapshot;
        assert_ron_snapshot!(parse!($ast));
    }};
}
