#[macro_export]
macro_rules! parse {
    ($source:expr) => {{
        use tlang_parser::parser::Parser;

        let mut parser = Parser::from_source($source);
        parser.parse().unwrap()
    }};
}

#[macro_export]
macro_rules! assert_parser_snapshot {
    ($ast:expr) => {{
        use insta::assert_ron_snapshot;
        assert_ron_snapshot!($ast);
    }};
}
