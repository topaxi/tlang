#[macro_export]
macro_rules! parse {
    ($source:expr) => {{
        use tlang_parser::parser::Parser;

        let mut parser = Parser::from_source($source);
        parser.parse().unwrap()
    }};
}
