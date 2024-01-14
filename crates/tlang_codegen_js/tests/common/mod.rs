use tlang_ast::symbols::SymbolType;
use tlang_codegen_js::generator::CodegenJS;
use tlang_parser::parser::Parser;
use tlang_semantics::SemanticAnalyzer;

pub fn compile_src(source: &str, builtin_symbols: &[(&str, SymbolType)]) -> String {
    let mut parser = Parser::from_source(source);
    let mut ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => panic!("{:#?}", errors),
    };

    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(builtin_symbols);
    match semantic_analyzer.analyze(&mut ast) {
        Ok(_) => {
            let mut codegen = CodegenJS::default();
            codegen.generate_code(&ast);
            codegen.get_output().to_string()
        }
        Err(diagnostics) => panic!("{:#?}", diagnostics),
    }
}

#[macro_export]
macro_rules! compile {
    ($source:expr) => {{
        use tlang_ast::symbols::SymbolType;

        $crate::common::compile_src(
            $source,
            &[
                ("log", SymbolType::Function),
                ("max", SymbolType::Function),
                ("min", SymbolType::Function),
            ],
        )
    }};
    ($source:expr, $builtin_symbols:expr) => {{
        $crate::common::compile_src($source, $builtin_symbols)
    }};
}
