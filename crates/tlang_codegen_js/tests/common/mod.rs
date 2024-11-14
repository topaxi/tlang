use tlang_ast::symbols::SymbolType;
use tlang_codegen_js::generator::CodegenJS;
use tlang_parser::parser::Parser;
use tlang_semantics::SemanticAnalyzer;

#[ctor::ctor]
fn before_all() {
    env_logger::init();
}

pub fn compile_src(source: &str, builtin_symbols: &[(&str, SymbolType)]) -> String {
    let mut parser = Parser::from_source(source);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => panic!("{errors:#?}"),
    };

    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(builtin_symbols);
    match semantic_analyzer.analyze(&ast) {
        Ok(()) => {
            let mut codegen = CodegenJS::default();
            let hir = tlang_ast_lowering::lower_to_hir(&ast);
            codegen.generate_code(&hir);
            codegen.get_output().to_string()
        }
        Err(diagnostics) => panic!("{diagnostics:#?}"),
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
                ("math", SymbolType::Module),
                ("math::max", SymbolType::Function),
                ("math::min", SymbolType::Function),
                ("math::sqrt", SymbolType::Function),
                ("Option", SymbolType::Enum),
                ("Option::Some", SymbolType::EnumVariant),
                ("Option::None", SymbolType::EnumVariant),
                ("Some", SymbolType::EnumVariant),
                ("None", SymbolType::EnumVariant),
            ],
        )
    }};
    ($source:expr, $builtin_symbols:expr) => {{
        $crate::common::compile_src($source, $builtin_symbols)
    }};
}
