use tlang_ast::symbols::SymbolType;
use tlang_codegen_js::generator::CodegenJS;
use tlang_parser::Parser;
use tlang_semantics::SemanticAnalyzer;

#[ctor::ctor]
fn before_all() {
    env_logger::init();
}

pub struct CodegenOptions<'a> {
    pub render_ternary: bool,
    pub builtin_symbols: Vec<(&'a str, SymbolType)>,
}

impl Default for CodegenOptions<'_> {
    fn default() -> Self {
        Self {
            render_ternary: true,
            builtin_symbols: CodegenJS::get_standard_library_symbols().to_vec(),
        }
    }
}

impl CodegenOptions<'_> {
    #[allow(dead_code)]
    pub fn set_render_ternary(mut self, render_ternary: bool) -> Self {
        self.render_ternary = render_ternary;
        self
    }
}

impl<'a> From<Vec<(&'a str, SymbolType)>> for CodegenOptions<'a> {
    fn from(builtin_symbols: Vec<(&'a str, SymbolType)>) -> Self {
        Self {
            builtin_symbols,
            ..Default::default()
        }
    }
}

pub fn compile_src(source: &str, options: &CodegenOptions) -> String {
    let mut parser = Parser::from_source(source);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(errors) => panic!("{errors:#?}"),
    };

    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(&options.builtin_symbols);
    match semantic_analyzer.analyze(&ast) {
        Ok(()) => {
            let mut codegen = CodegenJS::default();
            codegen.set_render_ternary(options.render_ternary);
            let hir = tlang_ast_lowering::lower_to_hir(
                &ast,
                semantic_analyzer.symbol_id_allocator(),
                semantic_analyzer.symbol_tables().clone(),
            );
            codegen.generate_code(&hir.module);
            codegen.get_output().to_string()
        }
        Err(diagnostics) => panic!("{diagnostics:#?}"),
    }
}

#[macro_export]
macro_rules! compile {
    ($source:expr) => {{ $crate::common::compile_src($source, &Default::default()) }};
    ($source:expr, $options:expr) => {{ $crate::common::compile_src($source, &$options.into()) }};
}
