use tlang_symbols::SymbolType;
use tlang_codegen_js::generator::CodegenJS;
use tlang_hir_opt::HirOptimizer;
use tlang_parser::Parser;
use tlang_semantics::SemanticAnalyzer;

#[ctor::ctor]
fn before_all() {
    let _ = env_logger::builder()
        .filter_level(log::LevelFilter::Warn)
        .parse_default_env()
        .is_test(true)
        .try_init();
}

pub struct CodegenOptions<'a> {
    pub render_ternary: bool,
    pub builtin_symbols: Vec<(&'a str, SymbolType)>,
    pub optimize: bool,
}

impl Default for CodegenOptions<'_> {
    fn default() -> Self {
        Self {
            render_ternary: true,
            builtin_symbols: CodegenJS::get_standard_library_symbols().to_vec(),
            optimize: true,
        }
    }
}

impl CodegenOptions<'_> {
    #[allow(dead_code)]
    pub fn render_ternary(mut self, render_ternary: bool) -> Self {
        self.render_ternary = render_ternary;
        self
    }

    #[allow(dead_code)]
    pub fn optimize(mut self, optimize: bool) -> Self {
        self.optimize = optimize;
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
            let (mut module, meta) = tlang_ast_lowering::lower_to_hir(
                &ast,
                semantic_analyzer.symbol_id_allocator(),
                semantic_analyzer.root_symbol_table(),
                semantic_analyzer.symbol_tables().clone(),
            );

            if options.optimize {
                let mut optimizer = HirOptimizer::default();
                optimizer.optimize_hir(&mut module, meta.into());
            }

            let mut codegen = CodegenJS::default();
            codegen.set_render_ternary(options.render_ternary);
            codegen.generate_code(&module);
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
