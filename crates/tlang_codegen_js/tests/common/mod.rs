use tlang_codegen_js::create_hir_js_opt_group;
use tlang_codegen_js::generator::CodegenJS;
use tlang_hir_opt::HirOptimizer;
use tlang_parser::Parser;
use tlang_semantics::SemanticAnalyzer;
use tlang_symbols::SymbolType;

#[ctor::ctor]
fn before_all() {
    let _ = env_logger::builder()
        .filter_level(log::LevelFilter::Warn)
        .parse_default_env()
        .is_test(true)
        .try_init();
}

pub struct CodegenOptions<'a> {
    pub builtin_symbols: Vec<(&'a str, SymbolType)>,
    pub optimize: bool,
}

impl Default for CodegenOptions<'_> {
    fn default() -> Self {
        Self {
            builtin_symbols: CodegenJS::get_standard_library_symbols().to_vec(),
            optimize: true,
        }
    }
}

impl CodegenOptions<'_> {
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

            let mut optimizer = if options.optimize {
                HirOptimizer::default()
            } else {
                HirOptimizer::new(vec![])
            };

            optimizer.add_pass(Box::new(create_hir_js_opt_group()));
            optimizer.optimize_hir(&mut module, meta.into());

            let mut codegen = CodegenJS::default();
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
