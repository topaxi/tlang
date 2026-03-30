use tlang_codegen_js::generator::CodegenJS;
use tlang_codegen_js::{JsAnfTransform, JsHirOptimizer};
use tlang_defs::DefKind;
use tlang_hir_opt::HirPass;
use tlang_hir_opt::symbol_resolution::SymbolResolution;
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
    #[allow(unused)]
    pub builtin_symbols: Vec<(&'a str, DefKind)>,
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

impl<'a> From<Vec<(&'a str, DefKind)>> for CodegenOptions<'a> {
    fn from(builtin_symbols: Vec<(&'a str, DefKind)>) -> Self {
        Self {
            builtin_symbols,
            ..Default::default()
        }
    }
}

#[allow(unused)]
pub fn compile_src(source: &str, options: &CodegenOptions) -> String {
    compile_src_with_warnings(source, options).0
}

pub fn compile_src_with_warnings(
    source: &str,
    options: &CodegenOptions,
) -> (String, Vec<tlang_codegen_js::CodegenWarning>) {
    let mut parser = Parser::from_source(source);
    let (mut ast, parse_meta) = match parser.parse() {
        Ok(result) => result,
        Err(errors) => panic!("{errors:#?}"),
    };

    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(&options.builtin_symbols);
    match semantic_analyzer.analyze(&mut ast) {
        Ok(()) => {
            let (mut module, meta) = tlang_ast_lowering::lower_to_hir(
                &ast,
                &parse_meta.constant_pool_node_ids,
                semantic_analyzer.symbol_id_allocator(),
                semantic_analyzer.root_symbol_table(),
                semantic_analyzer.symbol_tables().clone(),
            )
            .expect("lowering should succeed");

            if options.optimize {
                let mut optimizer = JsHirOptimizer::default();
                let mut ctx = meta.into();
                optimizer
                    .optimize_hir(&mut module, &mut ctx)
                    .expect("HIR optimization failed");
            } else {
                // Even without full optimization, SymbolResolution must run to
                // populate path.res with HirId references — codegen relies on
                // HirId-keyed name resolution.
                let mut ctx = meta.into();
                let mut sym_res = SymbolResolution::default();
                sym_res.init_context(&mut ctx);
                sym_res
                    .optimize_hir(&mut module, &mut ctx)
                    .expect("symbol resolution failed");
                let mut anf = JsAnfTransform::default();
                anf.optimize_hir(&mut module, &mut ctx)
                    .expect("internal compiler error: ANF transform failed to converge");
            }

            let mut codegen = CodegenJS::default();
            codegen
                .generate_code(&module)
                .expect("codegen should succeed");
            let warnings = codegen.get_warnings().to_vec();
            (codegen.get_output().to_string(), warnings)
        }
        Err(diagnostics) => panic!("{diagnostics:#?}"),
    }
}

#[macro_export]
macro_rules! compile {
    ($source:expr) => {{ $crate::common::compile_src($source, &Default::default()) }};
    ($source:expr, $options:expr) => {{ $crate::common::compile_src($source, &$options.into()) }};
}

#[macro_export]
macro_rules! compile_with_warnings {
    ($source:expr) => {{ $crate::common::compile_src_with_warnings($source, &Default::default()) }};
    ($source:expr, $options:expr) => {{ $crate::common::compile_src_with_warnings($source, &$options.into()) }};
}
