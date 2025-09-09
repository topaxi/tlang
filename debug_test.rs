use tlang_ast_lowering::lower_to_hir;
use tlang_codegen_js::create_hir_js_opt_group;
use tlang_hir::hir;
use tlang_hir_opt::{HirOptContext, HirPass};
use tlang_hir_pretty::HirPrettyOptions;
use tlang_parser::Parser;
use tlang_semantics::SemanticAnalyzer;

fn compile(source: &str) -> hir::LowerResult {
    let ast = Parser::from_source(source).parse().unwrap();
    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(
        tlang_codegen_js::generator::CodegenJS::get_standard_library_symbols(),
    );
    semantic_analyzer.analyze(&ast).unwrap();
    lower_to_hir(
        &ast,
        semantic_analyzer.symbol_id_allocator(),
        semantic_analyzer.root_symbol_table(),
        semantic_analyzer.symbol_tables().clone(),
    )
}

fn pretty_print(module: &hir::Module) -> String {
    let options = HirPrettyOptions {
        mark_unresolved: false,
        ..Default::default()
    };
    let mut prettier = tlang_hir_pretty::HirPretty::new(options);
    prettier.print_module(module);
    prettier.output().to_string()
}

fn main() {
    let source = r#"
        fn test() {
            for i in [1, 2, 3] {
                log(i);
            }
        }
    "#;
    
    let (mut module, meta) = compile(source);
    
    println!("=== HIR BEFORE JS PASS ===");
    println!("{}", pretty_print(&module));
    
    let mut ctx = HirOptContext::from(meta);
    let mut hir_js_opt_group = create_hir_js_opt_group();
    hir_js_opt_group.optimize_hir(&mut module, &mut ctx);
    
    println!("\n=== HIR AFTER JS PASS ===");
    println!("{}", pretty_print(&module));
}