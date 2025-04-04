use tlang_ast_lowering::lower_to_hir;
use tlang_hir::hir;
use tlang_hir::hir::Module;
use tlang_hir_opt::hir_opt::HirOptGroup;
use tlang_hir_opt::{
    HirPass, constant_folding::ConstantFolder, constant_propagation::ConstantPropagator,
    hir_opt::HirOptimizer,
};
use tlang_hir_pretty::HirPrettyOptions;
use tlang_parser::Parser;

pub fn compile(source: &str) -> Module {
    let ast = Parser::from_source(source).parse().unwrap();
    lower_to_hir(&ast)
}

pub fn optimize(module: &mut hir::Module, passes: Vec<Box<dyn HirPass>>) {
    let mut optimizer = HirOptimizer::new(HirOptGroup::new(passes));
    optimizer.optimize_module(module);
}

pub fn compile_and_optimize(source: &str) -> hir::Module {
    compile_with_passes(
        source,
        vec![
            Box::new(ConstantFolder::default()),
            Box::new(ConstantPropagator::default()),
        ],
    )
}

pub fn compile_with_passes(source: &str, passes: Vec<Box<dyn HirPass>>) -> hir::Module {
    let mut module = compile(source);
    optimize(&mut module, passes);
    module
}

pub fn pretty_print(module: &hir::Module) -> String {
    let options = HirPrettyOptions {
        mark_unresolved: false,
        ..Default::default()
    };
    let mut prettier = tlang_hir_pretty::HirPretty::new(options);
    prettier.print_module(module);
    prettier.output().to_string()
}
