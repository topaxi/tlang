use tlang_ast_lowering::lower_to_hir;
use tlang_hir::hir;
use tlang_hir_opt::{
    constant_folding::ConstantFolder, constant_propagation::ConstantPropagator, hir_opt::HirOptimizer, DeadCodeEliminator, HirPass
};
use tlang_hir_pretty::HirPrettyOptions;
use tlang_parser::Parser;

pub fn compile_to_hir(source: &str) -> hir::Module {
    let ast = Parser::from_source(source).parse().unwrap();
    lower_to_hir(&ast)
}

pub fn optimize(module: &mut hir::Module, passes: Vec<Box<dyn HirPass>>) {
    let mut optimizer = HirOptimizer::new();
    for pass in passes {
        optimizer.add_pass(pass);
    }
    optimizer.optimize_module(module);
}

pub fn compile_and_optimize(source: &str) -> hir::Module {
    compile_and_optimize_with_passes(source, vec![
        Box::new(ConstantFolder::default()),
        Box::new(ConstantPropagator::default()),
        Box::new(DeadCodeEliminator::default()),
    ])
}

pub fn compile_and_optimize_with_passes(source: &str, passes: Vec<Box<dyn HirPass>>) -> hir::Module {
    let mut module = compile_to_hir(source);
    optimize(&mut module, passes);
    module
}

pub fn pretty_print(module: &hir::Module) -> String {
    let options = HirPrettyOptions {
        mark_unresolved: false,
        ..Default::default()
    };
    let mut prettier = tlang_hir_pretty::HirPretty::new(options);
    let mut output = String::from("fn main() -> unknown {\n");
    prettier.print_module(module);
    let inner = prettier.output().trim_end();
    if !inner.is_empty() {
        output.push_str("    ");
        output.push_str(&inner.replace("\n", "\n    "));
        output.push('\n');
    }
    output.push_str("};\n");
    output
}
