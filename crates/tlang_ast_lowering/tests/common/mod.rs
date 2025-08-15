use tlang_ast as ast;
use tlang_hir::hir;

fn parse_str(input: &str) -> ast::node::Module {
    let mut parser = tlang_parser::Parser::from_source(input);
    parser.parse().unwrap()
}

pub fn hir_from_str(input: &str) -> hir::Module {
    let ast = parse_str(input);
    let (module, _) =
        tlang_ast_lowering::lower_to_hir(&ast, Default::default(), Default::default());
    module
}

pub fn pretty_print(node: &hir::Module) -> String {
    let mut printer = tlang_hir_pretty::HirPretty::new(tlang_hir_pretty::HirPrettyOptions {
        // We don't resolve in ast lowering, but in a optimization phase.
        mark_unresolved: false,
        ..Default::default()
    });

    printer.print_module(node);
    printer.output().to_string()
}
