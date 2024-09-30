use tlang_ast as ast;
use tlang_hir::hir;
use tlang_hir::visit::Visitor;

fn parse_from_str(input: &str) -> ast::node::Module {
    let mut parser = tlang_parser::Parser::from_source(input);
    parser.parse().unwrap()
}

#[allow(dead_code)]
pub fn hir_from_str(input: &str) -> hir::Module {
    let ast = parse_from_str(input);
    tlang_ast_lowering::lower_to_hir(&ast)
}

struct PathCollector {
    paths: Vec<hir::Path>,
}

impl PathCollector {
    fn new() -> Self {
        Self { paths: Vec::new() }
    }

    fn collect(&mut self, module: &hir::Module) {
        self.visit_module(module);
    }
}

impl<'hir> Visitor<'hir> for PathCollector {
    fn visit_path(&mut self, path: &'hir hir::Path) {
        self.paths.push(path.clone());
    }
}

#[allow(dead_code)]
pub fn collect_paths(node: &hir::Module) -> Vec<hir::Path> {
    let mut collector = PathCollector::new();
    collector.collect(node);
    collector.paths
}

#[allow(dead_code)]
pub fn pretty_print(node: &hir::Module) -> String {
    tlang_hir_pretty::HirPretty::pretty_print(node)
}
