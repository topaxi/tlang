use tlang_ast as ast;
use tlang_hir::{Visitor, hir};

fn parse_str(input: &str) -> ast::node::Module {
    let mut parser = tlang_parser::Parser::from_source(input);
    parser.parse().unwrap()
}

#[allow(dead_code)]
pub fn hir_from_str(input: &str) -> hir::Module {
    let ast = parse_str(input);
    let (module, _) =
        tlang_ast_lowering::lower_to_hir(&ast, Default::default(), Default::default());
    module
}

struct PathCollector {
    paths: Vec<hir::Path>,
}

impl PathCollector {
    fn new() -> Self {
        Self { paths: Vec::new() }
    }

    fn collect(&mut self, module: &mut hir::Module) {
        self.visit_module(module, &mut ());
    }
}

impl<'hir> Visitor<'hir> for PathCollector {
    fn visit_path(&mut self, path: &'hir mut hir::Path, _ctx: &mut Self::Context) {
        self.paths.push(path.clone());
    }
}

#[allow(dead_code)]
pub fn collect_paths(node: &mut hir::Module) -> Vec<hir::Path> {
    let mut collector = PathCollector::new();
    collector.collect(node);
    collector.paths
}

#[allow(dead_code)]
pub fn pretty_print(node: &hir::Module) -> String {
    let mut printer = tlang_hir_pretty::HirPretty::new(tlang_hir_pretty::HirPrettyOptions {
        // We don't resolve in ast lowering, but in a optimization phase.
        mark_unresolved: false,
        ..Default::default()
    });

    printer.print_module(node);
    printer.output().to_string()
}
