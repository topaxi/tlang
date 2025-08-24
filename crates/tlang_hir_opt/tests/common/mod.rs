#![allow(dead_code)]

use std::collections::HashMap;

use tlang_symbols::SymbolType;
use tlang_ast_lowering::lower_to_hir;
use tlang_hir::visit::{walk_pat, walk_stmt};
use tlang_hir::{Visitor, hir};
use tlang_hir_opt::hir_opt::HirOptimizer;
use tlang_hir_pretty::HirPrettyOptions;
use tlang_parser::Parser;
use tlang_semantics::SemanticAnalyzer;
use tlang_span::HirId;

#[ctor::ctor]
fn before_all() {
    let _ = env_logger::builder()
        .filter_level(log::LevelFilter::Warn)
        .parse_default_env()
        .is_test(true)
        .try_init();
}

pub fn compile(source: &str) -> hir::LowerResult {
    let ast = Parser::from_source(source).parse().unwrap();
    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(&[("println", SymbolType::Function(u16::MAX))]);
    semantic_analyzer.analyze(&ast).unwrap();
    lower_to_hir(
        &ast,
        semantic_analyzer.symbol_id_allocator(),
        semantic_analyzer.root_symbol_table(),
        semantic_analyzer.symbol_tables().clone(),
    )
}

pub fn compile_and_optimize(source: &str, optimizer: &mut HirOptimizer) -> hir::Module {
    let (mut module, meta) = compile(source);
    optimizer.optimize_hir(&mut module, meta.into());
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

pub fn collect_declarations(node: &mut hir::Module) -> HashMap<HirId, String> {
    let mut collector = DeclarationCollector::default();
    collector.collect(node);
    collector.declarations
}

#[derive(Default)]
struct DeclarationCollector {
    declarations: HashMap<HirId, String>,
}

impl DeclarationCollector {
    fn collect(&mut self, module: &mut hir::Module) {
        self.visit_module(module, &mut ());
    }
}

impl<'hir> Visitor<'hir> for DeclarationCollector {
    fn visit_stmt(&mut self, stmt: &'hir mut hir::Stmt, ctx: &mut Self::Context) {
        match &stmt.kind {
            hir::StmtKind::FunctionDeclaration(decl) => {
                self.declarations.insert(decl.hir_id, decl.name());

                walk_stmt(self, stmt, ctx);
            }
            hir::StmtKind::EnumDeclaration(decl) => {
                self.declarations.insert(decl.hir_id, decl.name.to_string());

                for variant in &decl.variants {
                    self.declarations.insert(
                        variant.hir_id,
                        decl.name.to_string() + "::" + variant.name.as_str(),
                    );
                }

                walk_stmt(self, stmt, ctx);
            }
            _ => walk_stmt(self, stmt, ctx),
        }
    }
    fn visit_pat(&mut self, pat: &'hir mut hir::Pat, ctx: &mut Self::Context) {
        match &pat.kind {
            hir::PatKind::Identifier(hir_id, ident) => {
                self.declarations.insert(*hir_id, ident.to_string());
            }
            _ => walk_pat(self, pat, ctx),
        }
    }
}

pub fn collect_paths(node: &mut hir::Module) -> Vec<hir::Path> {
    let mut collector = PathCollector::default();
    collector.collect(node);
    collector.paths
}

#[derive(Default)]
struct PathCollector {
    paths: Vec<hir::Path>,
}

impl PathCollector {
    fn collect(&mut self, module: &mut hir::Module) {
        self.visit_module(module, &mut ());
    }
}

impl<'hir> Visitor<'hir> for PathCollector {
    fn visit_path(&mut self, path: &'hir mut hir::Path, _ctx: &mut Self::Context) {
        self.paths.push(path.clone());
    }
}
