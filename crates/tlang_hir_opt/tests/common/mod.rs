#![allow(dead_code)]

use std::collections::HashMap;

use tlang_ast_lowering::lower_to_hir;
use tlang_hir::visit::{walk_pat, walk_stmt};
use tlang_hir::{self as hir, Visitor};
use tlang_hir_opt::HirOptimizer;
use tlang_hir_pretty::HirPrettyOptions;
use tlang_parser::Parser;
use tlang_semantics::SemanticAnalyzer;
use tlang_span::HirId;
use tlang_symbols::SymbolType;

#[ctor::ctor]
fn before_all() {
    let _ = env_logger::builder()
        .filter_level(log::LevelFilter::Warn)
        .parse_default_env()
        .is_test(true)
        .try_init();
}

pub fn compile(source: &str) -> hir::LowerResult {
    let (mut ast, parse_meta) = Parser::from_source(source).parse().unwrap();
    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(&[("println", SymbolType::Function(u16::MAX))]);
    semantic_analyzer.analyze(&mut ast).unwrap();
    lower_to_hir(
        &ast,
        &parse_meta.constant_pool_node_ids,
        semantic_analyzer.symbol_id_allocator(),
        semantic_analyzer.root_symbol_table(),
        semantic_analyzer.symbol_tables().clone(),
    )
}

/// Like [`compile`] but registers a representative set of interpreter-style
/// builtin symbols with explicit global slots.  Use this when the test source
/// references builtins such as `log`, `Option::Some`/`None`, or the
/// `Iterable`/`Iterator` protocols so that [`IdentifierResolver`] can assign
/// `Slot::Global` instead of leaving paths unresolved.
pub fn compile_with_interpreter_builtins(source: &str) -> hir::LowerResult {
    // Slot indices mirror the order the interpreter assigns them (native fns
    // sorted by name first, then const symbols).  We only list the subset
    // needed by the tests in this file; the exact numbers don't matter as long
    // as they are unique and non-None.
    let builtins: &[(&str, SymbolType, Option<usize>)] = &[
        // module symbols – no slot
        ("math", SymbolType::Module, None),
        // native functions (alphabetical, matching interpreter slot order)
        ("filter", SymbolType::Function(2), Some(0)),
        ("foldl", SymbolType::Function(3), Some(1)),
        ("log", SymbolType::Function(u16::MAX), Some(2)),
        ("map", SymbolType::Function(2), Some(3)),
        // enum/protocol type symbols – no slot
        ("Option", SymbolType::Enum, None),
        ("Result", SymbolType::Enum, None),
        ("Functor", SymbolType::Protocol, None),
        ("Functor::map", SymbolType::ProtocolMethod(2), None),
        ("Iterable", SymbolType::Protocol, None),
        ("Iterable::iter", SymbolType::ProtocolMethod(1), None),
        ("Iterator", SymbolType::Protocol, None),
        ("Iterator::next", SymbolType::ProtocolMethod(1), None),
        // const value symbols – need slots
        ("Option::Some", SymbolType::EnumVariant(1), Some(4)),
        ("Option::None", SymbolType::EnumVariant(0), Some(5)),
        ("Some", SymbolType::EnumVariant(1), Some(6)),
        ("None", SymbolType::EnumVariant(0), Some(7)),
        ("Result::Ok", SymbolType::EnumVariant(1), Some(8)),
        ("Result::Err", SymbolType::EnumVariant(1), Some(9)),
        ("Ok", SymbolType::EnumVariant(1), Some(10)),
        ("Err", SymbolType::EnumVariant(1), Some(11)),
    ];

    let (mut ast, parse_meta) = Parser::from_source(source).parse().unwrap();
    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols_with_slots(builtins);
    semantic_analyzer.analyze(&mut ast).unwrap();
    lower_to_hir(
        &ast,
        &parse_meta.constant_pool_node_ids,
        semantic_analyzer.symbol_id_allocator(),
        semantic_analyzer.root_symbol_table(),
        semantic_analyzer.symbol_tables().clone(),
    )
}

pub fn compile_with_interpreter_builtins_and_optimize(
    source: &str,
    optimizer: &mut HirOptimizer,
) -> hir::Module {
    let (mut module, meta) = compile_with_interpreter_builtins(source);
    let mut ctx = meta.into();
    optimizer.optimize_hir(&mut module, &mut ctx);
    module
}

pub fn pretty_print_with_unresolved_markers(module: &hir::Module) -> String {
    let options = HirPrettyOptions {
        mark_unresolved: true,
        ..Default::default()
    };
    let mut prettier = tlang_hir_pretty::HirPretty::new(options);
    prettier.print_module(module);
    prettier.output().to_string()
}

pub fn compile_and_optimize(source: &str, optimizer: &mut HirOptimizer) -> hir::Module {
    let (mut module, meta) = compile(source);
    let mut ctx = meta.into();
    optimizer.optimize_hir(&mut module, &mut ctx);
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
