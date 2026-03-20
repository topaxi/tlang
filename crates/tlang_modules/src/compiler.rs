use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::Path;

use tlang_ast_lowering::lower_to_hir;
use tlang_defs::DefKind;
use tlang_hir as hir;
use tlang_semantics::SemanticAnalyzer;

use crate::module_graph::{ModuleGraphError, ParsedModule};
use crate::resolver::{ModuleResolver, ResolvedImports};
use crate::{ModuleGraph, ModulePath};

/// Result of compiling a multi-module project.
pub struct MultiModuleCompileResult {
    /// Compiled HIR modules, keyed by module path, in topological order.
    pub modules: BTreeMap<ModulePath, CompiledModule>,
    /// The resolved imports for each module.
    pub imports: HashMap<ModulePath, ResolvedImports>,
}

pub struct CompiledModule {
    pub path: ModulePath,
    pub hir: hir::Module,
    pub constant_pool_ids: HashSet<hir::HirId>,
    pub lower_meta: hir::LowerResultMeta,
}

/// Errors from multi-module compilation.
#[derive(Debug)]
pub enum CompileError {
    ModuleGraphErrors(Vec<ModuleGraphError>),
    ImportErrors(Vec<ModuleGraphError>),
    SemanticError {
        module_path: ModulePath,
        errors: Vec<String>,
    },
}

impl std::fmt::Display for CompileError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CompileError::ModuleGraphErrors(errors) => {
                for e in errors {
                    writeln!(f, "{e}")?;
                }
                Ok(())
            }
            CompileError::ImportErrors(errors) => {
                for e in errors {
                    writeln!(f, "{e}")?;
                }
                Ok(())
            }
            CompileError::SemanticError {
                module_path,
                errors,
            } => {
                for e in errors {
                    writeln!(f, "error in module `{module_path}`: {e}")?;
                }
                Ok(())
            }
        }
    }
}

/// Compile a multi-module project.
///
/// `root_dir` should be the project root containing `src/lib.tlang`.
/// `builtin_symbols` should be the standard library symbols for the target backend.
pub fn compile_project(
    root_dir: &Path,
    builtin_symbols: &[(&str, DefKind)],
) -> Result<MultiModuleCompileResult, CompileError> {
    // Phase 1: Build module graph (scan, parse, validate)
    let mut graph = ModuleGraph::build(root_dir).map_err(CompileError::ModuleGraphErrors)?;

    // Phase 2: Resolve imports
    let (imports, import_errors) = ModuleResolver::resolve_imports(&graph);
    if !import_errors.is_empty() {
        return Err(CompileError::ImportErrors(import_errors));
    }

    // Phase 3: Compile each module with cross-module symbol awareness
    let mut compiled_modules = BTreeMap::new();

    // Collect exported symbols from all modules first (declaration pass)
    let exported_symbols = collect_exported_symbols(&graph);

    // Compile modules in order (root first, then children)
    for (module_path, parsed_module) in &mut graph.modules {
        let compiled =
            compile_single_module(parsed_module, &imports, &exported_symbols, builtin_symbols)
                .map_err(|errors| CompileError::SemanticError {
                    module_path: module_path.clone(),
                    errors,
                })?;

        compiled_modules.insert(module_path.clone(), compiled);
    }

    Ok(MultiModuleCompileResult {
        modules: compiled_modules,
        imports,
    })
}

/// Collect all public symbols exported from each module.
fn collect_exported_symbols(graph: &ModuleGraph) -> HashMap<ModulePath, Vec<(String, DefKind)>> {
    use tlang_ast::node::StmtKind;

    let mut exports = HashMap::new();

    for (path, module) in &graph.modules {
        let mut symbols = Vec::new();

        for stmt in &module.ast.statements {
            match &stmt.kind {
                StmtKind::FunctionDeclaration(decl)
                    if decl.visibility == tlang_ast::node::Visibility::Public =>
                {
                    let arity = decl.parameters.len() as u16;
                    symbols.push((decl.name(), DefKind::Function(arity)));
                }
                StmtKind::FunctionDeclarations(decls) => {
                    for decl in decls {
                        if decl.visibility == tlang_ast::node::Visibility::Public {
                            let arity = decl.parameters.len() as u16;
                            symbols.push((decl.name(), DefKind::Function(arity)));
                        }
                    }
                }
                StmtKind::EnumDeclaration(decl)
                    if decl.visibility == tlang_ast::node::Visibility::Public =>
                {
                    symbols.push((decl.name.as_str().to_string(), DefKind::Enum));
                    for variant in &decl.variants {
                        let qualified =
                            format!("{}::{}", decl.name.as_str(), variant.name.as_str());
                        symbols.push((
                            qualified,
                            DefKind::EnumVariant(variant.parameters.len() as u16),
                        ));
                    }
                }
                StmtKind::StructDeclaration(decl)
                    if decl.visibility == tlang_ast::node::Visibility::Public =>
                {
                    symbols.push((decl.name.as_str().to_string(), DefKind::Struct));
                }
                StmtKind::ProtocolDeclaration(decl)
                    if decl.visibility == tlang_ast::node::Visibility::Public =>
                {
                    symbols.push((decl.name.as_str().to_string(), DefKind::Protocol));
                }
                _ => {}
            }
        }

        exports.insert(path.clone(), symbols);
    }

    exports
}

/// Compile a single module with cross-module symbol awareness.
fn compile_single_module(
    parsed_module: &mut ParsedModule,
    imports: &HashMap<ModulePath, ResolvedImports>,
    exported_symbols: &HashMap<ModulePath, Vec<(String, DefKind)>>,
    builtin_symbols: &[(&str, DefKind)],
) -> Result<CompiledModule, Vec<String>> {
    // Create semantic analyzer with builtins
    let mut analyzer = SemanticAnalyzer::default();
    analyzer.add_builtin_symbols(builtin_symbols);

    // Register imported symbols from other modules
    if let Some(resolved) = imports.get(&parsed_module.path) {
        let mut import_symbols: Vec<(String, DefKind)> = Vec::new();

        for (local_name, resolved_sym) in &resolved.symbols {
            // Find the DefKind of the imported symbol
            if let Some(exports) = exported_symbols.get(&resolved_sym.source_module) {
                for (name, kind) in exports {
                    if name == &resolved_sym.original_name {
                        import_symbols.push((local_name.clone(), *kind));
                        break;
                    }
                }
            }
        }

        if !import_symbols.is_empty() {
            let symbol_refs: Vec<(&str, DefKind)> = import_symbols
                .iter()
                .map(|(name, kind)| (name.as_str(), *kind))
                .collect();
            analyzer.add_builtin_symbols(&symbol_refs);
        }
    }

    // Run semantic analysis
    analyzer
        .analyze(&mut parsed_module.ast)
        .map_err(|diags| diags.iter().map(|d| d.to_string()).collect::<Vec<_>>())?;

    // Lower to HIR
    let (module, meta) = lower_to_hir(
        &parsed_module.ast,
        &parsed_module.parse_meta.constant_pool_node_ids,
        analyzer.symbol_id_allocator(),
        analyzer.root_symbol_table(),
        analyzer.symbol_tables().clone(),
    );

    Ok(CompiledModule {
        path: parsed_module.path.clone(),
        hir: module,
        constant_pool_ids: meta.constant_pool_ids.clone(),
        lower_meta: meta,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn create_test_project(dir: &Path) {
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        fs::write(
            src.join("lib.tlang"),
            "pub mod math;\nuse math::add;\nadd(1, 2) |> log();",
        )
        .unwrap();
        fs::write(src.join("math.tlang"), "pub fn add(a, b) { a + b }").unwrap();
    }

    #[test]
    fn test_compile_project() {
        let dir = std::env::temp_dir().join("tlang_test_compile_project");
        let _ = fs::remove_dir_all(&dir);
        create_test_project(&dir);

        // Use empty builtins for this test — we only care about module resolution
        let builtins: Vec<(&str, DefKind)> = vec![("log", DefKind::Function(u16::MAX))];

        let result = compile_project(&dir, &builtins).unwrap();

        assert_eq!(result.modules.len(), 2);
        assert!(result.modules.contains_key(&ModulePath::root()));
        assert!(
            result
                .modules
                .contains_key(&ModulePath::from_str_segments(&["math"]))
        );

        let _ = fs::remove_dir_all(&dir);
    }
}
