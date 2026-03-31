use std::collections::{BTreeMap, HashMap, HashSet};
use std::path::{Path, PathBuf};

use tlang_ast_lowering::lower_to_hir_with_offset;
use tlang_defs::DefKind;
use tlang_diagnostics::Diagnostic;
use tlang_hir as hir;
use tlang_semantics::SemanticAnalyzer;

use crate::module_graph::{ModuleGraphError, ParsedModule};
use crate::resolver::{ModuleResolver, ResolvedImports};
use crate::{ModuleGraph, ModulePath};

/// Result of compiling a multi-module project.
#[derive(Debug)]
pub struct MultiModuleCompileResult {
    /// Compiled HIR modules, keyed by module path (ordered lexicographically by key).
    /// For topological evaluation order, see [`MultiModuleCompiler::compile_project`] which
    /// populates modules in dependency order.
    pub modules: BTreeMap<ModulePath, CompiledModule>,
    /// The resolved imports for each module.
    pub imports: HashMap<ModulePath, ResolvedImports>,
    /// Exported symbols for each module (name, DefKind).
    pub exported_symbols: HashMap<ModulePath, Vec<(String, DefKind)>>,
    /// Global slot assignments for cross-module imported symbols.
    /// Maps (module_path, local_name) to global slot index.
    /// Only populated when compiled with `compile_project_with_slots`.
    pub import_slots: HashMap<(ModulePath, String), usize>,
}

impl MultiModuleCompileResult {
    /// Collect all protocol names exported across all modules.
    pub fn protocol_names(&self) -> Vec<String> {
        self.exported_symbols
            .values()
            .flat_map(|syms| {
                syms.iter().filter_map(|(name, kind)| {
                    if matches!(kind, DefKind::Protocol) {
                        Some(name.clone())
                    } else {
                        None
                    }
                })
            })
            .collect()
    }
}

#[derive(Debug)]
pub struct CompiledModule {
    pub path: ModulePath,
    pub hir: hir::Module,
    pub constant_pool_ids: HashSet<hir::HirId>,
    pub lower_meta: hir::LowerResultMeta,
}

/// Source information for a module, used for rendering source-aware diagnostics.
pub type ModuleSourceInfo = BTreeMap<ModulePath, (PathBuf, String)>;

/// Errors from multi-module compilation.
#[derive(Debug)]
pub enum CompileError {
    ModuleGraphErrors(Vec<ModuleGraphError>),
    ImportErrors {
        errors: Vec<ModuleGraphError>,
        sources: ModuleSourceInfo,
    },
    SemanticError {
        module_path: ModulePath,
        file_path: PathBuf,
        source: String,
        diagnostics: Vec<Diagnostic>,
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
            CompileError::ImportErrors { errors, .. } => {
                for e in errors {
                    writeln!(f, "{e}")?;
                }
                Ok(())
            }
            CompileError::SemanticError {
                module_path,
                diagnostics,
                ..
            } => {
                for d in diagnostics {
                    writeln!(f, "error in module `{module_path}`: {}", d.message())?;
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
        let sources = graph.source_info();
        return Err(CompileError::ImportErrors {
            errors: import_errors,
            sources,
        });
    }

    // Phase 3: Compile each module with cross-module symbol awareness
    let mut compiled_modules = BTreeMap::new();

    // Collect exported symbols from all modules first (declaration pass)
    let exported_symbols = collect_exported_symbols(&graph);

    // Track running HirId offset to avoid collisions between modules
    let mut hir_id_offset: usize = 1;

    // Compile modules in order (root first, then children)
    for (module_path, parsed_module) in &mut graph.modules {
        let file_path = parsed_module.file_path.clone();
        let source = parsed_module.source.clone();
        let compiled = compile_single_module(
            parsed_module,
            &imports,
            &exported_symbols,
            builtin_symbols,
            hir_id_offset,
        )
        .map_err(|diagnostics| CompileError::SemanticError {
            module_path: module_path.clone(),
            file_path,
            source,
            diagnostics,
        })?;

        // Advance offset past all HirIds allocated by this module
        hir_id_offset = compiled.lower_meta.hir_id_allocator.peek_next();

        compiled_modules.insert(module_path.clone(), compiled);
    }

    Ok(MultiModuleCompileResult {
        modules: compiled_modules,
        imports,
        exported_symbols,
        import_slots: HashMap::new(),
    })
}

/// Compile a multi-module project with global slot assignments.
///
/// This variant is used by the interpreter backend, which needs global slot
/// indices for symbol resolution. Cross-module imported symbols are assigned
/// global slots starting after the last builtin slot.
pub fn compile_project_with_slots<S: AsRef<str>>(
    root_dir: &Path,
    builtin_symbols: &[(S, DefKind, Option<usize>)],
) -> Result<MultiModuleCompileResult, CompileError> {
    // Phase 1: Build module graph
    let mut graph = ModuleGraph::build(root_dir).map_err(CompileError::ModuleGraphErrors)?;

    // Phase 2: Resolve imports
    let (imports, import_errors) = ModuleResolver::resolve_imports(&graph);
    if !import_errors.is_empty() {
        let sources = graph.source_info();
        return Err(CompileError::ImportErrors {
            errors: import_errors,
            sources,
        });
    }

    // Phase 3: Assign global slots for cross-module imports
    let max_builtin_slot = builtin_symbols
        .iter()
        .filter_map(|(_, _, slot)| *slot)
        .max()
        .map(|m| m + 1)
        .unwrap_or(0);

    let exported_symbols = collect_exported_symbols(&graph);
    let mut next_slot = max_builtin_slot;
    let mut import_slots: HashMap<(ModulePath, String), usize> = HashMap::new();

    for (module_path, resolved) in &imports {
        for local_name in resolved.symbols.keys() {
            import_slots.insert((module_path.clone(), local_name.clone()), next_slot);
            next_slot += 1;
        }
    }

    // Phase 4: Compile each module with slot-aware symbol registration
    let mut compiled_modules = BTreeMap::new();

    // Track running HirId offset to avoid collisions between modules
    let mut hir_id_offset: usize = 1;

    for (module_path, parsed_module) in &mut graph.modules {
        let file_path = parsed_module.file_path.clone();
        let source = parsed_module.source.clone();
        let compiled = compile_single_module_with_slots(
            parsed_module,
            &imports,
            &exported_symbols,
            builtin_symbols,
            &import_slots,
            hir_id_offset,
        )
        .map_err(|diagnostics| CompileError::SemanticError {
            module_path: module_path.clone(),
            file_path,
            source,
            diagnostics,
        })?;

        // Advance offset past all HirIds allocated by this module
        hir_id_offset = compiled.lower_meta.hir_id_allocator.peek_next();

        compiled_modules.insert(module_path.clone(), compiled);
    }

    Ok(MultiModuleCompileResult {
        modules: compiled_modules,
        imports,
        exported_symbols,
        import_slots,
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
                    if let Some(name) = decl.name() {
                        let arity = decl.parameters.len() as u16;
                        symbols.push((name, DefKind::Function(arity)));
                    }
                }
                StmtKind::FunctionDeclarations(decls) => {
                    for decl in decls {
                        if decl.visibility == tlang_ast::node::Visibility::Public
                            && let Some(name) = decl.name()
                        {
                            let arity = decl.parameters.len() as u16;
                            symbols.push((name, DefKind::Function(arity)));
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
                    for const_item in &decl.consts {
                        if const_item.visibility == tlang_ast::node::Visibility::Public {
                            let qualified =
                                format!("{}::{}", decl.name.as_str(), const_item.name.as_str());
                            symbols.push((qualified, DefKind::Const));
                        }
                    }
                }
                StmtKind::StructDeclaration(decl)
                    if decl.visibility == tlang_ast::node::Visibility::Public =>
                {
                    symbols.push((decl.name.as_str().to_string(), DefKind::Struct));
                    for const_item in &decl.consts {
                        if const_item.visibility == tlang_ast::node::Visibility::Public {
                            let qualified =
                                format!("{}::{}", decl.name.as_str(), const_item.name.as_str());
                            symbols.push((qualified, DefKind::Const));
                        }
                    }
                }
                StmtKind::ProtocolDeclaration(decl)
                    if decl.visibility == tlang_ast::node::Visibility::Public =>
                {
                    symbols.push((decl.name.as_str().to_string(), DefKind::Protocol));
                    for method in &decl.methods {
                        let qualified = format!("{}::{}", decl.name.as_str(), method.name.as_str());
                        symbols.push((
                            qualified,
                            DefKind::ProtocolMethod(method.parameters.len() as u16),
                        ));
                    }
                    for const_item in &decl.consts {
                        if const_item.visibility == tlang_ast::node::Visibility::Public {
                            let qualified =
                                format!("{}::{}", decl.name.as_str(), const_item.name.as_str());
                            symbols.push((qualified, DefKind::Const));
                        }
                    }
                }
                StmtKind::Const(decl) if decl.visibility == tlang_ast::node::Visibility::Public => {
                    symbols.push((decl.name.as_str().to_string(), DefKind::Const));
                }
                _ => {}
            }
        }

        exports.insert(path.clone(), symbols);
    }

    exports
}

/// Collect import symbols for a module, including auto-imported children
/// (enum variants when an enum is imported, protocol methods when a protocol is imported).
fn collect_import_symbols(
    resolved: &ResolvedImports,
    exported_symbols: &HashMap<ModulePath, Vec<(String, DefKind)>>,
) -> Vec<(String, DefKind)> {
    let mut import_symbols: Vec<(String, DefKind)> = Vec::new();

    for (local_name, resolved_sym) in &resolved.symbols {
        if let Some(exports) = exported_symbols.get(&resolved_sym.source_module) {
            for (name, kind) in exports {
                if name == &resolved_sym.original_name {
                    import_symbols.push((local_name.clone(), *kind));

                    // Auto-import child symbols for enums (variants + consts),
                    // structs (consts), and protocols (methods + consts)
                    if matches!(kind, DefKind::Enum | DefKind::Protocol | DefKind::Struct) {
                        let prefix = format!("{}::", resolved_sym.original_name);
                        for (child_name, child_kind) in exports {
                            if child_name.starts_with(&prefix) {
                                // Replace the original parent name with the local alias
                                let local_child =
                                    format!("{}::{}", local_name, &child_name[prefix.len()..]);
                                import_symbols.push((local_child, *child_kind));
                            }
                        }
                    }

                    break;
                }
            }
        }
    }

    import_symbols
}

/// Compile a single module with cross-module symbol awareness.
fn compile_single_module(
    parsed_module: &mut ParsedModule,
    imports: &HashMap<ModulePath, ResolvedImports>,
    exported_symbols: &HashMap<ModulePath, Vec<(String, DefKind)>>,
    builtin_symbols: &[(&str, DefKind)],
    hir_id_start: usize,
) -> Result<CompiledModule, Vec<Diagnostic>> {
    // Create semantic analyzer with builtins
    let mut analyzer = SemanticAnalyzer::default();
    analyzer.add_builtin_symbols(builtin_symbols);

    // Register imported symbols from other modules
    if let Some(resolved) = imports.get(&parsed_module.path) {
        let import_symbols = collect_import_symbols(resolved, exported_symbols);

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
        .map_err(|diags| diags.to_vec())?;

    // Lower to HIR with module-specific HirId offset to avoid collisions
    let (module, meta) = lower_to_hir_with_offset(
        &parsed_module.ast,
        &parsed_module.parse_meta.constant_pool_node_ids,
        analyzer.symbol_id_allocator(),
        analyzer.root_symbol_table(),
        analyzer.symbol_tables().clone(),
        hir_id_start,
    )
    .map_err(|errs| {
        errs.iter()
            .map(|e| Diagnostic::error(&e.to_string(), e.span()))
            .collect::<Vec<_>>()
    })?;

    Ok(CompiledModule {
        path: parsed_module.path.clone(),
        hir: module,
        constant_pool_ids: meta.constant_pool_ids.clone(),
        lower_meta: meta,
    })
}

/// Compile a single module with global slot assignments for builtins and imports.
fn compile_single_module_with_slots<S: AsRef<str>>(
    parsed_module: &mut ParsedModule,
    imports: &HashMap<ModulePath, ResolvedImports>,
    exported_symbols: &HashMap<ModulePath, Vec<(String, DefKind)>>,
    builtin_symbols: &[(S, DefKind, Option<usize>)],
    import_slots: &HashMap<(ModulePath, String), usize>,
    hir_id_start: usize,
) -> Result<CompiledModule, Vec<Diagnostic>> {
    let mut analyzer = SemanticAnalyzer::default();
    analyzer.add_builtin_symbols_with_slots(builtin_symbols);

    // Register imported symbols with their assigned global slots
    if let Some(resolved) = imports.get(&parsed_module.path) {
        let base_symbols = collect_import_symbols(resolved, exported_symbols);
        let mut import_symbols: Vec<(String, DefKind, Option<usize>)> = Vec::new();

        for (local_name, kind) in base_symbols {
            let slot = import_slots
                .get(&(parsed_module.path.clone(), local_name.clone()))
                .copied();
            import_symbols.push((local_name, kind, slot));
        }

        if !import_symbols.is_empty() {
            analyzer.add_builtin_symbols_with_slots(&import_symbols);
        }
    }

    analyzer
        .analyze(&mut parsed_module.ast)
        .map_err(|diags| diags.to_vec())?;

    // Lower to HIR with module-specific HirId offset to avoid collisions
    let (module, meta) = lower_to_hir_with_offset(
        &parsed_module.ast,
        &parsed_module.parse_meta.constant_pool_node_ids,
        analyzer.symbol_id_allocator(),
        analyzer.root_symbol_table(),
        analyzer.symbol_tables().clone(),
        hir_id_start,
    )
    .map_err(|errs| {
        errs.iter()
            .map(|e| Diagnostic::error(&e.to_string(), e.span()))
            .collect::<Vec<_>>()
    })?;

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

    fn create_multi_module_project(dir: &Path) {
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        fs::write(
            src.join("lib.tlang"),
            "pub mod math;\npub mod utils;\nuse utils::double_add;\ndouble_add(3, 4) |> log();",
        )
        .unwrap();
        fs::write(src.join("math.tlang"), "pub fn add(a, b) { a + b }").unwrap();
        fs::write(
            src.join("utils.tlang"),
            "use math::add;\npub fn double_add(a, b) { add(a, b) + add(a, b) }",
        )
        .unwrap();
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

    #[test]
    fn test_compile_project_with_slots() {
        let dir = std::env::temp_dir().join("tlang_test_compile_with_slots");
        let _ = fs::remove_dir_all(&dir);
        create_test_project(&dir);

        let builtins: Vec<(String, DefKind, Option<usize>)> =
            vec![("log".to_string(), DefKind::Function(u16::MAX), Some(0))];

        let result = compile_project_with_slots(&dir, &builtins).unwrap();

        assert_eq!(result.modules.len(), 2);
        // Import slots should be assigned for cross-module imports
        assert!(!result.import_slots.is_empty());
        // The root module imports `add` from `math`
        assert!(
            result
                .import_slots
                .contains_key(&(ModulePath::root(), "add".to_string()))
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_hir_ids_dont_collide_across_modules() {
        let dir = std::env::temp_dir().join("tlang_test_hir_id_no_collision");
        let _ = fs::remove_dir_all(&dir);
        create_multi_module_project(&dir);

        let builtins: Vec<(&str, DefKind)> = vec![("log", DefKind::Function(u16::MAX))];
        let result = compile_project(&dir, &builtins).unwrap();

        // Collect all HirIds from all modules' function declarations
        let mut all_fn_hir_ids = std::collections::HashSet::new();
        for compiled in result.modules.values() {
            for stmt in &compiled.hir.block.stmts {
                if let tlang_hir::StmtKind::FunctionDeclaration(decl) = &stmt.kind {
                    let inserted = all_fn_hir_ids.insert(decl.hir_id);
                    assert!(
                        inserted,
                        "HirId {:?} collision for function `{}`",
                        decl.hir_id,
                        decl.name()
                    );
                }
            }
        }

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_compile_project_with_slots_cross_module() {
        let dir = std::env::temp_dir().join("tlang_test_slots_cross_module");
        let _ = fs::remove_dir_all(&dir);
        create_multi_module_project(&dir);

        let builtins: Vec<(String, DefKind, Option<usize>)> =
            vec![("log".to_string(), DefKind::Function(u16::MAX), Some(0))];

        let result = compile_project_with_slots(&dir, &builtins).unwrap();

        assert_eq!(result.modules.len(), 3);

        // utils imports `add` from math
        assert!(
            result
                .import_slots
                .contains_key(&(ModulePath::from_str_segments(&["utils"]), "add".to_string()))
        );
        // root imports `double_add` from utils
        assert!(
            result
                .import_slots
                .contains_key(&(ModulePath::root(), "double_add".to_string()))
        );

        // All import slots should have distinct indices
        let slot_values: std::collections::HashSet<_> =
            result.import_slots.values().copied().collect();
        assert_eq!(slot_values.len(), result.import_slots.len());

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_compile_project_missing_pub_mod() {
        let dir = std::env::temp_dir().join("tlang_test_missing_pub_mod");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        // lib.tlang declares `pub mod math` but math.tlang doesn't exist
        fs::write(src.join("lib.tlang"), "pub mod missing;").unwrap();

        let builtins: Vec<(&str, DefKind)> = vec![];
        let err = compile_project(&dir, &builtins).unwrap_err();

        let msg = err.to_string();
        assert!(
            msg.contains("missing"),
            "Error should mention missing module: {msg}"
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_compile_project_private_symbol() {
        let dir = std::env::temp_dir().join("tlang_test_private_symbol");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        fs::write(src.join("lib.tlang"), "pub mod math;\nuse math::secret;").unwrap();
        // `secret` is not pub
        fs::write(src.join("math.tlang"), "fn secret() { 42 }").unwrap();

        let builtins: Vec<(&str, DefKind)> = vec![];
        let err = compile_project(&dir, &builtins).unwrap_err();

        let msg = err.to_string();
        assert!(
            msg.contains("not declared as `pub`"),
            "Error should mention private symbol: {msg}"
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_compile_project_name_collision() {
        let dir = std::env::temp_dir().join("tlang_test_name_collision");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        fs::write(
            src.join("lib.tlang"),
            "pub mod math;\npub mod utils;\nuse math::add;\nuse utils::add;",
        )
        .unwrap();
        fs::write(src.join("math.tlang"), "pub fn add(a, b) { a + b }").unwrap();
        fs::write(src.join("utils.tlang"), "pub fn add(a, b) { a + b + 1 }").unwrap();

        let builtins: Vec<(&str, DefKind)> = vec![];
        let err = compile_project(&dir, &builtins).unwrap_err();

        let msg = err.to_string();
        assert!(
            msg.contains("already imported"),
            "Error should mention name collision: {msg}"
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_compile_error_display_semantic() {
        let err = CompileError::SemanticError {
            module_path: ModulePath::from_str_segments(&["math"]),
            file_path: PathBuf::from("src/math.tlang"),
            source: String::new(),
            diagnostics: vec![],
        };
        // Empty diagnostics should produce empty output
        assert_eq!(err.to_string(), "");
    }

    #[test]
    fn test_compile_error_display_module_graph() {
        let err = CompileError::ModuleGraphErrors(vec![ModuleGraphError::CycleError {
            cycle: vec![
                ModulePath::from_str_segments(&["a"]),
                ModulePath::from_str_segments(&["b"]),
            ],
            reason: "contains top-level expressions".to_string(),
        }]);
        let msg = err.to_string();
        assert!(msg.contains("module cycle detected"));
        assert!(msg.contains("top-level expressions"));
    }

    #[test]
    fn test_compile_error_display_import() {
        let err = CompileError::ImportErrors {
            errors: vec![ModuleGraphError::ImportError {
                module_path: ModulePath::root(),
                symbol_name: "foo".to_string(),
                reason: "module `bar` not found".to_string(),
                span: tlang_span::Span::default(),
            }],
            sources: BTreeMap::new(),
        };
        let msg = err.to_string();
        assert!(msg.contains("cannot import `foo`"));
    }

    #[test]
    fn test_compile_project_with_struct_export() {
        let dir = std::env::temp_dir().join("tlang_test_struct_export");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        fs::write(
            src.join("lib.tlang"),
            "pub mod shapes;\nuse shapes::Point;\nlet p = Point { x: 1, y: 2 };\np.x |> log();",
        )
        .unwrap();
        fs::write(
            src.join("shapes.tlang"),
            "pub struct Point {\n    x: int,\n    y: int,\n}",
        )
        .unwrap();

        let builtins: Vec<(&str, DefKind)> = vec![("log", DefKind::Function(u16::MAX))];
        let result = compile_project(&dir, &builtins).unwrap();

        assert_eq!(result.modules.len(), 2);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_compile_project_with_protocol_export() {
        let dir = std::env::temp_dir().join("tlang_test_protocol_export");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        // Importing the protocol auto-imports its methods too (see collect_import_symbols)
        fs::write(
            src.join("lib.tlang"),
            "pub mod protos;\nuse protos::Printable;",
        )
        .unwrap();
        fs::write(
            src.join("protos.tlang"),
            "pub protocol Printable {\n  fn print(self)\n}",
        )
        .unwrap();

        let builtins: Vec<(&str, DefKind)> = vec![("log", DefKind::Function(u16::MAX))];
        let result = compile_project(&dir, &builtins).unwrap();

        assert_eq!(result.modules.len(), 2);

        let _ = fs::remove_dir_all(&dir);
    }
}
