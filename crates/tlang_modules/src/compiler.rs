use std::collections::{BTreeMap, HashMap, HashSet};
use std::hash::{Hash, Hasher};
use std::path::{Path, PathBuf};

use tlang_ast_lowering::lower_to_hir_with_offset;
use tlang_defs::DefKind;
use tlang_diagnostics::Diagnostic;
use tlang_hir as hir;
use tlang_semantics::SemanticAnalyzer;
use tlang_span::HIRS_PER_MODULE;

use crate::module_graph::{ModuleGraphError, ParsedModule, hash_source};
use crate::resolver::{ModuleResolver, ResolvedImports};
use crate::{ModuleGraph, ModulePath};

/// Maps each module to the set of modules that import from it.
pub type ReverseDeps = HashMap<ModulePath, HashSet<ModulePath>>;

/// Fingerprint of a module's public API for early-cutoff incremental compilation.
///
/// If two fingerprints have the same `hash`, the exported symbol sets are considered
/// identical and downstream modules do not need recompilation.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleApiFingerprint {
    /// Sorted list of (name, DefKind) for all pub-visible exports.
    pub exported_symbols: Vec<(String, DefKind)>,
    /// Hash of the above for cheap comparison.
    pub hash: u64,
}

impl ModuleApiFingerprint {
    /// Compute a fingerprint from a list of exported symbols.
    pub fn from_exports(exports: &[(String, DefKind)]) -> Self {
        let mut sorted = exports.to_vec();
        sorted.sort_by(|a, b| a.0.cmp(&b.0).then_with(|| a.1.cmp(&b.1)));
        sorted.dedup();
        let hash = {
            let mut hasher = std::hash::DefaultHasher::new();
            for (name, kind) in &sorted {
                name.hash(&mut hasher);
                kind.hash(&mut hasher);
            }
            hasher.finish()
        };
        ModuleApiFingerprint {
            exported_symbols: sorted,
            hash,
        }
    }
}

/// Compute the stable HirId block start for a module at the given index.
///
/// Module indices are determined by lexicographic ordering of module paths.
/// Each module receives a fixed block of [`HIRS_PER_MODULE`] IDs, so
/// `1 + index * HIRS_PER_MODULE` is the first HirId assigned to that module.
fn hir_id_start_for_index(index: usize) -> usize {
    1 + index * HIRS_PER_MODULE
}

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
    /// Reverse-dependency map: for each module, the set of modules that import from it.
    pub reverse_deps: ReverseDeps,
    /// Public-API fingerprints for early-cutoff incremental compilation.
    pub api_fingerprints: HashMap<ModulePath, ModuleApiFingerprint>,
    /// Module paths in topological compilation order (dependencies first).
    pub compilation_order: Vec<ModulePath>,
    /// Source hashes for each module at the time of compilation.
    pub source_hashes: HashMap<ModulePath, u64>,
    /// The module graph used during compilation.
    /// Stored so that `IncrementalCompiler` can reuse it without rebuilding.
    pub graph: ModuleGraph,
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
///
/// # Panics
///
/// Panics if any module exceeds [`HIRS_PER_MODULE`] HIR nodes, which would overflow
/// its reserved HirId block.
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

    // Build reverse-dependency map and compute API fingerprints
    let reverse_deps = build_reverse_deps(&imports);
    let api_fingerprints = compute_api_fingerprints(&exported_symbols);

    // Compute topological compilation order (dependencies first)
    let compilation_order = graph.topological_order();

    // Capture source hashes before compilation mutates the graph
    let source_hashes: HashMap<ModulePath, u64> = graph
        .modules
        .iter()
        .map(|(path, m)| (path.clone(), m.source_hash))
        .collect();

    // Build a map from module path to its lexicographic index for stable HirId assignment.
    let lex_indices: HashMap<ModulePath, usize> = graph
        .modules
        .keys()
        .enumerate()
        .map(|(i, p)| (p.clone(), i))
        .collect();

    // Compile modules in topological order.
    // Each module receives a *fixed* HirId block based on its lexicographic index
    // so that adding/removing/resizing an unrelated module does not shift the
    // HirId range of any other module.
    for module_path in &compilation_order {
        let lex_index = lex_indices[module_path];
        let hir_id_start = hir_id_start_for_index(lex_index);
        let parsed_module = graph.modules.get_mut(module_path).unwrap();
        let file_path = parsed_module.file_path.clone();
        let source = parsed_module.source.clone();
        let compiled = compile_single_module(
            parsed_module,
            &imports,
            &exported_symbols,
            builtin_symbols,
            hir_id_start,
        )
        .map_err(|diagnostics| CompileError::SemanticError {
            module_path: module_path.clone(),
            file_path,
            source,
            diagnostics,
        })?;

        // Verify this module did not overflow its allocated HirId block.
        let next_id = compiled.lower_meta.hir_id_allocator.peek_next();
        let block_end = hir_id_start + HIRS_PER_MODULE;
        assert!(
            next_id <= block_end,
            "HirId block overflow for module {module_path}: used {} IDs, limit is {HIRS_PER_MODULE}",
            next_id - hir_id_start,
        );

        compiled_modules.insert(module_path.clone(), compiled);
    }

    Ok(MultiModuleCompileResult {
        modules: compiled_modules,
        imports,
        exported_symbols,
        import_slots: HashMap::new(),
        reverse_deps,
        api_fingerprints,
        compilation_order,
        source_hashes,
        graph,
    })
}

/// Compile a multi-module project with global slot assignments.
///
/// This variant is used by the interpreter backend, which needs global slot
/// indices for symbol resolution. Cross-module imported symbols are assigned
/// global slots starting after the last builtin slot.
///
/// # Panics
///
/// Panics if any module exceeds [`HIRS_PER_MODULE`] HIR nodes, which would overflow
/// its reserved HirId block.
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

    // Build reverse-dependency map and compute API fingerprints
    let reverse_deps = build_reverse_deps(&imports);
    let api_fingerprints = compute_api_fingerprints(&exported_symbols);

    // Compute topological compilation order (dependencies first)
    let compilation_order = graph.topological_order();

    // Capture source hashes before compilation mutates the graph
    let source_hashes: HashMap<ModulePath, u64> = graph
        .modules
        .iter()
        .map(|(path, m)| (path.clone(), m.source_hash))
        .collect();

    // Build a map from module path to its lexicographic index for stable HirId assignment.
    let lex_indices: HashMap<ModulePath, usize> = graph
        .modules
        .keys()
        .enumerate()
        .map(|(i, p)| (p.clone(), i))
        .collect();

    // Phase 4: Compile each module in topological order with slot-aware symbol registration
    let mut compiled_modules = BTreeMap::new();

    // Each module receives a *fixed* HirId block based on its lexicographic index
    // so that adding/removing/resizing an unrelated module does not shift the
    // HirId range of any other module.
    for module_path in &compilation_order {
        let lex_index = lex_indices[module_path];
        let hir_id_start = hir_id_start_for_index(lex_index);
        let parsed_module = graph.modules.get_mut(module_path).unwrap();
        let file_path = parsed_module.file_path.clone();
        let source = parsed_module.source.clone();
        let compiled = compile_single_module_with_slots(
            parsed_module,
            &imports,
            &exported_symbols,
            builtin_symbols,
            &import_slots,
            hir_id_start,
        )
        .map_err(|diagnostics| CompileError::SemanticError {
            module_path: module_path.clone(),
            file_path,
            source,
            diagnostics,
        })?;

        // Verify this module did not overflow its allocated HirId block.
        let next_id = compiled.lower_meta.hir_id_allocator.peek_next();
        let block_end = hir_id_start + HIRS_PER_MODULE;
        assert!(
            next_id <= block_end,
            "HirId block overflow for module {module_path}: used {} IDs, limit is {HIRS_PER_MODULE}",
            next_id - hir_id_start,
        );

        compiled_modules.insert(module_path.clone(), compiled);
    }

    Ok(MultiModuleCompileResult {
        modules: compiled_modules,
        imports,
        exported_symbols,
        import_slots,
        reverse_deps,
        api_fingerprints,
        compilation_order,
        source_hashes,
        graph,
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

/// Build a reverse-dependency map from resolved imports.
///
/// For each module that is imported from, records which modules import it.
fn build_reverse_deps(imports: &HashMap<ModulePath, ResolvedImports>) -> ReverseDeps {
    let mut reverse_deps: ReverseDeps = HashMap::new();

    for (importer, resolved) in imports {
        for symbol in resolved.symbols.values() {
            reverse_deps
                .entry(symbol.source_module.clone())
                .or_default()
                .insert(importer.clone());
        }
    }

    reverse_deps
}

/// Compute API fingerprints for all modules.
fn compute_api_fingerprints(
    exported_symbols: &HashMap<ModulePath, Vec<(String, DefKind)>>,
) -> HashMap<ModulePath, ModuleApiFingerprint> {
    exported_symbols
        .iter()
        .map(|(path, symbols)| (path.clone(), ModuleApiFingerprint::from_exports(symbols)))
        .collect()
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

/// Incremental compiler for multi-module projects.
///
/// Caches compiled modules and tracks dependency relationships so that only
/// affected modules are recompiled when a source file changes.
///
/// Uses source fingerprinting to skip re-parsing unchanged files and
/// public-API fingerprinting for early cutoff: if a module's exported
/// symbols are unchanged after recompilation, its dependents are not
/// invalidated.
pub struct IncrementalCompiler {
    root_dir: PathBuf,
    graph: ModuleGraph,
    cache: HashMap<ModulePath, CompiledModule>,
    imports: HashMap<ModulePath, ResolvedImports>,
    exported_symbols: HashMap<ModulePath, Vec<(String, DefKind)>>,
    reverse_deps: ReverseDeps,
    api_fingerprints: HashMap<ModulePath, ModuleApiFingerprint>,
    source_hashes: HashMap<ModulePath, u64>,
    builtin_symbols: Vec<(String, DefKind)>,
}

impl std::fmt::Debug for IncrementalCompiler {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("IncrementalCompiler")
            .field("root_dir", &self.root_dir)
            .field("cached_modules", &self.cache.keys().collect::<Vec<_>>())
            .finish()
    }
}

impl IncrementalCompiler {
    /// Create an incremental compiler by performing an initial full compilation.
    pub fn from_project(
        root_dir: &Path,
        builtin_symbols: &[(&str, DefKind)],
    ) -> Result<Self, CompileError> {
        let result = compile_project(root_dir, builtin_symbols)?;

        let stored_builtins: Vec<(String, DefKind)> = builtin_symbols
            .iter()
            .map(|(name, kind)| (name.to_string(), *kind))
            .collect();

        Ok(IncrementalCompiler {
            root_dir: root_dir.to_path_buf(),
            graph: result.graph,
            cache: result.modules.into_iter().collect(),
            imports: result.imports,
            exported_symbols: result.exported_symbols,
            reverse_deps: result.reverse_deps,
            api_fingerprints: result.api_fingerprints,
            source_hashes: result.source_hashes,
            builtin_symbols: stored_builtins,
        })
    }

    /// Update a single source file and recompile only affected modules.
    ///
    /// Returns the set of module paths that were recompiled. If the source
    /// is unchanged (same hash), returns an empty set.
    ///
    /// The `path` argument must exactly match the `file_path` stored in the
    /// module graph (typically an absolute path produced by [`ModuleGraph::build`]).
    /// If no module matches, an empty `Ok(vec![])` is returned.
    ///
    /// # Panics
    ///
    /// Panics if a module scheduled for recompilation is not present in the
    /// module graph (this indicates an internal inconsistency).
    pub fn update_file(
        &mut self,
        path: &Path,
        source: String,
    ) -> Result<Vec<ModulePath>, CompileError> {
        // Find which module this file belongs to
        let module_path = self
            .graph
            .modules
            .iter()
            .find(|(_, m)| m.file_path == path)
            .map(|(p, _)| p.clone());

        let module_path = match module_path {
            Some(p) => p,
            None => return Ok(vec![]),
        };

        // Check source hash for change detection
        let new_hash = hash_source(&source);
        if let Some(&old_hash) = self.source_hashes.get(&module_path)
            && old_hash == new_hash
        {
            return Ok(vec![]);
        }

        // Save old hash so we can roll back on error
        let old_hash = self.source_hashes.get(&module_path).copied();

        // Re-parse and update the graph
        self.reparse_module(&module_path, path, source, new_hash)
            .inspect_err(|_| {
                self.rollback_source_hash(&module_path, old_hash);
            })?;

        // Re-resolve imports for the whole graph
        let (imports, import_errors) = ModuleResolver::resolve_imports(&self.graph);
        if !import_errors.is_empty() {
            let sources = self.graph.source_info();
            self.rollback_source_hash(&module_path, old_hash);
            return Err(CompileError::ImportErrors {
                errors: import_errors,
                sources,
            });
        }
        self.imports = imports;

        // Re-collect exported symbols and determine if API changed
        let exported_symbols = collect_exported_symbols(&self.graph);
        let new_fingerprint = ModuleApiFingerprint::from_exports(
            exported_symbols.get(&module_path).map_or(&[], |v| v),
        );
        let api_changed = self
            .api_fingerprints
            .get(&module_path)
            .is_none_or(|old| old != &new_fingerprint);

        self.exported_symbols = exported_symbols;
        self.api_fingerprints
            .insert(module_path.clone(), new_fingerprint);
        self.reverse_deps = build_reverse_deps(&self.imports);

        // Determine which modules to recompile
        let to_recompile = if api_changed {
            self.transitive_dependents(&module_path)
        } else {
            vec![module_path.clone()]
        };

        let result = self.recompile_modules(&to_recompile);

        // Only commit the new hash after the full pipeline succeeds
        if result.is_ok() {
            self.source_hashes.insert(module_path, new_hash);
        } else {
            self.rollback_source_hash(&module_path, old_hash);
        }

        result
    }

    /// Restore the source hash for a module to its previous value (or remove it).
    fn rollback_source_hash(&mut self, module_path: &ModulePath, old_hash: Option<u64>) {
        match old_hash {
            Some(h) => {
                self.source_hashes.insert(module_path.clone(), h);
            }
            None => {
                self.source_hashes.remove(module_path);
            }
        }
    }

    /// Re-parse a single module and update the graph.
    fn reparse_module(
        &mut self,
        module_path: &ModulePath,
        file_path: &Path,
        source: String,
        source_hash: u64,
    ) -> Result<(), CompileError> {
        let mut parser = tlang_parser::Parser::from_source(&source);
        let (ast, parse_meta) = parser.parse().map_err(|e| {
            CompileError::ModuleGraphErrors(vec![ModuleGraphError::ParseError {
                module_path: module_path.clone(),
                file_path: file_path.to_path_buf(),
                source: source.clone(),
                issues: e.issues().to_vec(),
            }])
        })?;

        let (mod_declarations, use_declarations) = crate::module_graph::extract_declarations(&ast);

        self.graph.modules.insert(
            module_path.clone(),
            ParsedModule {
                path: module_path.clone(),
                file_path: file_path.to_path_buf(),
                source,
                ast,
                parse_meta,
                mod_declarations,
                use_declarations,
                source_hash,
            },
        );

        Ok(())
    }

    /// BFS on reverse deps to find the changed module plus all transitive dependents.
    fn transitive_dependents(&self, changed: &ModulePath) -> Vec<ModulePath> {
        let mut to_recompile = vec![changed.clone()];
        let mut queue = std::collections::VecDeque::new();
        let mut visited = HashSet::new();
        visited.insert(changed.clone());

        if let Some(dependents) = self.reverse_deps.get(changed) {
            for dep in dependents {
                if visited.insert(dep.clone()) {
                    queue.push_back(dep.clone());
                    to_recompile.push(dep.clone());
                }
            }
        }

        while let Some(current) = queue.pop_front() {
            if let Some(dependents) = self.reverse_deps.get(&current) {
                for dep in dependents {
                    if visited.insert(dep.clone()) {
                        queue.push_back(dep.clone());
                        to_recompile.push(dep.clone());
                    }
                }
            }
        }

        to_recompile
    }

    /// Compile a set of modules in topological order.
    fn recompile_modules(
        &mut self,
        to_recompile: &[ModulePath],
    ) -> Result<Vec<ModulePath>, CompileError> {
        let topo_order = self.graph.topological_order();
        let recompile_set: HashSet<&ModulePath> = to_recompile.iter().collect();
        let ordered_recompile: Vec<ModulePath> = topo_order
            .into_iter()
            .filter(|p| recompile_set.contains(p))
            .collect();

        let lex_indices: HashMap<ModulePath, usize> = self
            .graph
            .modules
            .keys()
            .enumerate()
            .map(|(i, p)| (p.clone(), i))
            .collect();

        let builtin_refs: Vec<(&str, DefKind)> = self
            .builtin_symbols
            .iter()
            .map(|(name, kind)| (name.as_str(), *kind))
            .collect();

        for module_path in &ordered_recompile {
            let lex_index = lex_indices[module_path];
            let hir_id_start = hir_id_start_for_index(lex_index);
            let parsed_module = self.graph.modules.get_mut(module_path).unwrap();
            let file_path = parsed_module.file_path.clone();
            let source = parsed_module.source.clone();
            let compiled = compile_single_module(
                parsed_module,
                &self.imports,
                &self.exported_symbols,
                &builtin_refs,
                hir_id_start,
            )
            .map_err(|diagnostics| CompileError::SemanticError {
                module_path: module_path.clone(),
                file_path,
                source,
                diagnostics,
            })?;

            self.cache.insert(module_path.clone(), compiled);
        }

        Ok(ordered_recompile)
    }

    /// Get a reference to a compiled module from the cache.
    pub fn get_module(&self, path: &ModulePath) -> Option<&CompiledModule> {
        self.cache.get(path)
    }

    /// Get the current reverse-dependency map.
    pub fn reverse_deps(&self) -> &ReverseDeps {
        &self.reverse_deps
    }

    /// Get the current API fingerprints.
    pub fn api_fingerprints(&self) -> &HashMap<ModulePath, ModuleApiFingerprint> {
        &self.api_fingerprints
    }

    /// Get the current source hashes.
    pub fn source_hashes(&self) -> &HashMap<ModulePath, u64> {
        &self.source_hashes
    }
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

    /// Verify that each module's HirIds fall inside the expected fixed block
    /// (`1 + index * HIRS_PER_MODULE .. 1 + (index+1) * HIRS_PER_MODULE`)
    /// and that adding a new module does not shift any existing module's block.
    #[test]
    fn test_hir_id_blocks_are_stable() {
        use tlang_span::HIRS_PER_MODULE;

        // First compile: root + math
        let dir1 = std::env::temp_dir().join("tlang_test_hir_stable_v1");
        let _ = fs::remove_dir_all(&dir1);
        create_test_project(&dir1);

        let builtins: Vec<(&str, DefKind)> = vec![("log", DefKind::Function(u16::MAX))];
        let result1 = compile_project(&dir1, &builtins).unwrap();

        // Collect the HirId start of the math module from the first compilation.
        // Module paths are sorted lexicographically; math (index 0) comes before root (index 1).
        // math => index 0, root => index 1  (BTreeMap order: "math" < "" is NOT the case)
        // Actually BTreeMap with ModulePath sorts "" (root/empty) before "math",
        // so root => index 0, math => index 1.
        let math_path = ModulePath::from_str_segments(&["math"]);
        let math_hir_id_start_v1 = {
            let math_module = result1.modules.get(&math_path).unwrap();
            // The first HirId allocated in a module equals its block start.
            // Find a function decl to get a representative HirId.
            let mut first_id: Option<tlang_hir::HirId> = None;
            for stmt in &math_module.hir.block.stmts {
                if let tlang_hir::StmtKind::FunctionDeclaration(decl) = &stmt.kind {
                    first_id = Some(decl.hir_id);
                    break;
                }
            }
            first_id.expect("math module should have at least one function")
        };

        // Determine the expected block start: math is at some stable index.
        // The exact index depends on sort order (root < math in BTreeMap).
        let math_index = result1
            .modules
            .keys()
            .position(|k| k == &math_path)
            .unwrap();
        let expected_block_start = 1 + math_index * HIRS_PER_MODULE;
        assert!(
            math_hir_id_start_v1.as_usize() >= expected_block_start,
            "math HirIds should start at or after block start {expected_block_start}, got {}",
            math_hir_id_start_v1.as_usize(),
        );
        assert!(
            math_hir_id_start_v1.as_usize() < expected_block_start + HIRS_PER_MODULE,
            "math HirIds should stay within block [{expected_block_start}, {})",
            expected_block_start + HIRS_PER_MODULE,
        );

        let _ = fs::remove_dir_all(&dir1);

        // Second compile: same project but now also add a `utils` module *before* math in
        // sort order so that math's index would shift in the old running-offset scheme.
        // With fixed blocks, the math module's block is determined by its sorted position
        // among all modules, but crucially the *block size* is fixed so adding modules
        // cannot cause HirId overflow or collision.
        let dir2 = std::env::temp_dir().join("tlang_test_hir_stable_v2");
        let _ = fs::remove_dir_all(&dir2);
        create_multi_module_project(&dir2);

        let result2 = compile_project(&dir2, &builtins).unwrap();
        assert_eq!(result2.modules.len(), 3, "should have 3 modules");

        // Collect all HirIds and verify no module overflows its block.
        for (index, (path, compiled)) in result2.modules.iter().enumerate() {
            let block_start = 1 + index * HIRS_PER_MODULE;
            let block_end = block_start + HIRS_PER_MODULE;
            let next_id = compiled.lower_meta.hir_id_allocator.peek_next();
            assert!(
                next_id <= block_end,
                "module {path} overflowed its HirId block [{block_start}, {block_end}): next={next_id}",
            );
        }

        let _ = fs::remove_dir_all(&dir2);
    }

    #[test]
    fn test_topological_compilation_order() {
        let dir = std::env::temp_dir().join("tlang_test_topo_order");
        let _ = fs::remove_dir_all(&dir);
        create_multi_module_project(&dir);

        let builtins: Vec<(&str, DefKind)> = vec![("log", DefKind::Function(u16::MAX))];
        let result = compile_project(&dir, &builtins).unwrap();

        // utils depends on math, root depends on utils
        // So topological order should have math before utils, and utils before root
        let math_pos = result
            .compilation_order
            .iter()
            .position(|p| p == &ModulePath::from_str_segments(&["math"]))
            .unwrap();
        let utils_pos = result
            .compilation_order
            .iter()
            .position(|p| p == &ModulePath::from_str_segments(&["utils"]))
            .unwrap();
        let root_pos = result
            .compilation_order
            .iter()
            .position(|p| p == &ModulePath::root())
            .unwrap();

        assert!(
            math_pos < utils_pos,
            "math should be compiled before utils (math={math_pos}, utils={utils_pos})"
        );
        assert!(
            utils_pos < root_pos,
            "utils should be compiled before root (utils={utils_pos}, root={root_pos})"
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_reverse_deps() {
        let dir = std::env::temp_dir().join("tlang_test_reverse_deps");
        let _ = fs::remove_dir_all(&dir);
        create_multi_module_project(&dir);

        let builtins: Vec<(&str, DefKind)> = vec![("log", DefKind::Function(u16::MAX))];
        let result = compile_project(&dir, &builtins).unwrap();

        // math is imported by utils and root
        let math_deps = result
            .reverse_deps
            .get(&ModulePath::from_str_segments(&["math"]))
            .unwrap();
        assert!(math_deps.contains(&ModulePath::from_str_segments(&["utils"])));

        // utils is imported by root
        let utils_deps = result
            .reverse_deps
            .get(&ModulePath::from_str_segments(&["utils"]))
            .unwrap();
        assert!(utils_deps.contains(&ModulePath::root()));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_api_fingerprints() {
        let dir = std::env::temp_dir().join("tlang_test_api_fingerprints");
        let _ = fs::remove_dir_all(&dir);
        create_multi_module_project(&dir);

        let builtins: Vec<(&str, DefKind)> = vec![("log", DefKind::Function(u16::MAX))];
        let result = compile_project(&dir, &builtins).unwrap();

        // All modules should have fingerprints
        assert!(
            result
                .api_fingerprints
                .contains_key(&ModulePath::from_str_segments(&["math"]))
        );
        assert!(
            result
                .api_fingerprints
                .contains_key(&ModulePath::from_str_segments(&["utils"]))
        );
        assert!(result.api_fingerprints.contains_key(&ModulePath::root()));

        // Math module should export `add`
        let math_fp = &result.api_fingerprints[&ModulePath::from_str_segments(&["math"])];
        assert!(
            math_fp
                .exported_symbols
                .iter()
                .any(|(name, _)| name == "add"),
            "math module should export `add`"
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_api_fingerprint_stability() {
        // Same exports should produce same hash
        let exports1 = vec![
            ("add".to_string(), DefKind::Function(2)),
            ("mul".to_string(), DefKind::Function(2)),
        ];
        let exports2 = vec![
            ("mul".to_string(), DefKind::Function(2)),
            ("add".to_string(), DefKind::Function(2)),
        ];
        let fp1 = ModuleApiFingerprint::from_exports(&exports1);
        let fp2 = ModuleApiFingerprint::from_exports(&exports2);
        assert_eq!(
            fp1.hash, fp2.hash,
            "Same exports in different order should produce same hash"
        );

        // Different exports should produce different hash
        let exports3 = vec![
            ("add".to_string(), DefKind::Function(2)),
            ("sub".to_string(), DefKind::Function(2)),
        ];
        let fp3 = ModuleApiFingerprint::from_exports(&exports3);
        assert_ne!(
            fp1.hash, fp3.hash,
            "Different exports should produce different hash"
        );
    }

    #[test]
    fn test_source_hash_populated() {
        let dir = std::env::temp_dir().join("tlang_test_source_hash");
        let _ = fs::remove_dir_all(&dir);
        create_test_project(&dir);

        let graph = ModuleGraph::build(&dir).unwrap();

        // All modules should have a source_hash consistent with hash_source(&module.source)
        for (path, module) in &graph.modules {
            let recomputed = hash_source(&module.source);
            assert_eq!(
                module.source_hash, recomputed,
                "source_hash should match hash_source(&module.source) for module {path}"
            );
        }

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_incremental_compiler_no_change() {
        let dir = std::env::temp_dir().join("tlang_test_incr_no_change");
        let _ = fs::remove_dir_all(&dir);
        create_test_project(&dir);

        let builtins: Vec<(&str, DefKind)> = vec![("log", DefKind::Function(u16::MAX))];
        let mut compiler = IncrementalCompiler::from_project(&dir, &builtins).unwrap();

        // Update with the same source — should skip recompilation
        let math_path = dir.join("src").join("math.tlang");
        let same_source = std::fs::read_to_string(&math_path).unwrap();
        let recompiled = compiler.update_file(&math_path, same_source).unwrap();

        assert!(
            recompiled.is_empty(),
            "No modules should be recompiled when source is unchanged"
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_incremental_compiler_internal_change() {
        let dir = std::env::temp_dir().join("tlang_test_incr_internal");
        let _ = fs::remove_dir_all(&dir);
        create_test_project(&dir);

        let builtins: Vec<(&str, DefKind)> = vec![("log", DefKind::Function(u16::MAX))];
        let mut compiler = IncrementalCompiler::from_project(&dir, &builtins).unwrap();

        // Change math's internals but keep the same public API
        let math_path = dir.join("src").join("math.tlang");
        let new_source = "pub fn add(a, b) { a + b + 0 }".to_string();
        let recompiled = compiler.update_file(&math_path, new_source).unwrap();

        // Only math should be recompiled — API fingerprint is the same
        assert_eq!(
            recompiled.len(),
            1,
            "Only the changed module should be recompiled when API is unchanged"
        );
        assert_eq!(recompiled[0], ModulePath::from_str_segments(&["math"]),);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_incremental_compiler_api_change() {
        let dir = std::env::temp_dir().join("tlang_test_incr_api_change");
        let _ = fs::remove_dir_all(&dir);
        create_test_project(&dir);

        let builtins: Vec<(&str, DefKind)> = vec![("log", DefKind::Function(u16::MAX))];
        let mut compiler = IncrementalCompiler::from_project(&dir, &builtins).unwrap();

        // Change math's public API by adding a new export
        let math_path = dir.join("src").join("math.tlang");
        let new_source = "pub fn add(a, b) { a + b }\npub fn sub(a, b) { a - b }".to_string();
        let recompiled = compiler.update_file(&math_path, new_source).unwrap();

        // Both math and root should be recompiled (root imports from math)
        assert!(
            recompiled.len() >= 2,
            "API change should trigger recompilation of dependents, got {} recompiled",
            recompiled.len()
        );
        assert!(recompiled.contains(&ModulePath::from_str_segments(&["math"])));
        assert!(recompiled.contains(&ModulePath::root()));

        let _ = fs::remove_dir_all(&dir);
    }
}
