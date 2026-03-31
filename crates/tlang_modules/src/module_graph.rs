use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};
use std::path::PathBuf;

use tlang_ast::node::{self, StmtKind, Visibility};
use tlang_parser::error::ParseIssue;
use tlang_parser::{ParseMeta, Parser};

use crate::module_tree::ModuleTreeError;
use crate::{ModulePath, ModuleTree};

/// A parsed module with its AST and metadata.
#[derive(Debug)]
pub struct ParsedModule {
    pub path: ModulePath,
    pub file_path: PathBuf,
    pub source: String,
    pub ast: node::Module,
    pub parse_meta: ParseMeta,
    /// Module names declared via `pub mod` or `mod`.
    pub mod_declarations: Vec<ModDeclInfo>,
    /// Use declarations for import resolution.
    pub use_declarations: Vec<UseDeclInfo>,
}

#[derive(Debug, Clone)]
pub struct ModDeclInfo {
    pub name: String,
    pub visibility: Visibility,
    pub span: tlang_span::Span,
}

#[derive(Debug, Clone)]
pub struct UseDeclInfo {
    /// The path prefix (e.g., `["string", "parse"]` in `use string::parse::foo`).
    pub path: Vec<String>,
    /// The imported items.
    pub items: Vec<UseItemInfo>,
    pub span: tlang_span::Span,
}

#[derive(Debug, Clone)]
pub struct UseItemInfo {
    pub name: String,
    pub alias: Option<String>,
    pub span: tlang_span::Span,
}

/// The complete module graph, with all modules parsed and analyzed.
#[derive(Debug)]
pub struct ModuleGraph {
    /// The module tree (filesystem structure).
    pub tree: ModuleTree,
    /// Parsed modules, keyed by module path.
    pub modules: BTreeMap<ModulePath, ParsedModule>,
    /// Set of modules that are publicly exposed (reachable from root via `pub mod` chains).
    pub public_modules: BTreeSet<ModulePath>,
    /// Map from module path to the set of module paths it can access (siblings + pub-exposed children).
    pub visible_modules: HashMap<ModulePath, BTreeSet<ModulePath>>,
}

#[derive(Debug)]
pub enum ModuleGraphError {
    TreeError(ModuleTreeError),
    ParseError {
        module_path: ModulePath,
        file_path: PathBuf,
        source: String,
        issues: Vec<ParseIssue>,
    },
    CycleError {
        cycle: Vec<ModulePath>,
        reason: String,
    },
    MissingModule {
        declared_in: ModulePath,
        name: String,
        span: tlang_span::Span,
    },
    VisibilityError {
        from_module: ModulePath,
        target_path: Vec<String>,
        inaccessible_segment: String,
        span: tlang_span::Span,
    },
    ImportError {
        module_path: ModulePath,
        symbol_name: String,
        reason: String,
        span: tlang_span::Span,
    },
    NameCollision {
        module_path: ModulePath,
        name: String,
        first_span: tlang_span::Span,
        second_span: tlang_span::Span,
    },
}

impl std::fmt::Display for ModuleGraphError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            ModuleGraphError::TreeError(e) => write!(f, "{e}"),
            ModuleGraphError::ParseError {
                module_path,
                file_path,
                issues,
                ..
            } => {
                let errors: Vec<_> = issues.iter().map(|i| i.to_string()).collect();
                write!(
                    f,
                    "parse error in module `{module_path}` ({}): {}",
                    file_path.display(),
                    errors.join(", ")
                )
            }
            ModuleGraphError::CycleError { cycle, reason } => {
                let path_str: Vec<_> = cycle.iter().map(|p| p.to_string()).collect();
                write!(
                    f,
                    "module cycle detected: {} — {reason}",
                    path_str.join(" -> ")
                )
            }
            ModuleGraphError::MissingModule {
                declared_in, name, ..
            } => {
                write!(
                    f,
                    "module `{declared_in}` declares `mod {name}`, but no corresponding file was found"
                )
            }
            ModuleGraphError::VisibilityError {
                from_module,
                target_path,
                inaccessible_segment,
                ..
            } => {
                write!(
                    f,
                    "module `{from_module}` cannot access `{}`: `{inaccessible_segment}` is not declared as `pub mod`",
                    target_path.join("::")
                )
            }
            ModuleGraphError::ImportError {
                module_path,
                symbol_name,
                reason,
                ..
            } => {
                write!(
                    f,
                    "import error in module `{module_path}`: cannot import `{symbol_name}` — {reason}"
                )
            }
            ModuleGraphError::NameCollision {
                module_path, name, ..
            } => {
                write!(
                    f,
                    "name collision in module `{module_path}`: `{name}` is already imported"
                )
            }
        }
    }
}

impl std::error::Error for ModuleGraphError {}

impl ModuleGraph {
    /// Build the module graph from a project root directory.
    ///
    /// This performs:
    /// 1. Filesystem scanning (module tree construction)
    /// 2. Parsing all discovered .tlang files
    /// 3. Extracting `mod` and `use` declarations
    /// 4. Computing visibility (pub mod chains)
    /// 5. Cycle detection
    pub fn build(root_dir: &std::path::Path) -> Result<Self, Vec<ModuleGraphError>> {
        let tree = ModuleTree::from_directory(root_dir)
            .map_err(|e| vec![ModuleGraphError::TreeError(e)])?;

        let mut modules = BTreeMap::new();
        let mut errors = Vec::new();

        // Phase 1: Parse all modules
        parse_all_modules(&tree.root, &mut modules, &mut errors);

        if !errors.is_empty() {
            return Err(errors);
        }

        // Phase 2: Validate mod declarations point to existing modules
        validate_mod_declarations(&tree, &modules, &mut errors);

        // Phase 3: Compute public module visibility
        let public_modules = compute_public_modules(&modules);

        // Phase 4: Compute per-module visible modules
        let visible_modules = compute_visible_modules(&tree, &modules, &public_modules);

        // Phase 5: Detect and validate cycles
        detect_cycles(&modules, &mut errors);

        if !errors.is_empty() {
            return Err(errors);
        }

        Ok(ModuleGraph {
            tree,
            modules,
            public_modules,
            visible_modules,
        })
    }

    /// Extract source information from all modules, useful for rendering
    /// source-aware diagnostics after errors.
    pub fn source_info(&self) -> BTreeMap<ModulePath, (PathBuf, String)> {
        self.modules
            .iter()
            .map(|(path, module)| {
                (
                    path.clone(),
                    (module.file_path.clone(), module.source.clone()),
                )
            })
            .collect()
    }
}

fn parse_all_modules(
    node: &crate::module_tree::ModuleNode,
    modules: &mut BTreeMap<ModulePath, ParsedModule>,
    errors: &mut Vec<ModuleGraphError>,
) {
    if let Some(ref file_path) = node.file_path {
        let source = match std::fs::read_to_string(file_path) {
            Ok(s) => s,
            Err(e) => {
                errors.push(ModuleGraphError::ParseError {
                    module_path: node.path.clone(),
                    file_path: file_path.clone(),
                    source: String::new(),
                    issues: vec![ParseIssue {
                        msg: e.to_string(),
                        kind: tlang_parser::error::ParseIssueKind::UnexpectedEof,
                        span: tlang_span::Span::default(),
                    }],
                });
                return;
            }
        };

        let mut parser = Parser::from_source(&source);
        match parser.parse() {
            Ok((ast, parse_meta)) => {
                let (mod_declarations, use_declarations) = extract_declarations(&ast);
                modules.insert(
                    node.path.clone(),
                    ParsedModule {
                        path: node.path.clone(),
                        file_path: file_path.clone(),
                        source,
                        ast,
                        parse_meta,
                        mod_declarations,
                        use_declarations,
                    },
                );
            }
            Err(e) => {
                errors.push(ModuleGraphError::ParseError {
                    module_path: node.path.clone(),
                    file_path: file_path.clone(),
                    source,
                    issues: e.issues().to_vec(),
                });
            }
        }
    }

    for child in node.children.values() {
        parse_all_modules(child, modules, errors);
    }
}

fn extract_declarations(ast: &node::Module) -> (Vec<ModDeclInfo>, Vec<UseDeclInfo>) {
    let mut mod_decls = Vec::new();
    let mut use_decls = Vec::new();

    for stmt in &ast.statements {
        match &stmt.kind {
            StmtKind::ModDeclaration(decl) => {
                for name in &decl.names {
                    mod_decls.push(ModDeclInfo {
                        name: name.as_str().to_string(),
                        visibility: decl.visibility,
                        span: stmt.span,
                    });
                }
            }
            StmtKind::UseDeclaration(decl) => {
                use_decls.push(UseDeclInfo {
                    path: decl.path.iter().map(|id| id.as_str().to_string()).collect(),
                    items: decl
                        .items
                        .iter()
                        .map(|item| UseItemInfo {
                            name: item.name.as_str().to_string(),
                            alias: item.alias.as_ref().map(|a| a.as_str().to_string()),
                            span: item.span,
                        })
                        .collect(),
                    span: stmt.span,
                });
            }
            _ => {}
        }
    }

    (mod_decls, use_decls)
}

fn validate_mod_declarations(
    tree: &ModuleTree,
    modules: &BTreeMap<ModulePath, ParsedModule>,
    errors: &mut Vec<ModuleGraphError>,
) {
    for module in modules.values() {
        for mod_decl in &module.mod_declarations {
            let child_path = module.path.child(&mod_decl.name);
            if tree.get(&child_path).is_none() || tree.get(&child_path).unwrap().file_path.is_none()
            {
                errors.push(ModuleGraphError::MissingModule {
                    declared_in: module.path.clone(),
                    name: mod_decl.name.clone(),
                    span: mod_decl.span,
                });
            }
        }
    }
}

/// Compute the set of publicly visible modules by following `pub mod` chains from root.
fn compute_public_modules(modules: &BTreeMap<ModulePath, ParsedModule>) -> BTreeSet<ModulePath> {
    let mut public = BTreeSet::new();
    // Root is always public
    public.insert(ModulePath::root());

    // BFS from root following pub mod declarations
    let mut queue = vec![ModulePath::root()];

    while let Some(current) = queue.pop() {
        if let Some(module) = modules.get(&current) {
            for mod_decl in &module.mod_declarations {
                if mod_decl.visibility == Visibility::Public {
                    let child_path = current.child(&mod_decl.name);
                    if public.insert(child_path.clone()) {
                        queue.push(child_path);
                    }
                }
            }
        }
    }

    public
}

/// Compute for each module which other modules it can access.
///
/// A module can access:
/// - Sibling modules (implicit, same parent directory)
/// - Any module reachable via `pub mod` chains from an accessible ancestor
fn compute_visible_modules(
    tree: &ModuleTree,
    modules: &BTreeMap<ModulePath, ParsedModule>,
    public_modules: &BTreeSet<ModulePath>,
) -> HashMap<ModulePath, BTreeSet<ModulePath>> {
    let mut visible = HashMap::new();

    for module_path in modules.keys() {
        let mut accessible = BTreeSet::new();

        // A module can always access itself
        accessible.insert(module_path.clone());

        // Sibling modules (children of same parent) are implicitly visible
        if let Some(parent_path) = module_path.parent() {
            if let Some(parent_node) = tree.get(&parent_path) {
                for (name, child) in &parent_node.children {
                    if child.file_path.is_some() {
                        accessible.insert(parent_path.child(name));
                    }
                }
            }
        } else {
            // Root module: direct children are siblings
            for (name, child) in &tree.root.children {
                if child.file_path.is_some() {
                    accessible.insert(ModulePath::root().child(name));
                }
            }
        }

        // All publicly exposed modules are accessible
        for pub_module in public_modules {
            accessible.insert(pub_module.clone());
        }

        // A module can access its own children (they declared mod for them)
        if let Some(module) = modules.get(module_path) {
            for mod_decl in &module.mod_declarations {
                let child = module_path.child(&mod_decl.name);
                accessible.insert(child);
            }
        }

        visible.insert(module_path.clone(), accessible);
    }

    visible
}

/// Detect cycles in the module graph based on `use` declarations.
///
/// Cycles are allowed only if all modules in the cycle contain no top-level
/// executable code (only function/type declarations).
fn detect_cycles(modules: &BTreeMap<ModulePath, ParsedModule>, errors: &mut Vec<ModuleGraphError>) {
    // Build adjacency list from use declarations
    let mut adj: HashMap<&ModulePath, HashSet<ModulePath>> = HashMap::new();

    for (path, module) in modules {
        let edges = adj.entry(path).or_default();
        for use_decl in &module.use_declarations {
            if !use_decl.path.is_empty() {
                // use_decl.path is already the exact module path of the target
                let target = ModulePath::new(use_decl.path.clone());
                if modules.contains_key(&target) {
                    edges.insert(target);
                }
            }
        }
    }

    // Find SCCs using Tarjan's algorithm
    let all_paths: Vec<&ModulePath> = modules.keys().collect();
    let sccs = tarjan_scc(&all_paths, &adj);

    for scc in sccs {
        if scc.len() <= 1 {
            continue;
        }

        // Check if all modules in the cycle are safe (no top-level executable code)
        for path in &scc {
            if let Some(module) = modules.get(path)
                && has_top_level_executable_code(&module.ast)
            {
                errors.push(ModuleGraphError::CycleError {
                    cycle: scc.clone(),
                    reason: format!("module `{path}` contains top-level executable code"),
                });
                break;
            }
        }
    }
}

fn has_top_level_executable_code(ast: &node::Module) -> bool {
    for stmt in &ast.statements {
        match &stmt.kind {
            // Declarations are fine
            StmtKind::FunctionDeclaration(_)
            | StmtKind::FunctionDeclarations(_)
            | StmtKind::EnumDeclaration(_)
            | StmtKind::StructDeclaration(_)
            | StmtKind::ProtocolDeclaration(_)
            | StmtKind::ImplBlock(_)
            | StmtKind::UseDeclaration(_)
            | StmtKind::ModDeclaration(_)
            | StmtKind::Const(_)
            | StmtKind::None => {}
            // Let declarations and expressions are executable code
            StmtKind::Let(_) | StmtKind::Expr(_) | StmtKind::Return(_) => {
                return true;
            }
        }
    }
    false
}

/// Tarjan's SCC algorithm
fn tarjan_scc<'a>(
    nodes: &[&'a ModulePath],
    adj: &HashMap<&'a ModulePath, HashSet<ModulePath>>,
) -> Vec<Vec<ModulePath>> {
    struct State<'a> {
        index: usize,
        indices: HashMap<&'a ModulePath, usize>,
        lowlinks: HashMap<&'a ModulePath, usize>,
        on_stack: HashSet<&'a ModulePath>,
        stack: Vec<&'a ModulePath>,
        sccs: Vec<Vec<ModulePath>>,
    }

    fn strongconnect<'a>(
        v: &'a ModulePath,
        adj: &HashMap<&'a ModulePath, HashSet<ModulePath>>,
        state: &mut State<'a>,
        all_nodes: &HashSet<&'a ModulePath>,
    ) {
        state.indices.insert(v, state.index);
        state.lowlinks.insert(v, state.index);
        state.index += 1;
        state.stack.push(v);
        state.on_stack.insert(v);

        if let Some(neighbors) = adj.get(v) {
            for w_owned in neighbors {
                // Find the reference in all_nodes
                if let Some(&w) = all_nodes.get(w_owned) {
                    if !state.indices.contains_key(w) {
                        strongconnect(w, adj, state, all_nodes);
                        let w_low = state.lowlinks[w];
                        let v_low = state.lowlinks.get_mut(v).unwrap();
                        if w_low < *v_low {
                            *v_low = w_low;
                        }
                    } else if state.on_stack.contains(w) {
                        let w_idx = state.indices[w];
                        let v_low = state.lowlinks.get_mut(v).unwrap();
                        if w_idx < *v_low {
                            *v_low = w_idx;
                        }
                    }
                }
            }
        }

        if state.lowlinks[v] == state.indices[v] {
            let mut scc = Vec::new();
            loop {
                let w = state.stack.pop().unwrap();
                state.on_stack.remove(w);
                scc.push(w.clone());
                if w == v {
                    break;
                }
            }
            state.sccs.push(scc);
        }
    }

    let all_nodes_set: HashSet<&ModulePath> = nodes.iter().copied().collect();
    let mut state = State {
        index: 0,
        indices: HashMap::new(),
        lowlinks: HashMap::new(),
        on_stack: HashSet::new(),
        stack: Vec::new(),
        sccs: Vec::new(),
    };

    for &v in nodes {
        if !state.indices.contains_key(v) {
            strongconnect(v, adj, &mut state, &all_nodes_set);
        }
    }

    state.sccs
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    fn create_test_project(dir: &std::path::Path) {
        let src = dir.join("src");
        fs::create_dir_all(src.join("string")).unwrap();

        fs::write(src.join("lib.tlang"), "pub mod string;\nuse string::trim;").unwrap();
        fs::write(
            src.join("string.tlang"),
            "pub mod parse, utils;\npub fn trim(s) { s }",
        )
        .unwrap();
        fs::write(
            src.join("string").join("parse.tlang"),
            "pub fn from_char_code(n) { n }",
        )
        .unwrap();
        fs::write(
            src.join("string").join("utils.tlang"),
            "pub fn upper(s) { s }",
        )
        .unwrap();
    }

    #[test]
    fn test_build_module_graph() {
        let dir = std::env::temp_dir().join("tlang_test_module_graph");
        let _ = fs::remove_dir_all(&dir);
        create_test_project(&dir);

        let graph = ModuleGraph::build(&dir).unwrap();

        assert_eq!(graph.modules.len(), 4);
        assert!(graph.modules.contains_key(&ModulePath::root()));
        assert!(
            graph
                .modules
                .contains_key(&ModulePath::from_str_segments(&["string"]))
        );
        assert!(
            graph
                .modules
                .contains_key(&ModulePath::from_str_segments(&["string", "parse"]))
        );
        assert!(
            graph
                .modules
                .contains_key(&ModulePath::from_str_segments(&["string", "utils"]))
        );

        // Check public modules
        assert!(graph.public_modules.contains(&ModulePath::root()));
        assert!(
            graph
                .public_modules
                .contains(&ModulePath::from_str_segments(&["string"]))
        );
        assert!(
            graph
                .public_modules
                .contains(&ModulePath::from_str_segments(&["string", "parse"]))
        );
        assert!(
            graph
                .public_modules
                .contains(&ModulePath::from_str_segments(&["string", "utils"]))
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_cycle_detection_safe() {
        let dir = std::env::temp_dir().join("tlang_test_cycle_safe");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        // Two modules that reference each other but only have declarations
        fs::write(src.join("lib.tlang"), "pub mod a;\npub mod b;").unwrap();
        fs::write(src.join("a.tlang"), "use b::bar;\npub fn foo() { 1 }").unwrap();
        fs::write(src.join("b.tlang"), "use a::foo;\npub fn bar() { 2 }").unwrap();

        let graph = ModuleGraph::build(&dir).unwrap();
        assert_eq!(graph.modules.len(), 3);

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_cycle_detection_unsafe() {
        let dir = std::env::temp_dir().join("tlang_test_cycle_unsafe");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        // Cycle with top-level executable code
        fs::write(src.join("lib.tlang"), "pub mod a;\npub mod b;").unwrap();
        fs::write(src.join("a.tlang"), "use b::bar;\nlet x = bar();").unwrap();
        fs::write(
            src.join("b.tlang"),
            "use a::foo;\npub fn foo() { 1 }\npub fn bar() { 2 }",
        )
        .unwrap();

        let result = ModuleGraph::build(&dir);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, ModuleGraphError::CycleError { .. }))
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_missing_mod_declaration() {
        let dir = std::env::temp_dir().join("tlang_test_missing_mod");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        fs::write(src.join("lib.tlang"), "pub mod nonexistent;").unwrap();

        let result = ModuleGraph::build(&dir);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        assert!(errors.iter().any(
            |e| matches!(e, ModuleGraphError::MissingModule { name, .. } if name == "nonexistent")
        ));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_parse_error_in_module() {
        let dir = std::env::temp_dir().join("tlang_test_parse_error");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        // Declare a module and provide a corresponding file with invalid syntax
        // so that building the module graph triggers a parse error in that module.
        fs::write(src.join("lib.tlang"), "pub mod bad;").unwrap();

        // `html"{x y}";` triggers a parse error (extra token `y` inside the
        // interpolation body) that is returned as an `Err` without panicking.
        fs::write(src.join("bad.tlang"), r#"html"{x y}";"#).unwrap();

        let result = ModuleGraph::build(&dir);
        assert!(result.is_err());
        let errors = result.unwrap_err();
        // Expect at least one parse error coming from the `bad` module.
        assert!(
            errors
                .iter()
                .any(|e| matches!(e, ModuleGraphError::ParseError { .. })),
            "expected a ParseError, got: {errors:?}"
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_source_info() {
        let dir = std::env::temp_dir().join("tlang_test_source_info");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        fs::write(src.join("lib.tlang"), "pub mod math;\nuse math::add;").unwrap();
        fs::write(src.join("math.tlang"), "pub fn add(a, b) { a + b }").unwrap();

        let graph = ModuleGraph::build(&dir).unwrap();
        let info = graph.source_info();

        assert_eq!(info.len(), 2);
        assert!(info.contains_key(&ModulePath::root()));
        assert!(info.contains_key(&ModulePath::from_str_segments(&["math"])));

        let (root_path, root_source) = &info[&ModulePath::root()];
        assert!(root_path.ends_with("lib.tlang"));
        assert!(root_source.contains("pub mod math"));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_display_visibility_error() {
        let err = ModuleGraphError::VisibilityError {
            from_module: ModulePath::root(),
            target_path: vec!["private".to_string()],
            inaccessible_segment: "private".to_string(),
            span: tlang_span::Span::default(),
        };
        let msg = err.to_string();
        assert!(msg.contains("private"));
        assert!(msg.contains("not declared as `pub mod`"));
    }

    #[test]
    fn test_display_tree_error() {
        use crate::module_tree::ModuleTreeError;
        let err = ModuleGraphError::TreeError(ModuleTreeError::NoSrcDirectory(
            std::path::PathBuf::from("/nonexistent"),
        ));
        let msg = err.to_string();
        assert!(!msg.is_empty());
    }
}
