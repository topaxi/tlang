use std::collections::HashMap;

use tlang_ast::node::{StmtKind, Visibility};

use crate::module_graph::{ModuleGraphError, UseDeclInfo};
use crate::{ModuleGraph, ModulePath};

/// Resolved import information for a single module.
#[derive(Debug, Default)]
pub struct ResolvedImports {
    /// Map from local name to (source module path, original symbol name).
    pub symbols: HashMap<String, ResolvedSymbol>,
}

#[derive(Debug, Clone)]
pub struct ResolvedSymbol {
    /// The module where the symbol is defined.
    pub source_module: ModulePath,
    /// The original name of the symbol in the source module.
    pub original_name: String,
    /// The local alias (same as original_name if no alias).
    pub local_name: String,
    pub span: tlang_span::Span,
}

/// Resolves all `use` declarations across the module graph.
pub struct ModuleResolver;

impl ModuleResolver {
    /// Resolve all imports in the module graph.
    ///
    /// Returns a map from module path to its resolved imports,
    /// and a list of errors for any unresolvable imports.
    pub fn resolve_imports(
        graph: &ModuleGraph,
    ) -> (HashMap<ModulePath, ResolvedImports>, Vec<ModuleGraphError>) {
        let mut all_imports = HashMap::new();
        let mut errors = Vec::new();

        for (module_path, module) in &graph.modules {
            let mut resolved = ResolvedImports::default();

            for use_decl in &module.use_declarations {
                Self::resolve_use_decl(graph, module_path, use_decl, &mut resolved, &mut errors);
            }

            all_imports.insert(module_path.clone(), resolved);
        }

        (all_imports, errors)
    }

    fn resolve_use_decl(
        graph: &ModuleGraph,
        from_module: &ModulePath,
        use_decl: &UseDeclInfo,
        resolved: &mut ResolvedImports,
        errors: &mut Vec<ModuleGraphError>,
    ) {
        // Resolve the module path
        // `use a::b::c` means import `c` from module `a::b`
        // `use a::b::{c, d}` means import `c` and `d` from module `a::b`

        // Find the target module by trying progressively longer prefixes
        let target_module_path = Self::resolve_module_path(graph, &use_decl.path);

        let target_module_path = match target_module_path {
            Some(p) => p,
            None => {
                let full_path: Vec<String> = use_decl
                    .path
                    .iter()
                    .chain(use_decl.items.iter().map(|i| &i.name))
                    .cloned()
                    .collect();
                errors.push(ModuleGraphError::ImportError {
                    module_path: from_module.clone(),
                    symbol_name: full_path.join("::"),
                    reason: format!("module `{}` not found", use_decl.path.join("::")),
                    span: use_decl.span,
                });
                return;
            }
        };

        // Check visibility: can `from_module` access `target_module_path`?
        if let Some(visible) = graph.visible_modules.get(from_module)
            && !visible.contains(&target_module_path)
        {
            errors.push(ModuleGraphError::VisibilityError {
                from_module: from_module.clone(),
                target_path: use_decl.path.clone(),
                inaccessible_segment: target_module_path.name().unwrap_or("<root>").to_string(),
                span: use_decl.span,
            });
            return;
        }

        // Resolve each imported item
        for item in &use_decl.items {
            let local_name = item.alias.as_ref().unwrap_or(&item.name).clone();

            // Check for name collisions
            if let Some(existing) = resolved.symbols.get(&local_name) {
                errors.push(ModuleGraphError::NameCollision {
                    module_path: from_module.clone(),
                    name: local_name.clone(),
                    first_span: existing.span,
                    second_span: item.span,
                });
                continue;
            }

            // Check that the symbol is declared as `pub` in the target module
            if let Some(target) = graph.modules.get(&target_module_path) {
                let is_pub = is_symbol_public(&target.ast, &item.name);
                if !is_pub {
                    errors.push(ModuleGraphError::ImportError {
                        module_path: from_module.clone(),
                        symbol_name: item.name.clone(),
                        reason: format!(
                            "`{}` is not declared as `pub` in module `{}`",
                            item.name, target_module_path
                        ),
                        span: item.span,
                    });
                    continue;
                }
            }

            resolved.symbols.insert(
                local_name.clone(),
                ResolvedSymbol {
                    source_module: target_module_path.clone(),
                    original_name: item.name.clone(),
                    local_name,
                    span: item.span,
                },
            );
        }
    }

    /// Resolve a use path to a module path.
    ///
    /// Tries progressively longer prefixes of the path to find a matching module.
    fn resolve_module_path(graph: &ModuleGraph, path: &[String]) -> Option<ModulePath> {
        // Try the full path first, then progressively shorter
        let mut candidate = ModulePath::root();
        let mut last_match = None;

        for segment in path {
            candidate = candidate.child(segment);
            if graph.modules.contains_key(&candidate) {
                last_match = Some(candidate.clone());
            }
        }

        last_match
    }
}

/// Check if a symbol is declared as `pub` in a module's AST.
fn is_symbol_public(ast: &tlang_ast::node::Module, name: &str) -> bool {
    for stmt in &ast.statements {
        match &stmt.kind {
            StmtKind::FunctionDeclaration(decl)
                if decl.visibility == Visibility::Public && decl.name() == name =>
            {
                return true;
            }
            StmtKind::FunctionDeclarations(decls)
                if decls
                    .iter()
                    .any(|d| d.visibility == Visibility::Public && d.name() == name) =>
            {
                return true;
            }
            StmtKind::EnumDeclaration(decl)
                if decl.visibility == Visibility::Public && decl.name.as_str() == name =>
            {
                return true;
            }
            StmtKind::StructDeclaration(decl)
                if decl.visibility == Visibility::Public && decl.name.as_str() == name =>
            {
                return true;
            }
            StmtKind::ProtocolDeclaration(decl)
                if decl.visibility == Visibility::Public && decl.name.as_str() == name =>
            {
                return true;
            }
            _ => {}
        }
    }
    false
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;

    #[test]
    fn test_resolve_imports() {
        let dir = std::env::temp_dir().join("tlang_test_resolve_imports");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        fs::write(src.join("lib.tlang"), "pub mod math;\nuse math::add;").unwrap();
        fs::write(src.join("math.tlang"), "pub fn add(a, b) { a + b }").unwrap();

        let graph = ModuleGraph::build(&dir).unwrap();
        let (imports, errors) = ModuleResolver::resolve_imports(&graph);

        assert!(errors.is_empty(), "errors: {errors:?}");

        let root_imports = imports.get(&ModulePath::root()).unwrap();
        assert!(root_imports.symbols.contains_key("add"));

        let add = &root_imports.symbols["add"];
        assert_eq!(add.source_module, ModulePath::from_str_segments(&["math"]));
        assert_eq!(add.original_name, "add");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_resolve_aliased_import() {
        let dir = std::env::temp_dir().join("tlang_test_resolve_alias");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        fs::write(
            src.join("lib.tlang"),
            "pub mod math;\nuse math::add as plus;",
        )
        .unwrap();
        fs::write(src.join("math.tlang"), "pub fn add(a, b) { a + b }").unwrap();

        let graph = ModuleGraph::build(&dir).unwrap();
        let (imports, errors) = ModuleResolver::resolve_imports(&graph);

        assert!(errors.is_empty(), "errors: {errors:?}");

        let root_imports = imports.get(&ModulePath::root()).unwrap();
        assert!(root_imports.symbols.contains_key("plus"));
        assert!(!root_imports.symbols.contains_key("add"));

        let plus = &root_imports.symbols["plus"];
        assert_eq!(plus.original_name, "add");

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_private_symbol_import_error() {
        let dir = std::env::temp_dir().join("tlang_test_private_import");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        fs::write(src.join("lib.tlang"), "pub mod math;\nuse math::internal;").unwrap();
        fs::write(src.join("math.tlang"), "fn internal() { 1 }").unwrap();

        let graph = ModuleGraph::build(&dir).unwrap();
        let (_, errors) = ModuleResolver::resolve_imports(&graph);

        assert!(!errors.is_empty());
        assert!(errors.iter().any(|e| matches!(
            e,
            ModuleGraphError::ImportError { symbol_name, .. } if symbol_name == "internal"
        )));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_name_collision_error() {
        let dir = std::env::temp_dir().join("tlang_test_collision");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        fs::write(
            src.join("lib.tlang"),
            "pub mod a;\npub mod b;\nuse a::foo;\nuse b::foo;",
        )
        .unwrap();
        fs::write(src.join("a.tlang"), "pub fn foo() { 1 }").unwrap();
        fs::write(src.join("b.tlang"), "pub fn foo() { 2 }").unwrap();

        let graph = ModuleGraph::build(&dir).unwrap();
        let (_, errors) = ModuleResolver::resolve_imports(&graph);

        assert!(!errors.is_empty());
        assert!(
            errors.iter().any(
                |e| matches!(e, ModuleGraphError::NameCollision { name, .. } if name == "foo")
            )
        );

        let _ = fs::remove_dir_all(&dir);
    }
}
