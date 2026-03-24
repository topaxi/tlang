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

        // Find the target module by exact match on the full path.
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

            // Check that the symbol exists and is declared as `pub` in the target module
            if let Some(target) = graph.modules.get(&target_module_path) {
                if !is_symbol_declared(&target.ast, &item.name) {
                    errors.push(ModuleGraphError::ImportError {
                        module_path: from_module.clone(),
                        symbol_name: item.name.clone(),
                        reason: format!(
                            "`{}` is not declared in module `{}`",
                            item.name, target_module_path
                        ),
                        span: item.span,
                    });
                    continue;
                }
                if !is_symbol_public(&target.ast, &item.name) {
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
    /// Requires an exact match: all segments of the path must correspond to a
    /// registered module. Returns `None` if no module with that exact path exists.
    fn resolve_module_path(graph: &ModuleGraph, path: &[String]) -> Option<ModulePath> {
        let candidate = ModulePath::new(path.to_vec());
        if graph.modules.contains_key(&candidate) {
            Some(candidate)
        } else {
            None
        }
    }
}

/// Check if a symbol with the given name exists in a module's AST.
///
/// When `require_pub` is true, only symbols declared as `pub` match.
fn symbol_visibility(ast: &tlang_ast::node::Module, name: &str, require_pub: bool) -> bool {
    let vis_ok = |vis: &Visibility| !require_pub || *vis == Visibility::Public;
    for stmt in &ast.statements {
        let found = match &stmt.kind {
            StmtKind::FunctionDeclaration(decl) if decl.name().is_some_and(|n| n == name) => {
                vis_ok(&decl.visibility)
            }
            StmtKind::FunctionDeclarations(decls) => decls
                .iter()
                .any(|d| d.name().is_some_and(|n| n == name) && vis_ok(&d.visibility)),
            StmtKind::EnumDeclaration(decl) if decl.name.as_str() == name => {
                vis_ok(&decl.visibility)
            }
            StmtKind::StructDeclaration(decl) if decl.name.as_str() == name => {
                vis_ok(&decl.visibility)
            }
            StmtKind::ProtocolDeclaration(decl) if decl.name.as_str() == name => {
                vis_ok(&decl.visibility)
            }
            _ => false,
        };
        if found {
            return true;
        }
    }
    false
}

/// Check if a symbol is declared (with any visibility) in a module's AST.
fn is_symbol_declared(ast: &tlang_ast::node::Module, name: &str) -> bool {
    symbol_visibility(ast, name, false)
}

/// Check if a symbol is declared as `pub` in a module's AST.
fn is_symbol_public(ast: &tlang_ast::node::Module, name: &str) -> bool {
    symbol_visibility(ast, name, true)
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
            ModuleGraphError::ImportError { symbol_name, reason, .. }
                if symbol_name == "internal" && reason.contains("not declared as `pub`")
        )));

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_undeclared_symbol_import_error() {
        let dir = std::env::temp_dir().join("tlang_test_undeclared_import");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        fs::write(
            src.join("lib.tlang"),
            "pub mod math;\nuse math::nonexistent;",
        )
        .unwrap();
        fs::write(src.join("math.tlang"), "pub fn add(a, b) { a + b }").unwrap();

        let graph = ModuleGraph::build(&dir).unwrap();
        let (_, errors) = ModuleResolver::resolve_imports(&graph);

        assert!(!errors.is_empty());
        assert!(errors.iter().any(|e| matches!(
            e,
            ModuleGraphError::ImportError { symbol_name, reason, .. }
                if symbol_name == "nonexistent" && reason.contains("is not declared in module")
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

    #[test]
    fn test_visibility_error_private_module() {
        let dir = std::env::temp_dir().join("tlang_test_visibility");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        let src_a = src.join("a");
        fs::create_dir_all(&src_a).unwrap();

        // Root declares both `a` (pub) and `app` (pub)
        fs::write(src.join("lib.tlang"), "pub mod a;\npub mod app;").unwrap();
        // Module `a` declares `private` WITHOUT pub — so it is not publicly accessible
        fs::write(src.join("a.tlang"), "mod private;\npub fn helper() { 1 }").unwrap();
        fs::write(src_a.join("private.tlang"), "pub fn secret() { 42 }").unwrap();
        // Module `app` tries to use the private sub-module of `a`
        fs::write(src.join("app.tlang"), "use a::private::secret;").unwrap();

        let graph = ModuleGraph::build(&dir).unwrap();
        let (_, errors) = ModuleResolver::resolve_imports(&graph);

        assert!(!errors.is_empty(), "expected visibility error, got none");
        assert!(errors.iter().any(|e| matches!(
            e,
            ModuleGraphError::VisibilityError { inaccessible_segment, .. }
                if inaccessible_segment == "private"
        )));

        // Verify Display output
        let msg = errors[0].to_string();
        assert!(
            msg.contains("private") || msg.contains("not declared as `pub mod`"),
            "message: {msg}"
        );

        let _ = fs::remove_dir_all(&dir);
    }

    #[test]
    fn test_module_not_found_import_error() {
        let dir = std::env::temp_dir().join("tlang_test_module_not_found");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        // `use nonexistent::symbol` where the module path doesn't resolve
        fs::write(src.join("lib.tlang"), "use nonexistent::symbol;").unwrap();

        let graph = ModuleGraph::build(&dir).unwrap();
        let (_, errors) = ModuleResolver::resolve_imports(&graph);

        assert!(!errors.is_empty());
        assert!(errors.iter().any(|e| matches!(
            e,
            ModuleGraphError::ImportError { reason, .. }
                if reason.contains("not found")
        )));

        let _ = fs::remove_dir_all(&dir);
    }

    /// Verifies that `resolve_module_path` requires an exact match on the full
    /// path.  A partial prefix that exists as a module (e.g. `math`) must NOT
    /// silently match an import like `use math::oops::add` — the intermediate
    /// segment `oops` doesn't exist, so the whole import must fail with a
    /// "module not found" error instead of resolving to the `math` module.
    #[test]
    fn test_partial_path_does_not_resolve() {
        let dir = std::env::temp_dir().join("tlang_test_partial_path_resolve");
        let _ = fs::remove_dir_all(&dir);
        let src = dir.join("src");
        fs::create_dir_all(&src).unwrap();

        // `math` module exists, but `math::oops` does not.
        fs::write(src.join("lib.tlang"), "pub mod math;\nuse math::oops::add;").unwrap();
        fs::write(src.join("math.tlang"), "pub fn add(a, b) { a + b }").unwrap();

        let graph = ModuleGraph::build(&dir).unwrap();
        let (imports, errors) = ModuleResolver::resolve_imports(&graph);

        // The resolution must fail — `math::oops` doesn't exist.
        assert!(
            !errors.is_empty(),
            "expected an import error for non-existent sub-path, got none"
        );
        assert!(errors.iter().any(|e| matches!(
            e,
            ModuleGraphError::ImportError { reason, .. }
                if reason.contains("not found")
        )));

        // The root module must NOT have `add` resolved under any name.
        let root = imports.get(&ModulePath::root()).unwrap();
        assert!(
            !root.symbols.contains_key("add"),
            "partial path must not silently resolve to the nearest prefix module"
        );

        let _ = fs::remove_dir_all(&dir);
    }
}
