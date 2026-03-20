use tlang_diagnostics::{render_error_at_span, render_parse_issues, render_semantic_diagnostics};
use tlang_modules::{CompileError, ModuleGraphError};

/// Render a multi-module compilation error with source-aware diagnostics.
pub fn render_compile_error(err: &CompileError) -> String {
    match err {
        CompileError::ModuleGraphErrors(errors) => render_module_graph_errors(errors),
        CompileError::ImportErrors { errors, sources } => {
            render_module_graph_errors_with_sources(errors, sources)
        }
        CompileError::SemanticError {
            file_path,
            source,
            diagnostics,
            ..
        } => {
            let source_name = file_path.to_string_lossy();
            render_semantic_diagnostics(&source_name, source, diagnostics)
        }
    }
}

fn render_module_graph_errors(errors: &[ModuleGraphError]) -> String {
    let mut parts = Vec::new();

    for error in errors {
        match error {
            ModuleGraphError::ParseError {
                file_path,
                source,
                issues,
                ..
            } => {
                let source_name = file_path.to_string_lossy();
                parts.push(render_parse_issues(&source_name, source, issues));
            }
            ModuleGraphError::MissingModule {
                declared_in,
                name,
                span,
            } => {
                // We don't have the source for the declaring module readily available
                // in the error, but we can produce a helpful message.
                parts.push(format!(
                    "error: module `{declared_in}` declares `mod {name}`, \
                     but no corresponding file was found\n  \
                     hint: create `src/{}.tlang` to define this module\n",
                    if declared_in.is_root() {
                        name.clone()
                    } else {
                        format!("{declared_in}/{name}").replace("::", "/")
                    }
                ));
                let _ = span; // Span available for future source-context rendering
            }
            ModuleGraphError::VisibilityError {
                from_module,
                target_path,
                inaccessible_segment,
                ..
            } => {
                parts.push(format!(
                    "error: module `{from_module}` cannot access `{}`\n  \
                     hint: add `pub mod {inaccessible_segment}` to the parent module \
                     to make it accessible\n",
                    target_path.join("::")
                ));
            }
            ModuleGraphError::ImportError {
                module_path,
                symbol_name,
                reason,
                ..
            } => {
                parts.push(format!(
                    "error: cannot import `{symbol_name}` in module `{module_path}`\n  \
                     {reason}\n"
                ));
            }
            ModuleGraphError::NameCollision {
                module_path, name, ..
            } => {
                parts.push(format!(
                    "error: name collision in module `{module_path}`: `{name}` is already imported\n  \
                     hint: use `as` to rename one of the imports\n"
                ));
            }
            ModuleGraphError::CycleError { cycle, reason } => {
                let path_str: Vec<_> = cycle.iter().map(|p| p.to_string()).collect();
                parts.push(format!(
                    "error: module cycle detected: {}\n  {reason}\n",
                    path_str.join(" → ")
                ));
            }
            ModuleGraphError::TreeError(e) => {
                parts.push(format!("error: {e}\n"));
            }
        }
    }

    parts.join("")
}

/// Render module graph errors that have source context available.
/// This is used when the ModuleGraph is still available and we can look up sources.
fn render_module_graph_errors_with_sources(
    errors: &[ModuleGraphError],
    sources: &tlang_modules::ModuleSourceInfo,
) -> String {
    let mut parts = Vec::new();

    for error in errors {
        match error {
            ModuleGraphError::ParseError {
                file_path,
                source,
                issues,
                ..
            } => {
                let source_name = file_path.to_string_lossy();
                parts.push(render_parse_issues(&source_name, source, issues));
            }
            ModuleGraphError::MissingModule {
                declared_in,
                name,
                span,
            } => {
                if let Some((file_path, source)) = sources.get(declared_in) {
                    let source_name = file_path.to_string_lossy();
                    parts.push(render_error_at_span(
                        &source_name,
                        source,
                        &format!("`mod {name}` declared but no corresponding file found"),
                        span,
                    ));
                    parts.push(format!(
                        "  hint: create `src/{}.tlang` to define this module\n\n",
                        if declared_in.is_root() {
                            name.clone()
                        } else {
                            format!("{declared_in}/{name}").replace("::", "/")
                        }
                    ));
                } else {
                    parts.push(format!(
                        "error: module `{declared_in}` declares `mod {name}`, \
                         but no corresponding file was found\n"
                    ));
                }
            }
            ModuleGraphError::VisibilityError {
                from_module,
                target_path,
                inaccessible_segment,
                span,
            } => {
                if let Some((file_path, source)) = sources.get(from_module) {
                    let source_name = file_path.to_string_lossy();
                    parts.push(render_error_at_span(
                        &source_name,
                        source,
                        &format!(
                            "cannot access `{}`: `{inaccessible_segment}` is not declared as `pub mod`",
                            target_path.join("::")
                        ),
                        span,
                    ));
                    parts.push(format!(
                        "  hint: add `pub mod {inaccessible_segment}` to the parent module\n\n"
                    ));
                } else {
                    parts.push(format!(
                        "error: module `{from_module}` cannot access `{}`\n",
                        target_path.join("::")
                    ));
                }
            }
            ModuleGraphError::ImportError {
                module_path,
                symbol_name,
                reason,
                span,
            } => {
                if let Some((file_path, source)) = sources.get(module_path) {
                    let source_name = file_path.to_string_lossy();
                    parts.push(render_error_at_span(
                        &source_name,
                        source,
                        &format!("cannot import `{symbol_name}`: {reason}"),
                        span,
                    ));
                } else {
                    parts.push(format!(
                        "error: cannot import `{symbol_name}` in module `{module_path}`: {reason}\n"
                    ));
                }
            }
            ModuleGraphError::NameCollision {
                module_path,
                name,
                second_span,
                ..
            } => {
                if let Some((file_path, source)) = sources.get(module_path) {
                    let source_name = file_path.to_string_lossy();
                    parts.push(render_error_at_span(
                        &source_name,
                        source,
                        &format!("`{name}` is already imported, use `as` to rename"),
                        second_span,
                    ));
                } else {
                    parts.push(format!(
                        "error: name collision in module `{module_path}`: `{name}` is already imported\n"
                    ));
                }
            }
            ModuleGraphError::CycleError { cycle, reason } => {
                let path_str: Vec<_> = cycle.iter().map(|p| p.to_string()).collect();
                parts.push(format!(
                    "error: module cycle detected: {}\n  {reason}\n",
                    path_str.join(" → ")
                ));
            }
            ModuleGraphError::TreeError(e) => {
                parts.push(format!("error: {e}\n"));
            }
        }
    }

    parts.join("")
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::BTreeMap;
    use std::path::PathBuf;
    use tlang_modules::ModulePath;
    use tlang_span::Span;

    #[test]
    fn test_render_graph_errors_parse() {
        let errors = vec![ModuleGraphError::ParseError {
            module_path: ModulePath::from_str_segments(&["math"]),
            file_path: PathBuf::from("src/math.tlang"),
            source: "fn add(a, b { a + b }".to_string(),
            issues: vec![],
        }];
        let _output = render_module_graph_errors(&errors);
    }

    #[test]
    fn test_render_graph_errors_missing_module_root() {
        let errors = vec![ModuleGraphError::MissingModule {
            declared_in: ModulePath::root(),
            name: "utils".to_string(),
            span: Span::default(),
        }];
        let output = render_module_graph_errors(&errors);
        assert!(output.contains("mod utils"));
        assert!(output.contains("no corresponding file"));
        assert!(output.contains("src/utils.tlang"));
    }

    #[test]
    fn test_render_graph_errors_missing_module_nested() {
        let errors = vec![ModuleGraphError::MissingModule {
            declared_in: ModulePath::from_str_segments(&["string"]),
            name: "parse".to_string(),
            span: Span::default(),
        }];
        let output = render_module_graph_errors(&errors);
        assert!(output.contains("mod parse"));
        assert!(output.contains("string/parse.tlang"));
    }

    #[test]
    fn test_render_graph_errors_visibility() {
        let errors = vec![ModuleGraphError::VisibilityError {
            from_module: ModulePath::root(),
            target_path: vec!["string".to_string(), "parse".to_string()],
            inaccessible_segment: "parse".to_string(),
            span: Span::default(),
        }];
        let output = render_module_graph_errors(&errors);
        assert!(output.contains("cannot access"));
        assert!(output.contains("string::parse"));
        assert!(output.contains("pub mod parse"));
    }

    #[test]
    fn test_render_graph_errors_import() {
        let errors = vec![ModuleGraphError::ImportError {
            module_path: ModulePath::root(),
            symbol_name: "foo".to_string(),
            reason: "not found".to_string(),
            span: Span::default(),
        }];
        let output = render_module_graph_errors(&errors);
        assert!(output.contains("cannot import `foo`"));
        assert!(output.contains("not found"));
    }

    #[test]
    fn test_render_graph_errors_name_collision() {
        let errors = vec![ModuleGraphError::NameCollision {
            module_path: ModulePath::root(),
            name: "add".to_string(),
            first_span: Span::default(),
            second_span: Span::default(),
        }];
        let output = render_module_graph_errors(&errors);
        assert!(output.contains("name collision"));
        assert!(output.contains("`add`"));
        assert!(output.contains("use `as`"));
    }

    #[test]
    fn test_render_graph_errors_cycle() {
        let errors = vec![ModuleGraphError::CycleError {
            cycle: vec![
                ModulePath::from_str_segments(&["a"]),
                ModulePath::from_str_segments(&["b"]),
            ],
            reason: "contains top-level expressions".to_string(),
        }];
        let output = render_module_graph_errors(&errors);
        assert!(output.contains("module cycle detected"));
        assert!(output.contains("top-level expressions"));
    }

    #[test]
    fn test_render_graph_errors_tree_error() {
        let errors = vec![ModuleGraphError::TreeError(
            tlang_modules::ModuleTreeError::NoSrcDirectory(PathBuf::from("/missing")),
        )];
        let output = render_module_graph_errors(&errors);
        assert!(output.contains("error:"));
    }

    #[test]
    fn test_render_compile_error_semantic() {
        let err = CompileError::SemanticError {
            module_path: ModulePath::from_str_segments(&["math"]),
            file_path: PathBuf::from("src/math.tlang"),
            source: String::new(),
            diagnostics: vec![],
        };
        let output = render_compile_error(&err);
        assert!(output.is_empty());
    }

    #[test]
    fn test_render_compile_error_module_graph() {
        let err = CompileError::ModuleGraphErrors(vec![ModuleGraphError::CycleError {
            cycle: vec![ModulePath::from_str_segments(&["x"])],
            reason: "side effects".to_string(),
        }]);
        let output = render_compile_error(&err);
        assert!(output.contains("module cycle detected"));
    }

    #[test]
    fn test_render_compile_error_import() {
        let err = CompileError::ImportErrors {
            errors: vec![ModuleGraphError::ImportError {
                module_path: ModulePath::root(),
                symbol_name: "bar".to_string(),
                reason: "not declared as `pub`".to_string(),
                span: Span::default(),
            }],
            sources: BTreeMap::new(),
        };
        let output = render_compile_error(&err);
        assert!(output.contains("cannot import `bar`"));
    }

    #[test]
    fn test_with_sources_missing_module_with_source() {
        let mut sources = BTreeMap::new();
        let root_path = ModulePath::root();
        sources.insert(
            root_path.clone(),
            (
                PathBuf::from("src/lib.tlang"),
                "pub mod missing;\n".to_string(),
            ),
        );

        let errors = vec![ModuleGraphError::MissingModule {
            declared_in: root_path,
            name: "missing".to_string(),
            span: Span::default(),
        }];
        let output = render_module_graph_errors_with_sources(&errors, &sources);
        assert!(output.contains("no corresponding file found"));
        assert!(output.contains("src/missing.tlang"));
    }

    #[test]
    fn test_with_sources_missing_module_no_source() {
        let errors = vec![ModuleGraphError::MissingModule {
            declared_in: ModulePath::from_str_segments(&["deep"]),
            name: "sub".to_string(),
            span: Span::default(),
        }];
        let output = render_module_graph_errors_with_sources(&errors, &BTreeMap::new());
        assert!(output.contains("mod sub"));
        assert!(output.contains("no corresponding file"));
    }

    #[test]
    fn test_with_sources_visibility_with_source() {
        let mut sources = BTreeMap::new();
        let root_path = ModulePath::root();
        sources.insert(
            root_path.clone(),
            (
                PathBuf::from("src/lib.tlang"),
                "use math::internal;\n".to_string(),
            ),
        );

        let errors = vec![ModuleGraphError::VisibilityError {
            from_module: root_path,
            target_path: vec!["math".to_string(), "internal".to_string()],
            inaccessible_segment: "internal".to_string(),
            span: Span::default(),
        }];
        let output = render_module_graph_errors_with_sources(&errors, &sources);
        assert!(output.contains("not declared as `pub mod`"));
    }

    #[test]
    fn test_with_sources_visibility_no_source() {
        let errors = vec![ModuleGraphError::VisibilityError {
            from_module: ModulePath::root(),
            target_path: vec!["x".to_string()],
            inaccessible_segment: "x".to_string(),
            span: Span::default(),
        }];
        let output = render_module_graph_errors_with_sources(&errors, &BTreeMap::new());
        assert!(output.contains("cannot access"));
    }

    #[test]
    fn test_with_sources_import_with_source() {
        let mut sources = BTreeMap::new();
        let root_path = ModulePath::root();
        sources.insert(
            root_path.clone(),
            (
                PathBuf::from("src/lib.tlang"),
                "use math::secret;\n".to_string(),
            ),
        );

        let errors = vec![ModuleGraphError::ImportError {
            module_path: root_path,
            symbol_name: "secret".to_string(),
            reason: "not declared as `pub`".to_string(),
            span: Span::default(),
        }];
        let output = render_module_graph_errors_with_sources(&errors, &sources);
        assert!(output.contains("cannot import `secret`"));
    }

    #[test]
    fn test_with_sources_import_no_source() {
        let errors = vec![ModuleGraphError::ImportError {
            module_path: ModulePath::root(),
            symbol_name: "x".to_string(),
            reason: "not found".to_string(),
            span: Span::default(),
        }];
        let output = render_module_graph_errors_with_sources(&errors, &BTreeMap::new());
        assert!(output.contains("cannot import `x`"));
    }

    #[test]
    fn test_with_sources_name_collision_with_source() {
        let mut sources = BTreeMap::new();
        let root_path = ModulePath::root();
        sources.insert(
            root_path.clone(),
            (
                PathBuf::from("src/lib.tlang"),
                "use a::add;\nuse b::add;\n".to_string(),
            ),
        );

        let errors = vec![ModuleGraphError::NameCollision {
            module_path: root_path,
            name: "add".to_string(),
            first_span: Span::default(),
            second_span: Span::default(),
        }];
        let output = render_module_graph_errors_with_sources(&errors, &sources);
        assert!(output.contains("already imported"));
    }

    #[test]
    fn test_with_sources_name_collision_no_source() {
        let errors = vec![ModuleGraphError::NameCollision {
            module_path: ModulePath::root(),
            name: "x".to_string(),
            first_span: Span::default(),
            second_span: Span::default(),
        }];
        let output = render_module_graph_errors_with_sources(&errors, &BTreeMap::new());
        assert!(output.contains("name collision"));
    }

    #[test]
    fn test_with_sources_cycle() {
        let errors = vec![ModuleGraphError::CycleError {
            cycle: vec![ModulePath::from_str_segments(&["a"])],
            reason: "bad".to_string(),
        }];
        let output = render_module_graph_errors_with_sources(&errors, &BTreeMap::new());
        assert!(output.contains("module cycle detected"));
    }

    #[test]
    fn test_with_sources_tree_error() {
        let errors = vec![ModuleGraphError::TreeError(
            tlang_modules::ModuleTreeError::NoSrcDirectory(PathBuf::from("/x")),
        )];
        let output = render_module_graph_errors_with_sources(&errors, &BTreeMap::new());
        assert!(output.contains("error:"));
    }

    #[test]
    fn test_with_sources_parse_error() {
        let errors = vec![ModuleGraphError::ParseError {
            module_path: ModulePath::root(),
            file_path: PathBuf::from("src/lib.tlang"),
            source: "fn bad {".to_string(),
            issues: vec![],
        }];
        let _output = render_module_graph_errors_with_sources(&errors, &BTreeMap::new());
    }

    #[test]
    fn test_with_sources_missing_module_nested() {
        let mut sources = BTreeMap::new();
        let parent = ModulePath::from_str_segments(&["string"]);
        sources.insert(
            parent.clone(),
            (
                PathBuf::from("src/string.tlang"),
                "pub mod parse;\n".to_string(),
            ),
        );

        let errors = vec![ModuleGraphError::MissingModule {
            declared_in: parent,
            name: "parse".to_string(),
            span: Span::default(),
        }];
        let output = render_module_graph_errors_with_sources(&errors, &sources);
        assert!(output.contains("string/parse.tlang"));
    }
}
