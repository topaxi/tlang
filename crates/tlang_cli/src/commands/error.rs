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
