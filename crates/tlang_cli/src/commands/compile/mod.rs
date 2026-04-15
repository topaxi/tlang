mod output;

use std::{
    fs::File,
    io::{Read, Write},
    path::Path,
};

use output::{
    CompileTarget, ast::AstTarget, codegen_errors_to_diagnostics, codegen_warnings_to_diagnostics,
    hir::HirTarget, hir_raw::HirRawTarget, js::JsTarget,
};
use tlang_ast_lowering::lower_to_hir;
use tlang_codegen_js::{
    generator::{CodegenJS, shift_source_map_lines},
    js_hir_opt::JsHirOptimizer,
};
use tlang_diagnostics::{Diagnostic, diagnostics_from_parse_error, render_diagnostics, render_ice};
use tlang_hir_opt::{HirOptError, HirOptimizer, HirPass};
use tlang_semantics::SemanticAnalyzer;

use crate::commands::{optimize_and_typecheck, print_source_diagnostics};

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum CompileTargetArg {
    Js,
}

#[derive(Debug, PartialEq, Eq, Clone, Copy)]
pub enum OutputFormat {
    Source,
    Ast,
    HirRaw,
    Hir,
}

pub enum CompileTargetHirOptimizer {
    Interpreter(HirOptimizer),
    JavaScript(JsHirOptimizer),
}

impl HirPass for CompileTargetHirOptimizer {
    fn optimize_hir(
        &mut self,
        module: &mut tlang_hir::Module,
        ctx: &mut tlang_hir_opt::hir_opt::HirOptContext,
    ) -> Result<bool, HirOptError> {
        match self {
            CompileTargetHirOptimizer::Interpreter(optimizer) => {
                optimizer.optimize_hir(module, ctx)
            }
            CompileTargetHirOptimizer::JavaScript(optimizer) => optimizer.optimize_hir(module, ctx),
        }
    }
}

#[derive(Debug)]
pub struct CompileOptions {
    pub input_file: Option<String>,
    pub output_file: Option<String>,
    pub target: CompileTargetArg,
    pub output_format: OutputFormat,
    pub output_stdlib: bool,
    pub silent: bool,
    pub quiet_warnings: bool,
    pub source_map: bool,
}

fn get_compiled_standard_library() -> &'static str {
    static STDLIB: std::sync::OnceLock<String> = std::sync::OnceLock::new();
    STDLIB.get_or_init(CodegenJS::compile_stdlib_module)
}

fn compile_to_hir(
    source_name: &str,
    source: &str,
    target: &CompileTargetArg,
    show_warnings: bool,
) -> Result<tlang_hir::Module, Vec<Diagnostic>> {
    let mut parser = tlang_parser::Parser::from_source(source);
    let (mut ast, parse_meta) = parser
        .parse()
        .map_err(|e| diagnostics_from_parse_error(&e))?;

    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(CodegenJS::get_standard_library_symbols());
    semantic_analyzer.analyze(&mut ast)?;

    // Display warnings (but don't fail compilation)
    if show_warnings {
        let warnings = semantic_analyzer
            .get_diagnostics()
            .into_iter()
            .filter(|diagnostic| diagnostic.is_warning())
            .collect::<Vec<_>>();

        print_source_diagnostics(source_name, source, &warnings);
    }

    let (mut module, meta) = lower_to_hir(
        &ast,
        &parse_meta.constant_pool_node_ids,
        semantic_analyzer.symbol_id_allocator(),
        semantic_analyzer.root_symbol_table(),
        semantic_analyzer.symbol_tables().clone(),
    )
    .map_err(|errs| {
        errs.iter()
            .map(|e| Diagnostic::error(&e.to_string(), e.span()))
            .collect::<Vec<_>>()
    })?;
    let mut ctx = meta.into();

    let mut optimizer = if matches!(target, CompileTargetArg::Js) {
        CompileTargetHirOptimizer::JavaScript(JsHirOptimizer::default())
    } else {
        CompileTargetHirOptimizer::Interpreter(HirOptimizer::default())
    };

    let diagnostics = match optimize_and_typecheck(&mut optimizer, &mut module, &mut ctx) {
        Ok(diagnostics) => diagnostics,
        Err(err) => {
            eprint!("{}", render_ice(&err));
            std::process::exit(1);
        }
    };

    if show_warnings {
        print_source_diagnostics(source_name, source, &diagnostics.warnings);
    }

    if diagnostics.has_errors() {
        return Err(diagnostics.errors);
    }

    Ok(module)
}

pub fn handle_compile(options: CompileOptions) -> bool {
    if options.output_stdlib {
        if !options.silent {
            println!("{}", get_compiled_standard_library());
        }
        return true;
    }

    if let Some(input_file) = &options.input_file {
        let path = Path::new(&input_file);
        let mut file = match File::open(path) {
            Err(why) => panic!("couldn't open {}: {}", path.display(), why),
            Ok(file) => file,
        };
        let mut source = String::new();
        if let Err(why) = file.read_to_string(&mut source) {
            panic!("couldn't read {}: {}", path.display(), why)
        }

        let (mut output, source_map) = match compile_source(path, &source, &options) {
            Ok(result) => result,
            Err(errors) => {
                eprint!(
                    "{}",
                    render_diagnostics(
                        &path.to_string_lossy(),
                        &source,
                        &errors,
                        std::io::IsTerminal::is_terminal(&std::io::stderr()),
                    )
                );
                return false;
            }
        };

        if let Some(map_json) = source_map {
            append_source_map(&mut output, &map_json, options.output_file.as_deref());
        }

        if options.output_file.is_none() {
            println!("{output}");
            return true;
        }

        let output_file_name = options.output_file.unwrap();

        let mut output_file = match File::create(&output_file_name) {
            Err(why) => panic!("couldn't create {output_file_name}: {why}"),
            Ok(file) => file,
        };
        if let Err(why) = output_file.write_all(output.as_bytes()) {
            panic!("couldn't write to {output_file_name}: {why}")
        }
    }

    true
}

/// Compile `source` through the full pipeline and return `(js_output, optional_map_json)`.
/// The map JSON (when present) already has its generated lines shifted to
/// account for the stdlib preamble that is prepended to the JS output.
fn compile_source(
    path: &Path,
    source: &str,
    options: &CompileOptions,
) -> Result<(String, Option<String>), Vec<Diagnostic>> {
    let source_name = path.to_string_lossy();
    // Holds the unshifted source map JSON; shifted once we know the stdlib
    // preamble line count.
    let mut pending_source_map: Option<String> = None;

    let output_result = if options.output_format == OutputFormat::Ast {
        let mut module = tlang_hir::Module::default();
        AstTarget.compile(source, &mut module)
    } else {
        match compile_to_hir(
            &source_name,
            source,
            &options.target,
            !options.quiet_warnings,
        ) {
            Ok(mut module) => match (&options.target, &options.output_format) {
                (CompileTargetArg::Js, OutputFormat::Source) => {
                    if options.source_map {
                        let source_name = path.to_string_lossy();
                        let mut generator = CodegenJS::default();
                        generator
                            .generate_code_with_source_map(&module, &source_name, source)
                            .map_err(codegen_errors_to_diagnostics)?;
                        pending_source_map = generator.get_source_map_json();
                        let warnings =
                            codegen_warnings_to_diagnostics(generator.get_warnings().to_vec());
                        Ok((generator.get_output().to_string(), warnings))
                    } else {
                        JsTarget.compile(source, &mut module)
                    }
                }
                (_, OutputFormat::HirRaw) => HirRawTarget.compile(source, &mut module),
                (_, OutputFormat::Hir) => HirTarget.compile(source, &mut module),
                _ => panic!("Unsupported target/format combination"),
            },
            Err(err) => Err(err),
        }
    };

    let (output, codegen_warnings) = output_result?;

    // Display codegen warnings (unless suppressed).
    if !options.quiet_warnings && !codegen_warnings.is_empty() {
        eprint!(
            "{}",
            render_diagnostics(
                &source_name,
                source,
                &codegen_warnings,
                std::io::IsTerminal::is_terminal(&std::io::stderr()),
            )
        );
    }

    let output = if matches!(
        (&options.target, &options.output_format),
        (CompileTargetArg::Js, OutputFormat::Source)
    ) {
        let std_lib_source = get_compiled_standard_library();
        if let Some(map_json) = pending_source_map.take() {
            let stdlib_line_count = std_lib_source.lines().count();
            pending_source_map = Some(shift_source_map_lines(&map_json, stdlib_line_count));
        }
        format!("{std_lib_source}\n{{{output}}}")
    } else {
        output
    };

    Ok((output, pending_source_map))
}

/// Append a `//# sourceMappingURL=` comment to `output` and, when an output
/// file is given, write the map to `<output_file>.map`.  Falls back to an
/// inline base64 data URL when no output file is available (e.g. stdout).
fn append_source_map(output: &mut String, map_json: &str, output_file: Option<&str>) {
    if let Some(output_file) = output_file {
        let map_file = format!("{output_file}.map");
        match File::create(&map_file) {
            Err(why) => panic!("couldn't create {map_file}: {why}"),
            Ok(mut f) => {
                if let Err(why) = f.write_all(map_json.as_bytes()) {
                    panic!("couldn't write to {map_file}: {why}");
                }
            }
        }
        let map_basename = format!(
            "{}.map",
            Path::new(output_file)
                .file_name()
                .unwrap_or_default()
                .to_string_lossy()
        );
        output.push_str(&format!("\n//# sourceMappingURL={map_basename}"));
    } else {
        let encoded = base64_encode(map_json.as_bytes());
        output.push_str(&format!(
            "\n//# sourceMappingURL=data:application/json;base64,{encoded}"
        ));
    }
}

/// Minimal base64 encoder — avoids a heavy dependency for embedding inline
/// source map data URLs in stdout output.
fn base64_encode(data: &[u8]) -> String {
    const CHARS: &[u8; 64] = b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";
    let mut out = String::with_capacity((data.len() * 4).div_ceil(3));
    for chunk in data.chunks(3) {
        let b0 = chunk[0] as u32;
        let b1 = if chunk.len() > 1 { chunk[1] as u32 } else { 0 };
        let b2 = if chunk.len() > 2 { chunk[2] as u32 } else { 0 };
        let n = (b0 << 16) | (b1 << 8) | b2;
        out.push(CHARS[((n >> 18) & 63) as usize] as char);
        out.push(CHARS[((n >> 12) & 63) as usize] as char);
        out.push(if chunk.len() > 1 {
            CHARS[((n >> 6) & 63) as usize] as char
        } else {
            '='
        });
        out.push(if chunk.len() > 2 {
            CHARS[(n & 63) as usize] as char
        } else {
            '='
        });
    }
    out
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::{
        fs,
        time::{SystemTime, UNIX_EPOCH},
    };

    #[test]
    fn handle_compile_keeps_existing_output_when_compilation_fails() {
        let unique = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let temp_dir = std::env::temp_dir().join(format!(
            "tlang-handle-compile-{unique}-{}",
            std::process::id()
        ));
        fs::create_dir_all(&temp_dir).unwrap();

        let input_file = temp_dir.join("input.tlang");
        let output_file = temp_dir.join("output.js");
        fs::write(&input_file, "missing_symbol;").unwrap();
        fs::write(&output_file, "existing output").unwrap();

        handle_compile(CompileOptions {
            input_file: Some(input_file.to_string_lossy().into_owned()),
            output_file: Some(output_file.to_string_lossy().into_owned()),
            target: CompileTargetArg::Js,
            output_format: OutputFormat::Source,
            output_stdlib: false,
            silent: false,
            quiet_warnings: false,
            source_map: false,
        });

        assert_eq!(fs::read_to_string(&output_file).unwrap(), "existing output");

        fs::remove_dir_all(&temp_dir).unwrap();
    }
}
