mod output;

use std::{
    fs::File,
    io::{Read, Write},
    path::Path,
};

use output::{CompileTarget, ast::AstTarget, hir::HirTarget, js::JsTarget};
use tlang_ast_lowering::lower_to_hir;
use tlang_codegen_js::generator::CodegenJS;
use tlang_hir::hir;
use tlang_hir_opt::HirOptimizer;
use tlang_semantics::SemanticAnalyzer;

use crate::error::ParserError;

#[derive(Debug, PartialEq, Clone)]
pub enum CompileTargetArg {
    Js,
}

#[derive(Debug, PartialEq, Clone)]
pub enum OutputFormat {
    Source,
    Hir,
    Ast,
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
}

fn compile_standard_library() -> Result<String, ParserError> {
    let source = CodegenJS::get_standard_library_source();
    let mut module = compile_to_hir(&source, &CompileTargetArg::Js, false)?;
    let mut js = JsTarget.compile(&source, &mut module)?;

    js.push_str("\nfunction panic(msg) { throw new Error(msg); }\n");

    Ok(js)
}

fn compile_to_hir(
    source: &str,
    target: &CompileTargetArg,
    show_warnings: bool,
) -> Result<hir::Module, ParserError> {
    let mut parser = tlang_parser::Parser::from_source(source);
    let ast = parser.parse()?;

    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(CodegenJS::get_standard_library_symbols());
    semantic_analyzer.analyze(&ast)?;

    // Display warnings (but don't fail compilation)
    if show_warnings {
        for diagnostic in semantic_analyzer.get_diagnostics() {
            if diagnostic.is_warning() {
                eprintln!("{diagnostic}");
            }
        }
    }

    let (mut module, meta) = lower_to_hir(
        &ast,
        semantic_analyzer.symbol_id_allocator(),
        semantic_analyzer.root_symbol_table(),
        semantic_analyzer.symbol_tables().clone(),
    );

    let mut optimizer = HirOptimizer::default();
    optimizer.optimize_hir(&mut module, meta.into());

    if matches!(target, CompileTargetArg::Js) {
        // Future: Apply JS-specific HIR optimizations here
        // optimizer.optimize_for_js(&mut module);
    }

    Ok(module)
}

pub fn handle_compile(options: CompileOptions) {
    if options.output_stdlib {
        if !options.silent {
            println!("{}", compile_standard_library().unwrap());
        }
        return;
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

        let output_result = if options.output_format == OutputFormat::Ast {
            // AST output has a separate pipeline path as it doesn't require HIR lowering
            let mut module = hir::Module::default();
            AstTarget.compile(&source, &mut module)
        } else {
            // Standard pipeline: Parse -> AST -> Semantics -> HIR -> Optimize
            match compile_to_hir(&source, &options.target, !options.quiet_warnings) {
                Ok(mut module) => {
                    match (&options.target, &options.output_format) {
                        (CompileTargetArg::Js, OutputFormat::Source) => {
                            JsTarget.compile(&source, &mut module)
                        }
                        (_, OutputFormat::Hir) => HirTarget.compile(&source, &mut module),
                        // Invalid combinations should be caught by argument parsing, but as a fallback:
                        _ => panic!("Unsupported target/format combination"),
                    }
                }
                Err(err) => Err(err),
            }
        };

        let output = match output_result {
            Ok(output) => output,
            Err(errors) => {
                eprintln!("{errors:?}");
                return;
            }
        };

        let output = if matches!(
            (&options.target, &options.output_format),
            (CompileTargetArg::Js, OutputFormat::Source)
        ) {
            let std_lib_source = compile_standard_library().unwrap();
            format!("{std_lib_source}\n{{{output}}}")
        } else {
            output
        };

        if options.output_file.is_none() {
            println!("{output}");
            return;
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
}
