mod output;

use std::{
    fs::File,
    io::{Read, Write},
    path::Path,
};

use output::{CompileTarget, ast::AstTarget, hir::HirTarget, hir_raw::HirRawTarget, js::JsTarget};
use tlang_ast_lowering::lower_to_hir;
use tlang_codegen_js::{generator::CodegenJS, js_hir_opt::JsHirOptimizer};
use tlang_hir_opt::{HirOptimizer, HirPass};
use tlang_semantics::SemanticAnalyzer;

use crate::error::ParserError;

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
    ) -> bool {
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

fn compile_standard_library() -> Result<String, ParserError> {
    let source = CodegenJS::get_standard_library_source();
    let mut module = compile_to_hir(&source, &CompileTargetArg::Js, false)?;
    let mut js = JsTarget.compile(&source, &mut module)?;

    js.push('\n');
    js.push_str(CodegenJS::get_standard_library_native_js());

    Ok(js)
}

fn compile_to_hir(
    source: &str,
    target: &CompileTargetArg,
    show_warnings: bool,
) -> Result<tlang_hir::Module, ParserError> {
    let mut parser = tlang_parser::Parser::from_source(source);
    let mut ast = parser.parse()?;

    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(CodegenJS::get_standard_library_symbols());
    semantic_analyzer.analyze(&mut ast)?;

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
    let mut ctx = meta.into();

    let mut optimizer = if matches!(target, CompileTargetArg::Js) {
        CompileTargetHirOptimizer::JavaScript(JsHirOptimizer::default())
    } else {
        CompileTargetHirOptimizer::Interpreter(HirOptimizer::default())
    };

    optimizer.optimize_hir(&mut module, &mut ctx);

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
            let mut module = tlang_hir::Module::default();
            AstTarget.compile(&source, &mut module)
        } else {
            // Standard pipeline: Parse -> AST -> Semantics -> HIR -> Optimize
            match compile_to_hir(&source, &options.target, !options.quiet_warnings) {
                Ok(mut module) => {
                    match (&options.target, &options.output_format) {
                        (CompileTargetArg::Js, OutputFormat::Source) => {
                            if options.source_map {
                                let source_name = path.to_string_lossy();
                                let mut generator = CodegenJS::default();
                                generator.generate_code_with_source_map(
                                    &module,
                                    &source_name,
                                    &source,
                                );
                                if let Some(output_file) = &options.output_file {
                                    // Write source map alongside the output file.
                                    let map_file = format!("{output_file}.map");
                                    if let Some(map) = generator.get_source_map() {
                                        let map_json = map.to_json_string();
                                        let mut f = match File::create(&map_file) {
                                            Err(why) => {
                                                panic!("couldn't create {map_file}: {why}")
                                            }
                                            Ok(f) => f,
                                        };
                                        if let Err(why) = f.write_all(map_json.as_bytes()) {
                                            panic!("couldn't write to {map_file}: {why}")
                                        }
                                    }
                                }
                                Ok(generator.get_output().to_string())
                            } else {
                                JsTarget.compile(&source, &mut module)
                            }
                        }
                        (_, OutputFormat::HirRaw) => HirRawTarget.compile(&source, &mut module),
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
