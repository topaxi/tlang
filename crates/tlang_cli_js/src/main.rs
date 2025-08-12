use std::{
    fs::File,
    io::{Read, Write},
    path::Path,
};

use clap::{ArgMatches, arg, command};
use tlang_ast_lowering::lower_to_hir;
use tlang_codegen_js::generator::CodegenJS;
use tlang_parser::error::ParseIssue;
use tlang_semantics::{SemanticAnalyzer, diagnostic::Diagnostic};

#[derive(Debug, PartialEq)]
enum OutputType {
    Ast,
    Hir,
    Js,
}

#[derive(Debug)]
struct Args {
    input_file: Option<String>,
    output_file: Option<String>,
    output_type: OutputType,
    output_stdlib: bool,
    silent: bool,
}

fn validate_args(matches: &ArgMatches) -> Args {
    let silent = matches.get_flag("silent");
    let output_stdlib = matches.get_flag("output_stdlib");

    let input_file = if output_stdlib {
        None
    } else {
        matches.get_one::<String>("input_file").cloned()
    };

    if !output_stdlib && input_file.is_none() {
        eprintln!("Error: input_file is required unless output_stdlib is true.");
        std::process::exit(1);
    }

    let output_type = matches.get_one::<String>("output_type").cloned();
    let output_type = match output_type.as_deref() {
        Some("ast") => OutputType::Ast,
        Some("hir") => OutputType::Hir,
        Some("js") | None => OutputType::Js,
        _ => {
            eprintln!("Error: output_type must be one of 'ast', 'hir', or 'js'.");
            std::process::exit(1);
        }
    };

    let output_file = matches.get_one::<String>("output_file").cloned();

    Args {
        input_file,
        output_file,
        output_type,
        output_stdlib,
        silent,
    }
}

fn get_args() -> Args {
    let matches = command!()
        .arg(arg!(input_file: <INPUT_FILE> "Input file").required(false))
        .arg(arg!(output_file: -o --"output-file" <OUTPUT_FILE> "Output file").required(false))
        .arg(arg!(output_stdlib: --"output-stdlib" "Flag to output the stdlib"))
        .arg(arg!(output_type: -t --"output-type" <OUTPUT_TYPE> "Output type, defaults to js"))
        .arg(arg!(silent: -s --"silent" "Flag to suppress output"))
        .get_matches();

    validate_args(&matches)
}

fn compile_standard_library() -> Result<String, ParserError> {
    let mut js = compile(&CodegenJS::get_standard_library_source())?;

    js.push_str("\nfunction panic(msg) { throw new Error(msg); }\n");

    Ok(js)
}

// Main entry point for the CLI, which compiles tlang to javascript.
// The cli takes a single argument, the path to the tlang source file.
fn main() {
    let args = get_args();

    if args.output_stdlib {
        if !args.silent {
            println!("{}", compile_standard_library().unwrap());
        }

        return;
    }

    if let Some(input_file) = &args.input_file {
        let path = Path::new(&input_file);
        let mut file = match File::open(path) {
            Err(why) => panic!("couldn't open {}: {}", path.display(), why),
            Ok(file) => file,
        };
        let mut source = String::new();
        if let Err(why) = file.read_to_string(&mut source) {
            panic!("couldn't read {}: {}", path.display(), why)
        }

        let output = match args.output_type {
            OutputType::Ast => compile_to_ast(&source),
            OutputType::Hir => compile_to_hir(&source),
            OutputType::Js => compile(&source),
        };

        let output = match output {
            Ok(output) => output,
            Err(errors) => {
                eprintln!("{errors:?}");
                return;
            }
        };

        let output = if args.output_type == OutputType::Js {
            let std_lib_source = compile_standard_library().unwrap();
            format!("{std_lib_source}\n{{{output}}}")
        } else {
            output
        };

        if args.output_file.is_none() {
            println!("{output}");
            return;
        }

        let output_file_name = args.output_file.unwrap();

        let mut output_file = match File::create(&output_file_name) {
            Err(why) => panic!("couldn't create {output_file_name}: {why}"),
            Ok(file) => file,
        };
        if let Err(why) = output_file.write_all(output.as_bytes()) {
            panic!("couldn't write to {output_file_name}: {why}")
        }
    }
}

#[derive(Debug)]
enum ParserError {
    #[allow(dead_code)]
    ParseError(Vec<ParseIssue>),
    #[allow(dead_code)]
    DiagnosticError(Vec<Diagnostic>),
}

impl From<tlang_parser::error::ParseError> for ParserError {
    fn from(errors: tlang_parser::error::ParseError) -> Self {
        ParserError::ParseError(errors.issues().to_vec())
    }
}

impl From<Vec<Diagnostic>> for ParserError {
    fn from(errors: Vec<Diagnostic>) -> Self {
        ParserError::DiagnosticError(errors)
    }
}

fn compile_to_ast(source: &str) -> Result<String, ParserError> {
    let mut parser = tlang_parser::Parser::from_source(source);
    let ast = parser.parse()?;
    Ok(ron::ser::to_string_pretty(&ast, ron::ser::PrettyConfig::default()).unwrap())
}

fn compile_to_hir(source: &str) -> Result<String, ParserError> {
    let mut parser = tlang_parser::Parser::from_source(source);
    let ast = parser.parse()?;
    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(CodegenJS::get_standard_library_symbols());
    semantic_analyzer.analyze(&ast)?;
    let hir = lower_to_hir(
        &ast,
        semantic_analyzer.symbol_id_allocator(),
        semantic_analyzer.symbol_tables().clone(),
    );
    Ok(ron::ser::to_string_pretty(&hir.module, ron::ser::PrettyConfig::default()).unwrap())
}

fn compile(source: &str) -> Result<String, ParserError> {
    let mut parser = tlang_parser::Parser::from_source(source);
    let ast = parser.parse()?;
    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(CodegenJS::get_standard_library_symbols());
    match semantic_analyzer.analyze(&ast) {
        Ok(()) => {
            let mut generator = CodegenJS::default();
            let hir = lower_to_hir(
                &ast,
                semantic_analyzer.symbol_id_allocator(),
                semantic_analyzer.symbol_tables().clone(),
            );
            generator.generate_code(&hir.module);
            Ok(generator.get_output().to_string())
        }
        Err(diagnostics) => Err(diagnostics.into()),
    }
}
