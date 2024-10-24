use std::{
    fs::File,
    io::{Read, Write},
    path::Path,
};

use clap::{arg, command, ArgMatches};
use tlang_codegen_js::generator::CodegenJS;
use tlang_parser::error::ParseError;
use tlang_semantics::{diagnostic::Diagnostic, SemanticAnalyzer};

#[derive(Debug)]
struct Args {
    input_file: Option<String>,
    output_file: Option<String>,
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

    Args {
        input_file,
        output_file: matches.get_one::<String>("output_file").cloned(),
        output_stdlib,
        silent,
    }
}

fn get_args() -> Args {
    let matches = command!()
        .arg(arg!(input_file: <INPUT_FILE> "Input file").required(false))
        .arg(arg!(output_file: -o --"output-file" <OUTPUT_FILE> "Output file").required(false))
        .arg(arg!(output_stdlib: --"output-stdlib" "Flag to use stdlib"))
        .arg(arg!(silent: -s --"silent" "Flag to suppress output"))
        .get_matches();

    validate_args(&matches)
}

fn compile_standard_library() -> Result<String, ParserError> {
    compile(&CodegenJS::get_standard_library_source())
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
        };
        let output = match compile(&source) {
            Ok(output) => output,
            Err(errors) => {
                eprintln!("{errors:?}");
                return;
            }
        };

        let std_lib_source = compile_standard_library().unwrap();
        let output = format!("{std_lib_source}\n{{{output}}}");

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
        };
    }
}

#[derive(Debug)]
enum ParserError {
    #[allow(dead_code)]
    ParseError(Vec<ParseError>),
    #[allow(dead_code)]
    DiagnosticError(Vec<Diagnostic>),
}

impl From<&[ParseError]> for ParserError {
    fn from(errors: &[ParseError]) -> Self {
        ParserError::ParseError(errors.to_vec())
    }
}

impl From<Vec<Diagnostic>> for ParserError {
    fn from(errors: Vec<Diagnostic>) -> Self {
        ParserError::DiagnosticError(errors)
    }
}

fn compile(source: &str) -> Result<String, ParserError> {
    let mut parser = tlang_parser::parser::Parser::from_source(source);
    let mut ast = parser.parse()?;
    let mut semantic_analyzer = SemanticAnalyzer::default();
    semantic_analyzer.add_builtin_symbols(CodegenJS::get_standard_library_symbols());
    match semantic_analyzer.analyze(&mut ast) {
        Ok(()) => {
            let mut generator = CodegenJS::default();
            generator.generate_code(&ast);
            Ok(generator.get_output().to_string())
        }
        Err(diagnostics) => Err(diagnostics.into()),
    }
}
