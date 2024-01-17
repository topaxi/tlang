use std::{
    fs::File,
    io::{Read, Write},
    path::Path,
};

use tlang_ast::symbols::SymbolType;
use tlang_codegen_js::generator::CodegenJS;
use tlang_parser::error::ParseError;
use tlang_semantics::{diagnostic::Diagnostic, SemanticAnalyzer};

#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    input_file: String,

    #[arg(short, long)]
    output_file: Option<String>,
}

fn compile_standard_library() -> Result<(), ParserError> {
    let std_lib_source = compile(&CodegenJS::get_standard_library_source())?;
    let output_file_name = "js/stdlib.js";
    let mut output_file = match File::create(&output_file_name) {
        Err(why) => panic!("couldn't create {}: {}", output_file_name, why),
        Ok(file) => file,
    };
    if let Err(why) = output_file.write_all(std_lib_source.as_bytes()) {
        panic!("couldn't write to {}: {}", output_file_name, why)
    };

    Ok(())
}

// Main entry point for the CLI, which compiles tlang to javascript.
// The cli takes a single argument, the path to the tlang source file.
fn main() {
    let args = <Args as clap::Parser>::parse();
    let path = Path::new(&args.input_file);
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
            eprintln!("{:?}", errors);
            return;
        }
    };

    compile_standard_library().unwrap();

    let std_lib_file = Path::new("js/stdlib.js");
    let mut std_lib = match File::open(std_lib_file) {
        Err(why) => panic!("couldn't open {}: {}", std_lib_file.display(), why),
        Ok(file) => file,
    };
    let mut std_lib_source = String::new();
    if let Err(why) = std_lib.read_to_string(&mut std_lib_source) {
        panic!("couldn't read {}: {}", std_lib_file.display(), why)
    };

    let output = format!("{}\n{}", std_lib_source, output);

    if args.output_file.is_none() {
        println!("{}", output);
        return;
    }

    let output_file_name = args.output_file.unwrap();

    let mut output_file = match File::create(&output_file_name) {
        Err(why) => panic!("couldn't create {}: {}", output_file_name, why),
        Ok(file) => file,
    };
    if let Err(why) = output_file.write_all(output.as_bytes()) {
        panic!("couldn't write to {}: {}", output_file_name, why)
    };
}

#[derive(Debug)]
enum ParserError {
    #[allow(dead_code)]
    ParseError(Vec<ParseError>),
    #[allow(dead_code)]
    DiagnosticError(Vec<Diagnostic>),
}

impl From<Vec<ParseError>> for ParserError {
    fn from(errors: Vec<ParseError>) -> Self {
        ParserError::ParseError(errors)
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
    semantic_analyzer.add_builtin_symbols(&[
        ("log", SymbolType::Function),
        ("max", SymbolType::Function),
        ("min", SymbolType::Function),
        ("floor", SymbolType::Function),
        ("random", SymbolType::Function),
    ]);
    match semantic_analyzer.analyze(&mut ast) {
        Ok(_) => {
            let mut generator = CodegenJS::default();
            generator.generate_code(&ast);
            Ok(generator.get_output().to_string())
        }
        Err(diagnostics) => Err(diagnostics.into()),
    }
}
