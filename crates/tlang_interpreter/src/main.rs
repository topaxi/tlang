use std::env;
use std::fs;
use std::process;

use tlang_ast_lowering::lower_to_hir;
use tlang_interpreter::Interpreter;

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        eprintln!("Usage: {} <file.tlang>", args[0]);
        process::exit(1);
    }

    let filename = &args[1];

    if !filename.ends_with(".tlang") {
        eprintln!("Error: The file must have a '.tlang' extension.");
        process::exit(1);
    }

    let code = match fs::read_to_string(filename) {
        Ok(content) => content,
        Err(err) => {
            eprintln!("Error reading file '{}': {}", filename, err);
            process::exit(1);
        }
    };

    let mut parser = tlang_parser::parser::Parser::from_source(&code);
    let ast = match parser.parse() {
        Ok(ast) => ast,
        Err(err) => {
            eprintln!("Error parsing file '{}': {}", filename, err);
            process::exit(1);
        }
    };
    let hir = lower_to_hir(&ast);
    let mut interp = Interpreter::default();
    interp.eval(&hir);
}
