use std::{
    fs::File,
    io::{Read, Write},
    path::Path,
};

use tlang_codegen_js::generator::CodegenJS;

#[derive(clap::Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    input_file: String,

    #[arg(short, long)]
    output_file: Option<String>,
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
    let output = compile(&source);

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

fn compile(source: &str) -> String {
    let lexer = tlang_parser::lexer::Lexer::new(source);
    let mut parser = tlang_parser::parser::Parser::new(lexer);
    let ast = parser.parse_program();
    let mut generator = CodegenJS::default();
    generator.generate_code(&ast);
    generator.get_output().to_string()
}