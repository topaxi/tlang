use clap::{ArgMatches, arg, command};

mod commands;

use commands::build::{BuildOptions, handle_build};
use commands::compile::{CompileOptions, CompileTargetArg, OutputFormat, handle_compile};
use commands::run::handle_run;

#[derive(Debug)]
enum Command {
    Run { input_file: String },
    Compile(CompileOptions),
    Build(BuildOptions),
}

fn validate_compile_args(matches: &ArgMatches) -> Command {
    let silent = matches.get_flag("silent");
    let output_stdlib = matches.get_flag("output_stdlib");
    let quiet_warnings = matches.get_flag("quiet_warnings");
    let source_map = matches.get_flag("source_map");

    let input_file = if output_stdlib {
        None
    } else {
        matches.get_one::<String>("input_file").cloned()
    };

    if !output_stdlib && input_file.is_none() {
        eprintln!("Error: input_file is required unless output-stdlib is true.");
        std::process::exit(1);
    }

    let output_type = matches.get_one::<String>("output_type").cloned();
    let output_format = match output_type.as_deref() {
        Some("ast") => OutputFormat::Ast,
        Some("hir-raw") => OutputFormat::HirRaw,
        Some("hir") => OutputFormat::Hir,
        Some("js") | None => OutputFormat::Source,
        _ => {
            eprintln!("Error: output_type must be one of 'ast', 'hir-raw', 'hir', or 'js'.");
            std::process::exit(1);
        }
    };

    let output_file = matches.get_one::<String>("output_file").cloned();

    Command::Compile(CompileOptions {
        input_file,
        output_file,
        target: CompileTargetArg::Js, // Default to JS target for now
        output_format,
        output_stdlib,
        silent,
        quiet_warnings,
        source_map,
    })
}

fn get_args() -> Command {
    let mut command = command!()
        .subcommand(
            command!("run")
                .about("Run a tlang file")
                .arg(arg!(input_file: <INPUT_FILE> "Input file").required(true)),
        )
        .subcommand(
            command!("compile")
                .about("Compile a tlang file")
                .arg(arg!(input_file: <INPUT_FILE> "Input file").required(false))
                .arg(arg!(output_file: -o --"output-file" <OUTPUT_FILE> "Output file").required(false))
                .arg(arg!(output_stdlib: --"output-stdlib" "Flag to output the stdlib"))
                .arg(arg!(output_type: -t --"output-type" <OUTPUT_TYPE> "Output type, defaults to js"))
                .arg(arg!(silent: -s --"silent" "Flag to suppress output"))
                .arg(arg!(quiet_warnings: -q --"quiet-warnings" "Flag to suppress warning output"))
                .arg(arg!(source_map: --"source-map" "Emit a source map alongside JS output, or inline when writing to stdout")),
        )
        .subcommand(
            command!("build")
                .about("Build a multi-module tlang project")
                .arg(arg!(project_dir: [PROJECT_DIR] "Project directory (defaults to current directory)").default_value("."))
                .arg(arg!(output_file: -o --"output-file" <OUTPUT_FILE> "Output file").required(false)),
        );

    let matches = command.clone().get_matches();

    match matches.subcommand() {
        Some(("run", sub_matches)) => Command::Run {
            input_file: sub_matches.get_one::<String>("input_file").unwrap().clone(),
        },
        Some(("compile", sub_matches)) => validate_compile_args(sub_matches),
        Some(("build", sub_matches)) => Command::Build(BuildOptions {
            project_dir: sub_matches
                .get_one::<String>("project_dir")
                .unwrap()
                .clone(),
            output_file: sub_matches.get_one::<String>("output_file").cloned(),
        }),
        _ => {
            let _ = command.print_help();
            std::process::exit(0);
        }
    }
}

fn main() {
    env_logger::builder()
        .filter_level(log::LevelFilter::Warn)
        .parse_default_env()
        .init();

    let command = get_args();

    match command {
        Command::Compile(options) => {
            if !handle_compile(options) {
                std::process::exit(1);
            }
        }
        Command::Build(options) => {
            if !handle_build(&options) {
                std::process::exit(1);
            }
        }
        Command::Run { input_file } => handle_run(&input_file),
    }
}
