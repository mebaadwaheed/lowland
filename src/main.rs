use clap::{Parser, Subcommand};
use colored::Colorize;
use lowland::interpreter::Interpreter;
use rustyline::error::ReadlineError;
use rustyline::DefaultEditor;
use std::path::PathBuf;

/// Lowland programming language CLI
#[derive(Parser)]
#[command(name = "low")]
#[command(about = "Lowland programming language interpreter", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,
}

#[derive(Subcommand)]
enum Commands {
    /// Run a Lowland script file
    Run {
        /// The script file to run
        #[arg(value_name = "FILE")]
        file: PathBuf,
    },
    /// Start an interactive REPL
    Start,
    /// Build a Lowland program
    Build {
        /// The script file to build
        #[arg(value_name = "FILE")]
        file: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();
    let mut interpreter = Interpreter::new();

    match &cli.command {
        Some(Commands::Run { file }) => {
            run_file(file, &mut interpreter);
        }
        Some(Commands::Build { file: _ }) => {
            println!("{}", "Building is not implemented yet".yellow());
            // TODO: Implement build functionality
        }
        Some(Commands::Start) => {
            start_repl(&mut interpreter);
        }
        None => {
            start_repl(&mut interpreter);
        }
    }
}

fn run_file(file_path_buf: &PathBuf, interpreter: &mut Interpreter) {
    let extension = file_path_buf.extension().and_then(|e| e.to_str());
    if extension != Some("lln") && extension != Some("lowl") {
        eprintln!(
            "{} {}",
            "Error:".red().bold(),
            "File must have .lln or .lowl extension"
        );
        return;
    }
    
    let file_path_str = match file_path_buf.to_str() {
        Some(s) => s,
        None => {
            eprintln!("{} Invalid file path provided.", "Error:".red().bold());
            return;
        }
    };

    println!("Executing file: {}", file_path_buf.display());
    match interpreter.interpret_file(file_path_str) {
        Ok(_) => {}
        Err(err) => {
            eprintln!("{} {}", "Error:".red().bold(), err);
        }
    }
}

fn start_repl(interpreter: &mut Interpreter) {
    println!("{}", "Lowland Programming Language".green().bold());
    println!("{}", "Type 'exit' to quit".italic());
    println!();

    let mut rl = DefaultEditor::new().unwrap();

    loop {
        match rl.readline(">> ") {
            Ok(line) => {
                if line.trim() == "exit" {
                    break;
                }

                rl.add_history_entry(&line).unwrap();

                match interpreter.interpret_repl_input(&line) {
                    Ok(_) => {}
                    Err(err) => {
                        eprintln!("{} {}", "Error:".red().bold(), err);
                    }
                }
            }
            Err(ReadlineError::Interrupted) | Err(ReadlineError::Eof) => {
                break;
            }
            Err(err) => {
                eprintln!("{} {}", "Error:".red().bold(), err);
                break;
            }
        }
    }
}
