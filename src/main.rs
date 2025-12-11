mod lexing;
mod parser;

use lexing::lexer::lex_program;
use parser::parser::parse_program;
use std::env;
use std::fs;
use std::process;

fn main() {
    let args: Vec<String> = env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: {} <filename>", args[0]);
        process::exit(1);
    }
    
    let filename = &args[1];

    let program = fs::read_to_string(filename)
        .unwrap_or_else(|err| {
            eprintln!("Error reading file '{}': {} ", filename, err);
            process::exit(1);
        });

    let tokens = lex_program(&program);

    for token in tokens.iter() {
        println!("{:?}", token);
    }

    let parsed = parse_program(&program);
    dbg!(parsed);
}
