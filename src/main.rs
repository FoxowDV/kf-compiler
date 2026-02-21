mod lexing;
mod parser;

//use lexing::lexer::lex_program;
use parser::parser::parse_program;
use parser::symbol_analyzer::extract_symbols;
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

    //let tokens = lex_program(&program);

    //for token in tokens.iter() {
    //    println!("{:?}", token);
    //}

    let parsed = match parse_program(&program) {
        Ok(p) => p,
        Err(e) => {
            let _ = dbg!(&e);
            return;
        }
    };
    let _ = dbg!(&parsed);
    //let symbol_table = extract_symbols(&parsed);
    //let _ = dbg!(&symbol_table);

}
