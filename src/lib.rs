pub mod lexing;
pub mod parser;
pub mod codegen;

pub use lexing::lexer::{lex_program, calculate_position};
pub use lexing::token::{Token, TokenWithPosition, Position};

pub use parser::parser::KFParser;
pub use parser::parser::{parse_program};
pub use parser::symbol_analyzer::{extract_symbols, SymbolType, SemanticError};


pub use codegen::tac::{TACGenerator, TACInstruction, instructions_to_string};
