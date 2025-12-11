pub mod lexing;
pub mod parser;

pub use lexing::lexer::{lex_program, calculate_position};
pub use lexing::token::{Token, TokenWithPosition, Position};

pub use parser::parser::KFParser;
pub use parser::parser::{parse_program};
