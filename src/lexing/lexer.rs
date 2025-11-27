
use regex::Regex;
use crate::lexing::token::Token;
use crate::lexing::token::Position;
use crate::lexing::token::TokenWithPosition;


pub fn lex_program(program: &str) -> Vec<TokenWithPosition> {
    let current_input = program;
    let tokens = [
        "Plusplus",
        "Minusminus",

        "Utl",
        "Of",
        "Onoff",
        "On",
        "Wii",
        "Mote",
        "Dec",
        "Kf",
        "Ont",
        "Uont",
        "Michi",
        "Ntr",
        "Chip",
        "Yes",
        "No",
        "Yesorno",
        "Next",
        "Ash",
        "Brokie",
        "Send",
        "Tnirp",
        "Tupni",
        "Join",


        "Is",
        "Plus",
        "Mult",
        "Minus",
        "By",
        "Mod",
        "And",
        "Or",
        "Nah",
        "Great",
        "Lesst",
        "Eq",
        "Noteq",

        "FloatLiteral",
        "IntegerLiteral",
        "CharLiteral",
        "StringLiteral",

        "Identifier",

        "Semicolon",
        "LeftParen",
        "RightParen",
        "LeftBrace",
        "RightBrace",
        "LeftBracket",
        "RightBracket",
    ];

    let mut match_vec: Vec<(&str, usize, usize)> = Vec::new();

    for token in tokens.iter() {
        let token_regex = Token::get_token_regex(token);
        let re = Regex::new(token_regex.as_str()).unwrap();
        let matched = re.find_iter(current_input);
        let all_matches = matched.collect::<Vec<_>>();
        
        if all_matches.len() == 0 {
            continue;
        }

        // Regex returns the start and end of the match, so we save that also
        for m in all_matches.iter() {
            match_vec.push((token, m.start(), m.end()));
        }

    }

    // Sorting by position and then by length
    match_vec.sort_by(|a, b| a.1.cmp(&b.1).then_with(|| (b.2 - b.1).cmp(&(a.2 - a.1))));


    let mut token_vec: Vec<TokenWithPosition> = Vec::new();
    let mut last_end = 0;
    for m in match_vec.iter() {
        // if already got the token at this position, skip
        if m.1 < last_end {
            continue;
        }
        let position = calculate_position(program, m.1);
        let token = Token::get_token(m.0, Some(&current_input[m.1..m.2]));
        token_vec.push(TokenWithPosition { token, position });
    }

    token_vec
}


// It calculates the position based on the start that is byte_offset
fn calculate_position(input: &str, byte_offset: usize) -> Position {
    let mut line = 1;
    let mut col = 1;

    for (i, ch) in input.chars().enumerate() {
        if i >= byte_offset {
            break;
        }

        if ch == '\n' {
            line += 1;
            col = 1;
        } else {
            col += 1;
        }
    }

    Position { line, col }
}
