
#[derive(Debug)]
pub struct Position {
    pub line: usize,
    pub col: usize,
}

#[derive(Debug)]
pub struct TokenWithPosition {
    pub token: Token,
    pub position: Position,
}


#[derive(Debug)]
pub enum Token{
    // Keywords
    Utl(String),
    Of(String),
    Onoff(String),
    On(String),
    Wii(String),
    Mote(String),
    Dec(String),
    Kf(String),
    Ont(String),
    Uont(String),
    Michi(String),
    Ntr(String),
    Chip(String),
    Yes(String),
    No(String),
    Yesorno(String),
    Next(String),
    Ash(String),
    Brokie(String),
    Send(String),
    Tnirp(String),
    Tupni(String),
    Join(String),

    // Identifiers
    Identifier(String),

    // Operators
    Is(String),
    Plus(String),
    Plusplus(String),
    Mult(String),
    Minus(String),
    Minusminus(String),
    By(String),
    Mod(String),

    // Logical Operators
    And(String),
    Or(String),
    Nah(String),
    Great(String),
    Lesst(String),
    Eq(String),
    Noteq(String),

    // Literals
    IntegerLiteral(i32),
    FloatLiteral(f64),
    CharLiteral(char),
    StringLiteral(String),

    // Punctuation
    Semicolon(String),
    LeftParen(String),
    RightParen(String),
    LeftBrace(String),
    RightBrace(String),
    LeftBracket(String),
    RightBracket(String),
}


impl Token {
    pub fn get_token(token_type: &str, value: Option<&str>) -> Token {
        match token_type {
            "Utl" => Token::Utl("utl".to_string()),
            "Of" => Token::Of("of".to_string()),
            "Onoff" => Token::Onoff("onoff".to_string()),
            "On" => Token::On("on".to_string()),
            "Wii" => Token::Wii("wii".to_string()),
            "Mote" => Token::Mote("mote".to_string()),
            "Dec" => Token::Dec("dec".to_string()),
            "Kf" => Token::Kf("kf".to_string()),
            "Ont" => Token::Ont("ont".to_string()),
            "Uont" => Token::Uont("uont".to_string()),
            "Michi" => Token::Michi("michi".to_string()),
            "Ntr" => Token::Ntr("ntr".to_string()),
            "Chip" => Token::Chip("chip".to_string()),
            "Yes" => Token::Yes("yes".to_string()),
            "No" => Token::No("no".to_string()),
            "Yesorno" => Token::Yesorno("yesorno".to_string()),
            "Next" => Token::Next("next".to_string()),
            "Ash" => Token::Ash("ash".to_string()),
            "Brokie" => Token::Brokie("brokie".to_string()),
            "Send" => Token::Send("send".to_string()),
            "Tnirp" => Token::Tnirp("tnirp".to_string()),
            "Tupni" => Token::Tupni("tupni".to_string()),
            "Join" => Token::Join("join".to_string()),

            "Identifier" => Token::Identifier(value.unwrap().to_string()),

            "Is" => Token::Plus("is".to_string()),
            "Plus" => Token::Plus("plus".to_string()),
            "Plusplus" => Token::Plusplus("plusplus".to_string()),
            "Mult" => Token::Mult("mult".to_string()),
            "Minus" => Token::Minus("minus".to_string()),
            "Minusminus" => Token::Minusminus("minusminus".to_string()),
            "By" => Token::By("by".to_string()),
            "Mod" => Token::Mod("mod".to_string()),
            "And" => Token::And("and".to_string()),
            "Or" => Token::Or("or".to_string()),
            "Nah" => Token::Nah("nah".to_string()),
            "Great" => Token::Great("great".to_string()),
            "Lesst" => Token::Lesst("lesst".to_string()),
            "Eq" => Token::Eq("eq".to_string()),
            "Noteq" => Token::Noteq("noteq".to_string()),
            "IntegerLiteral" => Token::IntegerLiteral(value.unwrap().parse::<i32>().unwrap()),
            "FloatLiteral" => Token::FloatLiteral(value.unwrap().parse::<f64>().unwrap()),
            "CharLiteral" => Token::CharLiteral(value.unwrap().chars().next().unwrap()),
            "StringLiteral" => Token::StringLiteral(value.unwrap().to_string()),
            "Semicolon" => Token::Semicolon(";".to_string()),
            "LeftParen" => Token::LeftParen("(".to_string()),
            "RightParen" => Token::RightParen(")".to_string()),
            "LeftBrace" => Token::LeftBrace("{".to_string()),
            "RightBrace" => Token::RightBrace("}".to_string()),
            "LeftBracket" => Token::LeftBracket("[".to_string()),
            "RightBracket" => Token::RightBracket("]".to_string()),
            _ => panic!("Invalid token type: {}", token_type),
        }
    }


    pub fn get_token_regex(token_type: &str) -> String {
        match token_type {
            "Utl" => r"utl",
            "Of" => r"of",
            "Onoff" => r"onoff",
            "On" => r"on",
            "Wii" => r"wii",
            "Mote" => r"mote",
            "Dec" => r"dec",
            "Kf" => r"kf",
            "Ont" => r"ont",
            "Uont" => r"uont",
            "Michi" => r"michi",
            "Ntr" => r"ntr",
            "Chip" => r"chip",
            "Yes" => r"yes",
            "No" => r"no",
            "Yesorno" => r"yesorno",
            "Next" => r"next",
            "Ash" => r"ash",
            "Brokie" => r"brokie",
            "Send" => r"send",
            "Tnirp" => r"tnirp",
            "Tupni" => r"tupni",
            "Join" => r"join",

            "Identifier" => r"[a-zA-Z_][a-zA-Z0-9_]*",

            "Is" => r"is",
            "Plus" => r"plus",
            "Plusplus" => r"plusplus",
            "Mult" => r"mult",
            "Minus" => r"minus",
            "Minusminus" => r"minusminus",
            "By" => r"by",
            "Mod" => r"mod",
            "And" => r"and",
            "Or" => r"or",
            "Nah" => r"nah",
            "Great" => r"great",
            "Lesst" => r"lesst",
            "Eq" => r"eq",
            "Noteq" => r"noteq",

            "IntegerLiteral" => r"\d+",
            "FloatLiteral" => r"\d+\.\d+",
            "CharLiteral" => r"'.'",
            "StringLiteral" => r#"\".*\""#,

            "Semicolon" => r";",
            "LeftParen" => r"\(",
            "RightParen" => r"\)",
            "LeftBrace" => r"\{",
            "RightBrace" => r"\}",
            "LeftBracket" => r"\[",
            "RightBracket" => r"\]",
            _ => panic!("Invalid token type: {}", token_type),
        }.to_string()
    }
}
