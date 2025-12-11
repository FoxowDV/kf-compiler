
#[derive(Debug, Clone, PartialEq)]
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
    Off(String),
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

    // Errores
    Error(String),
}


impl Token {
    pub fn get_token(token_type: &str, value: Option<&str>) -> Token {
        match token_type {
            "Utl" => Token::Utl("utl".to_string()),
            "Off" => Token::Off("off".to_string()),
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

            "Is" => Token::Is("is".to_string()),
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
            _ => Token::Error("Lexema invalido".to_string()),
        }
    }


    pub fn get_token_regex(token_type: &str) -> String {
        match token_type {
            "Utl" => r"\butl\b",
            "Off" => r"\boff\b",
            "Onoff" => r"\bonoff\b",
            "On" => r"\bon\b",
            "Wii" => r"\bwii\b",
            "Mote" => r"\bmote\b",
            "Dec" => r"\bdec\b",
            "Kf" => r"\bkf\b",
            "Ont" => r"\bont\b",
            "Uont" => r"\buont\b",
            "Michi" => r"\bmichi\b",
            "Ntr" => r"\bntr\b",
            "Chip" => r"\bchip\b",
            "Yes" => r"\byes\b",
            "No" => r"\bno\b",
            "Yesorno" => r"\byesorno\b",
            "Next" => r"\bnext\b",
            "Ash" => r"\bash\b",
            "Brokie" => r"\bbrokie\b",
            "Send" => r"\bsend\b",
            "Tnirp" => r"\btnirp\b",
            "Tupni" => r"\btupni\b",
            "Join" => r"\bjoin\b",

            "Identifier" => r"\b[a-zA-Z_][a-zA-Z0-9_]*\b",

            "Is" => r"\bis\b",
            "Plus" => r"\bplus\b",
            "Plusplus" => r"\bplusplus\b",
            "Mult" => r"\bmult\b",
            "Minus" => r"\bminus\b",
            "Minusminus" => r"\bminusminus\b",
            "By" => r"\bby\b",
            "Mod" => r"\bmod\b",
            "And" => r"\band\b",
            "Or" => r"\bor\b",
            "Nah" => r"\bnah\b",
            "Great" => r"\bgreat\b",
            "Lesst" => r"\blesst\b",
            "Eq" => r"\beq\b",
            "Noteq" => r"\bnoteq\b",

            "IntegerLiteral" => r"\b\d+\b",
            "FloatLiteral" => r"\b\d+\.\d+\b",
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


    pub fn name(&self) -> &str {
        match self {
            Token::Utl(_) => "Utl",
            Token::Off(_) => "Off",
            Token::Onoff(_) => "Onoff",
            Token::On(_) => "On",
            Token::Wii(_) => "Wii",
            Token::Mote(_) => "Mote",
            Token::Dec(_) => "Dec",
            Token::Kf(_) => "Kf",
            Token::Ont(_) => "Ont",
            Token::Uont(_) => "Uont",
            Token::Michi(_) => "Michi",
            Token::Ntr(_) => "Ntr",
            Token::Chip(_) => "Chip",
            Token::Yes(_) => "Yes",
            Token::No(_) => "No",
            Token::Yesorno(_) => "Yesorno",
            Token::Next(_) => "Next",
            Token::Ash(_) => "Ash",
            Token::Brokie(_) => "Brokie",
            Token::Send(_) => "Send",
            Token::Tnirp(_) => "Tnirp",
            Token::Tupni(_) => "Tupni",
            Token::Join(_) => "Join",
            Token::Identifier(_) => "Identifier",
            Token::Is(_) => "Is",
            Token::Plus(_) => "Plus",
            Token::Plusplus(_) => "Plusplus",
            Token::Mult(_) => "Mult",
            Token::Minus(_) => "Minus",
            Token::Minusminus(_) => "Minusminus",
            Token::By(_) => "By",
            Token::Mod(_) => "Mod",
            Token::And(_) => "And",
            Token::Or(_) => "Or",
            Token::Nah(_) => "Nah",
            Token::Great(_) => "Great",
            Token::Lesst(_) => "Lesst",
            Token::Eq(_) => "Eq",
            Token::Noteq(_) => "Noteq",
            Token::IntegerLiteral(_) => "IntegerLiteral",
            Token::FloatLiteral(_) => "FloatLiteral",
            Token::CharLiteral(_) => "CharLiteral",
            Token::StringLiteral(_) => "StringLiteral",
            Token::Semicolon(_) => "Semicolon",
            Token::LeftParen(_) => "LeftParen",
            Token::RightParen(_) => "RightParen",
            Token::LeftBrace(_) => "LeftBrace",
            Token::RightBrace(_) => "RightBrace",
            Token::LeftBracket(_) => "LeftBracket",
            Token::RightBracket(_) => "RightBracket",
            _ => "Error",
        }
    }
    
    pub fn value(&self) -> String {
        match self {
            Token::Utl(s) | Token::Off(s) | Token::Onoff(s) | Token::On(s) |
            Token::Wii(s) | Token::Mote(s) | Token::Dec(s) | Token::Kf(s) |
            Token::Ont(s) | Token::Uont(s) | Token::Michi(s) | Token::Ntr(s) |
            Token::Chip(s) | Token::Yes(s) | Token::No(s) | Token::Yesorno(s) |
            Token::Next(s) | Token::Ash(s) | Token::Brokie(s) | Token::Send(s) |
            Token::Tnirp(s) | Token::Tupni(s) | Token::Join(s) |
            Token::Identifier(s) | Token::Is(s) |Token::Plus(s) | Token::Plusplus(s) |
            Token::Mult(s) | Token::Minus(s) | Token::Minusminus(s) |
            Token::By(s) | Token::Mod(s) | Token::And(s) | Token::Or(s) |
            Token::Nah(s) | Token::Great(s) | Token::Lesst(s) | Token::Eq(s) |
            Token::Noteq(s) | Token::Semicolon(s) | Token::LeftParen(s) |
            Token::RightParen(s) | Token::LeftBrace(s) | Token::RightBrace(s) |
            Token::LeftBracket(s) | Token::RightBracket(s) | Token::StringLiteral(s) | Token::Error(s) => s.clone(),
            Token::IntegerLiteral(i) => i.to_string(),
            Token::FloatLiteral(f) => f.to_string(),
            Token::CharLiteral(c) => c.to_string(),
        }
    }
}
