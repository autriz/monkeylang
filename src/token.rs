use std::fmt::Display;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Token {
    Illegal,
    Eof,

    Ident(String),
    Int(String),
    Float(String),
    Hex(String),
    String(String),
    Assign,

    Bang,
    Dash,
    ForwardSlash,
    Asterisk,
    Equal,
    NotEqual,
    LessThan,
    LessThanOrEqual,
    GreaterThan,
    GreaterThanOrEqual,
    
    Plus,
    Comma,
    Semicolon,
    LParen,
    RParen,
    LBrace,
    RBrace,

    Function,
    Let,

    If,
    Else,
    Return,
    True,
    False,
}

impl Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Token::Illegal => write!(f, "Illegal"),
            Token::Eof => write!(f, "Eof"),
            Token::Ident(x) => write!(f, "Ident({})", x),
            Token::Int(x) => write!(f, "Int({})", x),
            Token::Float(x) => write!(f, "Float({})", x),
            Token::Hex(x) => write!(f, "Hex({})", x),
            Token::String(x) => write!(f, "String({})", x),
            Token::Assign => write!(f, "Assign"),
            Token::Bang => write!(f, "Bang"),
            Token::Dash => write!(f, "Dash"),
            Token::ForwardSlash => write!(f, "ForwardSlash"),
            Token::Asterisk => write!(f, "Asterisk"),
            Token::Equal => write!(f, "Equal"),
            Token::NotEqual => write!(f, "NotEqual"),
            Token::LessThan => write!(f, "LessThan"),
            Token::LessThanOrEqual => write!(f, "LessThanOrEqual"),
            Token::GreaterThan => write!(f, "GreaterThan"),
            Token::GreaterThanOrEqual => write!(f, "GreaterThanOrEqual"),
            Token::Plus => write!(f, "Plus"),
            Token::Comma => write!(f, "Comma"),
            Token::Semicolon => write!(f, "Semicolon"),
            Token::LParen => write!(f, "LParen"),
            Token::RParen => write!(f, "RParen"),
            Token::LBrace => write!(f, "LBrace"),
            Token::RBrace => write!(f, "RBrace"),
            Token::Function => write!(f, "Function"),
            Token::Let => write!(f, "Let"),
            Token::If => write!(f, "If"),
            Token::Else => write!(f, "Else"),
            Token::Return => write!(f, "Return"),
            Token::True => write!(f, "True"),
            Token::False => write!(f, "False"),
        }
    }
}

impl Token {
    pub fn is_ident(&self) -> bool {
        match self {
            Self::Ident(_) => true,
            _ => false,
        }        
    }

    pub fn as_literal(&self) -> String {
        match self {
            Token::Illegal => "Illegal".to_string(),
            Token::Eof => "Eof".to_string(),
            Token::Ident(x) => format!("{}", x),
            Token::Int(x) => format!("{}", x),
            Token::Float(x) => format!("{}", x),
            Token::Hex(x) => format!("{}", x),
            Token::String(x) => format!("\"{}\"", x),
            Token::Assign => "=".to_string(),
            Token::Bang => "!".to_string(),
            Token::Dash => "-".to_string(),
            Token::ForwardSlash => "/".to_string(),
            Token::Asterisk => "*".to_string(),
            Token::Equal => "==".to_string(),
            Token::NotEqual => "!=".to_string(),
            Token::LessThan => "<".to_string(),
            Token::LessThanOrEqual => "<=".to_string(),
            Token::GreaterThan => ">".to_string(),
            Token::GreaterThanOrEqual => ">=".to_string(),
            Token::Plus => "+".to_string(),
            Token::Comma => ",".to_string(),
            Token::Semicolon => ";".to_string(),
            Token::LParen => "(".to_string(),
            Token::RParen => ")".to_string(),
            Token::LBrace => "{".to_string(),
            Token::RBrace => "}".to_string(),
            Token::Function => "fn".to_string(),
            Token::Let => "let".to_string(),
            Token::If => "if".to_string(),
            Token::Else => "else".to_string(),
            Token::Return => "return".to_string(),
            Token::True => "true".to_string(),
            Token::False => "false".to_string(),
        }
    }
}