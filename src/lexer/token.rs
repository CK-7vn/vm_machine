use std::str::FromStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenKind {
    Print,
    If,
    Else,
    Then,
    Let,
    Fn,
    True,
    False,
    Return,

    IntegerLiteral(i32),
    String(String),
    Identifier(String),

    Plus,
    Equal,
    Minus,
    Div,
    Asterisk,
    Assign,

    Semicolon,
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Colon,
    Comma,
    Bang,

    GreaterThan,
    LessThan,
    NotEq,
    Eof,
    Unknown,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Span {
    start: usize,
    end: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Token {
    kind: TokenKind,
    span: Span,
}

impl FromStr for TokenKind {
    type Err = ();
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "print" => Ok(TokenKind::Print),
            "if" => Ok(TokenKind::If),
            "else" => Ok(TokenKind::Else),
            "then" => Ok(TokenKind::Then),
            "let" => Ok(TokenKind::Let),
            "fn" => Ok(TokenKind::Fn),
            "true" => Ok(TokenKind::True),
            "false" => Ok(TokenKind::False),
            "return" => Ok(TokenKind::Return),
            c if c.chars().all(char::is_numeric) => Ok(TokenKind::IntegerLiteral(
                c.parse::<i32>().expect("Unabled to parse as i32"),
            )),
            c if is_valid_identifier(c) => Ok(TokenKind::Identifier(c.to_string())),
            "+" => Ok(TokenKind::Plus),
            "*" => Ok(TokenKind::Asterisk),
            "=" => Ok(TokenKind::Assign),
            "==" => Ok(TokenKind::Equal),
            "!=" => Ok(TokenKind::NotEq),
            "!" => Ok(TokenKind::Bang),
            ";" => Ok(TokenKind::Semicolon),
            ":" => Ok(TokenKind::Colon),
            "(" => Ok(TokenKind::LeftParen),
            ")" => Ok(TokenKind::RightParen),
            "{" => Ok(TokenKind::LeftBrace),
            "}" => Ok(TokenKind::RightBrace),
            ">" => Ok(TokenKind::GreaterThan),
            "<" => Ok(TokenKind::LessThan),
            "-" => Ok(TokenKind::Minus),
            "/" => Ok(TokenKind::Div),
            "[" => Ok(TokenKind::LeftBracket),
            "]" => Ok(TokenKind::RightBracket),
            "," => Ok(TokenKind::Comma),
            "EOF" => Ok(TokenKind::Eof),
            _ => Ok(TokenKind::Unknown),
        }
    }
}

fn is_valid_identifier(identifier: &str) -> bool {
    identifier
        .chars()
        .all(|ch| ch.is_ascii_digit() || ch.is_alphanumeric() || ch == '_' || ch == '-')
}
