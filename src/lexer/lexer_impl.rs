use crate::lexer::token::TokenKind;
use log::{debug, info};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Lexer {
    input: Vec<char>,
    pos: usize,
    read_pos: usize,
    ch: char,
    length: usize,
}

impl Lexer {
    #[allow(dead_code)]
    pub fn new(input: String) -> Self {
        let length = input.len();
        Lexer {
            input: input.chars().collect(),
            ch: input.chars().next().unwrap(),
            pos: 0,
            read_pos: 1,
            length,
        }
    }

    fn read_char(&mut self) {
        self.ch = match self.input.get(self.read_pos) {
            Some(&char) => {
                debug!("current char {}", char);
                char
            }
            None => '\0',
        };
        self.pos = self.read_pos;
        self.read_pos += 1;
    }

    fn peek(&self) -> char {
        match self.input.get(self.read_pos) {
            Some(&char) => char,
            None => '\0',
        }
    }
    pub fn skip_whitespace(&mut self) {
        while self.ch.is_whitespace() {
            self.read_char();
        }
    }
    pub fn next_token(&mut self) -> TokenKind {
        self.skip_whitespace();
        let tok = match self.ch {
            '=' => {
                if self.peek() == '=' {
                    self.read_char();
                    debug!("next_token under read_char {}", self.ch);
                    TokenKind::Equal
                } else {
                    TokenKind::Assign
                }
            }
            '+' => TokenKind::Plus,
            '-' => TokenKind::Minus,
            '!' => {
                if self.peek() == '=' {
                    self.read_char();
                    debug!("next_token under read_char line 66 {}", self.ch);
                    TokenKind::NotEq
                } else {
                    TokenKind::Bang
                }
            }
            '*' => TokenKind::Asterisk,
            '/' => TokenKind::Div,
            '<' => TokenKind::LessThan,
            '>' => TokenKind::GreaterThan,
            ';' => TokenKind::Semicolon,
            '(' => TokenKind::LeftParen,
            ')' => TokenKind::RightParen,
            '[' => TokenKind::LeftBracket,
            ']' => TokenKind::RightBracket,
            ':' => TokenKind::Colon,
            '{' => TokenKind::LeftBrace,
            '}' => TokenKind::RightBrace,
            ',' => TokenKind::Comma,
            c if c.is_letter() => {
                let s = self.read_ident();
                debug!("next_token under read_ident line 84 {}", self.ch);
                return self.lookup_ident(s);
            }
            c if c.is_ascii_digit() => {
                let i = self.read_number();
                debug!("next_token under read_number line 89 {}", self.ch);
                return TokenKind::IntegerLiteral(i);
            }
            c if c == '"' => {
                let s = self.read_string();
                debug!("next_char under read_number line 94 {}", self.ch);
                TokenKind::String(s)
            }
            '\0' => TokenKind::Eof,
            _ => TokenKind::Unknown,
        };
        self.read_char();
        debug!("next_char under read_number line 101 {} ", self.ch);
        tok
    }

    fn lookup_ident(&self, s: String) -> TokenKind {
        match s.as_str() {
            "fn" => TokenKind::Fn,
            "let" => TokenKind::Let,
            "true" => TokenKind::True,
            "false" => TokenKind::False,
            "if" => TokenKind::If,
            "else" => TokenKind::Else,
            "then" => TokenKind::Then,
            "print" => TokenKind::Print,
            "return" => TokenKind::Return,
            _ => TokenKind::Identifier(s),
        }
    }
    fn read_ident(&mut self) -> String {
        let start = self.pos;
        while self.ch.is_letter() {
            self.read_char();
        }
        self.input[start..self.pos].iter().collect()
    }

    fn read_number(&mut self) -> i32 {
        let start = self.pos;
        while self.ch.is_ascii_digit() {
            self.read_char();
        }
        self.input[start..self.pos]
            .iter()
            .collect::<String>()
            .parse()
            .unwrap()
    }
    fn read_string(&mut self) -> String {
        let start = self.pos + 1;
        loop {
            self.read_char();
            if self.ch == '"' || self.ch == '\0' {
                break;
            }
        }
        self.input[start..self.pos].iter().collect()
    }
}
impl Iterator for Lexer {
    type Item = TokenKind;
    fn next(&mut self) -> Option<Self::Item> {
        if self.pos <= self.input.len() {
            Some(self.next_token())
        } else {
            None
        }
    }
}

trait IsLetter {
    fn is_letter(&self) -> bool;
}

impl IsLetter for char {
    fn is_letter(&self) -> bool {
        self.is_alphabetic() || *self == '_'
    }
}
