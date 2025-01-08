mod lexer;
mod parser;
use ctor::ctor;
use env_logger;
use std::*;

#[cfg(test)]
mod tests {
    use super::*;

    #[ctor]
    fn init() {
        env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("debug")).init();
    }
}

use crate::parser::parser_impl::Parser;
use std::io::{self, Write};

use crate::lexer::Lexer;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    let mut input = String::new();
    env_logger::init();

    loop {
        print!("> ");
        io::stdout().flush().expect("Failed to flush stdout");

        input.clear();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read line");

        let input = input.trim();
        if input == "exit" {
            break;
        }

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer.clone());

        println!("Tokens:");
        while let Some(token) = lexer.next() {
            println!("{:?}", token);
        }
    }

    println!("Goodbye!");
}
