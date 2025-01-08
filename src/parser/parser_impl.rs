use crate::lexer::token::TokenKind;
use crate::lexer::Lexer;
use crate::parser::ast::{BlockStmt, Expr, Program, Stmt};
use env_logger;
use log::{debug, error, info};
use std::*;

type Result<T> = std::result::Result<T, ParseError>;

#[allow(dead_code)]
#[derive(Debug, Clone)]
enum ParseError {
    ExpectedIdentifier(TokenKind),
    ExpectedAssign(TokenKind),
    ExpectedSemicolon(TokenKind),
    ExpectedColon(TokenKind),
    ExpectedLParen(TokenKind),
    ExpectedRParen(TokenKind),
    ExpectedComma(TokenKind),
    ExpectedLBrace(TokenKind),
    ExpectedRBrace(TokenKind),
    ExpectedRBracket(TokenKind),
    ExpectedLBracket(TokenKind),
    ExpectedToken { expected: TokenKind, got: TokenKind },
}

#[derive(Debug, Clone, PartialEq, PartialOrd, Eq)]
enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    SumMinus,
    ProductDiv,
    Prefix,
    Call,
    Index,
}

impl Precedence {
    fn from_token(t: &TokenKind) -> Precedence {
        match t {
            TokenKind::Equal | TokenKind::NotEq => Precedence::Equals,
            TokenKind::LessThan | TokenKind::GreaterThan => Precedence::LessGreater,
            TokenKind::Plus | TokenKind::Minus => Precedence::SumMinus,
            TokenKind::Asterisk | TokenKind::Div => Precedence::ProductDiv,
            TokenKind::LeftParen | TokenKind::RightParen => Precedence::Call,
            _ => Precedence::Lowest,
        }
    }
}

pub struct Parser {
    lexer: Lexer,
    curr_token: TokenKind,
    peek_token: TokenKind,
    errors: Vec<ParseError>,
}

#[allow(dead_code)]
impl Parser {
    pub fn new(lexer: Lexer) -> Self {
        let mut p = Self {
            lexer,
            curr_token: TokenKind::Eof,
            peek_token: TokenKind::Eof,
            errors: vec![],
        };
        p.next_token();
        p.next_token();
        p
    }
    fn next_token(&mut self) {
        self.curr_token = mem::replace(&mut self.peek_token, self.lexer.next_token());
    }
    fn expect_peek<F>(&mut self, token: TokenKind, parser_error: F) -> Result<()>
    where
        F: Fn(TokenKind) -> ParseError,
    {
        let peek = self.peek_token.clone();
        if peek == token {
            self.next_token();
            Ok(())
        } else {
            Err(parser_error(peek))
        }
    }
    pub fn parse_program(&mut self) -> Program {
        let mut program = Program { statements: vec![] };
        while self.curr_token != TokenKind::Eof {
            match self.parse_statement() {
                Ok(stmt) => program.statements.push(stmt),
                Err(error) => self.errors.push(error),
            }
            self.next_token();
        }
        program
    }
    fn parse_statement(&mut self) -> Result<Stmt> {
        match &self.curr_token {
            TokenKind::Let => self.parse_let_statement(),
            TokenKind::Return => self.parse_return_statement(),
            _ => self.parse_expression_statement(),
        }
    }
    fn parse_expression_statement(&mut self) -> Result<Stmt> {
        let stmt = Stmt::Expression(self.parse_expression(Precedence::Lowest)?);
        if self.peek_token == TokenKind::Semicolon {
            self.next_token();
        }
        Ok(stmt)
    }

    fn parse_call_arguments(&mut self) -> Result<Vec<Expr>> {
        let mut args = vec![];

        self.next_token();
        if self.curr_token == TokenKind::RightParen {
            return Ok(args);
        }
        args.push(self.parse_expression(Precedence::Lowest)?);
        while self.peek_token == TokenKind::Comma {
            self.next_token();
            self.next_token();
            args.push(self.parse_expression(Precedence::Lowest)?);
        }
        self.expect_peek(TokenKind::RightParen, ParseError::ExpectedRParen)?;
        Ok(args)
    }
    //    fn parse_function_expression(&mut self) -> Result<Expr> {
    //    debug!("Starting function expression parse");
    //
    //    // Skip the 'fn' token
    //    self.expect_peek(TokenKind::LeftParen, ParseError::ExpectedLParen)?;
    //
    //    // Parse parameters
    //    let parameters = self.parse_function_parameters()?;
    //
    //    // Parse function body
    //    self.expect_peek(TokenKind::LeftBrace, ParseError::ExpectedLBrace)?;
    //    let body = self.parse_block_statement()?;
    //
    //    // Parse immediate call arguments
    //    let arguments = if self.peek_token == TokenKind::LeftParen {
    //        self.next_token();
    //        self.parse_call_arguments()?
    //    } else {
    //        vec![]
    //    };
    //
    //    debug!("Function expression complete");
    //    Ok(Expr::ImmediateCall(parameters, body, arguments))
    //}

    fn parse_function_expression(&mut self) -> Result<Expr> {
        debug!("Starting function expression parse");
        debug!("Current token: {:?}", self.curr_token);

        let parameters = if self.peek_token == TokenKind::LeftParen {
            self.next_token();
            self.parse_function_parameters()?
        } else {
            return Err(ParseError::ExpectedLParen(self.peek_token.clone()));
        };

        // Parse function body
        if self.peek_token == TokenKind::LeftBrace {
            self.next_token(); // consume left brace
            let body = self.parse_block_statement()?;

            Ok(Expr::Function(parameters, body))
        } else {
            Err(ParseError::ExpectedLBrace(self.peek_token.clone()))
        }
    }

    fn parse_prefix(&mut self) -> Result<Expr> {
        match &self.curr_token {
            TokenKind::Identifier(s) => Ok(Expr::Identifier(s.clone())),
            TokenKind::Print => Ok(Expr::Identifier("print".to_string())),
            TokenKind::IntegerLiteral(i) => Ok(Expr::Integer(*i)),
            TokenKind::String(s) => Ok(Expr::Str(s.clone())),
            TokenKind::True => Ok(Expr::Boolean(true)),
            TokenKind::False => Ok(Expr::Boolean(false)),
            TokenKind::LeftBracket => self.parse_array_literal(),
            TokenKind::Bang | TokenKind::Minus => self.parse_prefix_expression(),
            TokenKind::LeftParen => self.parse_grouped_expression(),
            TokenKind::LeftBrace => self.parse_map_literal(),
            TokenKind::If => self.parse_if_expression(),
            TokenKind::Fn => self.parse_function_expression(),
            t => Err(ParseError::ExpectedToken {
                expected: TokenKind::Identifier("".to_string()),
                got: t.clone(),
            }),
        }
    }

    fn parse_function_literal(&mut self) -> Result<Expr> {
        self.expect_peek(TokenKind::LeftParen, ParseError::ExpectedLParen)?;

        let parameters = self.parse_function_parameters()?;

        self.expect_peek(TokenKind::LeftBrace, ParseError::ExpectedLBrace)?;

        let body = self.parse_block_statement()?;

        Ok(Expr::Function(parameters, body))
    }
    fn parse_expression(&mut self, prec: Precedence) -> Result<Expr> {
        debug!("Parsing express with Precedence: {:?}", prec);

        let mut left_exp = self.parse_prefix()?;

        debug!("Initial left expression: {:?}", left_exp);

        while self.peek_token != TokenKind::Semicolon
            && self.peek_token != TokenKind::RightParen
            && prec < Precedence::from_token(&self.peek_token)
        {
            debug!("Building infix expression ");
            debug!("Current left: {:?}", left_exp);
            debug!("Peek token: {:?}", self.peek_token);
            left_exp = match Precedence::from_token(&self.peek_token) {
                Precedence::Equals
                | Precedence::LessGreater
                | Precedence::SumMinus
                | Precedence::ProductDiv => {
                    self.next_token();
                    self.parse_infix_expression(Box::new(left_exp))?
                }
                Precedence::Call => {
                    self.next_token();
                    self.parse_call_expression(Box::new(left_exp))?
                }
                Precedence::Index => {
                    self.next_token();
                    self.parse_index_expression(Box::new(left_exp))?
                }
                _ => break,
            };
        }
        debug!("Returning expression: {:?}", left_exp);
        Ok(left_exp)
    }
    fn parse_index_expression(&mut self, left: Box<Expr>) -> Result<Expr> {
        // cur_token: LBracket
        self.next_token();
        let index = Box::new(self.parse_expression(Precedence::Lowest)?);

        self.expect_peek(TokenKind::RightBracket, ParseError::ExpectedRBracket)?;

        Ok(Expr::Index(left, index))
    }
    fn parse_prefix_expression(&mut self) -> Result<Expr> {
        let curr_token = self.curr_token.clone();

        self.next_token();
        let right = self.parse_expression(Precedence::Prefix)?;
        Ok(Expr::Prefix(curr_token, Box::new(right)))
    }
    fn parse_infix_expression(&mut self, left: Box<Expr>) -> Result<Expr> {
        let token = self.curr_token.clone();
        let right = {
            let prec = Precedence::from_token(&token);
            self.next_token();
            let expr = self.parse_expression(prec)?;
            Box::new(expr)
        };
        Ok(Expr::Infix(left, token, right))
    }
    fn parse_call_expression(&mut self, left: Box<Expr>) -> Result<Expr> {
        let args = self.parse_expression_list(TokenKind::RightParen)?;
        Ok(Expr::Call(left, args))
    }

    fn parse_function_parameters(&mut self) -> Result<Vec<Expr>> {
        let mut parameters = vec![];
        self.next_token();
        if self.curr_token == TokenKind::RightParen {
            return Ok(parameters);
        };
        let ident = match &self.curr_token {
            TokenKind::Identifier(s) => Expr::Identifier(s.clone()),
            t => return Err(ParseError::ExpectedIdentifier(t.clone())),
        };
        parameters.push(ident);
        while self.peek_token == TokenKind::Comma {
            self.next_token();
            self.next_token();
            let ident = match &self.curr_token {
                TokenKind::Identifier(s) => Expr::Identifier(s.clone()),
                t => return Err(ParseError::ExpectedIdentifier(t.clone())),
            };
            parameters.push(ident);
        }
        self.expect_peek(TokenKind::RightParen, ParseError::ExpectedRParen)?;
        Ok(parameters)
    }

    fn parse_array_literal(&mut self) -> Result<Expr> {
        let mut elements = vec![];

        self.next_token();

        if self.curr_token == TokenKind::RightBracket {
            return Ok(Expr::Array(elements));
        }

        elements.push(self.parse_expression(Precedence::Lowest)?);

        while self.peek_token == TokenKind::Comma {
            self.next_token();
            self.next_token();
            elements.push(self.parse_expression(Precedence::Lowest)?);
        }

        self.expect_peek(TokenKind::RightBracket, ParseError::ExpectedRBracket)?;

        Ok(Expr::Array(elements))
    }

    fn parse_expression_list(&mut self, end: TokenKind) -> Result<Vec<Expr>> {
        let mut items = vec![];
        self.next_token();
        if self.curr_token != end {
            items.push(self.parse_expression(Precedence::Lowest)?);

            while self.peek_token == TokenKind::Comma {
                self.next_token();
                self.next_token();
                items.push(self.parse_expression(Precedence::Lowest)?);
            }
            self.expect_peek(end.clone(), |got| ParseError::ExpectedToken {
                expected: end.clone(),
                got,
            })?;
        }
        Ok(items)
    }
    fn parse_grouped_expression(&mut self) -> Result<Expr> {
        debug!("Parsing grouped expression");
        debug!("Expression state:");
        debug!("Current token: {:?}", self.curr_token);
        debug!("Peek token: {:?}", self.peek_token);
        self.next_token();
        let exp = self.parse_expression(Precedence::Lowest)?;
        debug!("After expression, looking for right paren");
        debug!("Current token: {:?}", self.curr_token);
        debug!("Peek token: {:?}", self.peek_token);
        self.expect_peek(TokenKind::RightParen, ParseError::ExpectedRParen)?;
        Ok(exp)
    }

    fn parse_map_literal(&mut self) -> Result<Expr> {
        let mut pairs = vec![];
        while self.peek_token != TokenKind::RightBrace {
            self.next_token();
            let key = self.parse_expression(Precedence::Lowest)?;
            self.expect_peek(TokenKind::Colon, ParseError::ExpectedColon)?;
            self.next_token();
            let value = self.parse_expression(Precedence::Lowest)?;
            pairs.push((key, value));
            if self.peek_token == TokenKind::RightBrace {
                break;
            }
            self.expect_peek(TokenKind::Comma, ParseError::ExpectedComma)?;
        }
        self.expect_peek(TokenKind::RightBrace, ParseError::ExpectedRBrace)?;
        Ok(Expr::Hash(pairs))
    }
    fn parse_if_expression(&mut self) -> Result<Expr> {
        self.expect_peek(TokenKind::LeftParen, ParseError::ExpectedLParen)?;
        self.next_token();
        let condition = Box::new(self.parse_expression(Precedence::Lowest)?);
        self.expect_peek(TokenKind::RightParen, ParseError::ExpectedRParen)?;
        self.expect_peek(TokenKind::LeftBrace, ParseError::ExpectedLParen)?;
        let consequence = self.parse_block_statement()?;
        let alternative = if self.peek_token == TokenKind::Else {
            self.next_token();
            let _ = self.expect_peek(TokenKind::LeftBrace, ParseError::ExpectedLBrace);
            Some(self.parse_block_statement()?)
        } else {
            None
        };
        Ok(Expr::If(condition, consequence, alternative))
    }

    fn parse_block_statement(&mut self) -> Result<BlockStmt> {
        let mut block = BlockStmt::new();
        self.next_token();

        while self.curr_token != TokenKind::RightBrace && self.curr_token != TokenKind::Eof {
            let stmt = self.parse_statement()?;
            block.statements.push(stmt);
            self.next_token();
        }
        Ok(block)
    }
    fn parse_let_statement(&mut self) -> Result<Stmt> {
        let identifier: String;
        if let TokenKind::Identifier(ident) = self.peek_token.clone() {
            identifier = ident;
            self.next_token();
        } else {
            return Err(ParseError::ExpectedIdentifier(self.peek_token.clone()));
        }
        self.expect_peek(TokenKind::Let, ParseError::ExpectedAssign)?;
        self.next_token();
        let value = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token == TokenKind::Semicolon {
            self.next_token();
        }
        Ok(Stmt::Let(identifier, value))
    }

    fn parse_return_statement(&mut self) -> Result<Stmt> {
        self.next_token();
        let return_value = self.parse_expression(Precedence::Lowest)?;
        if self.peek_token == TokenKind::Semicolon {
            self.next_token();
        }
        Ok(Stmt::Return(return_value))
    }
    pub fn check_parser_errors(&self) {
        if self.errors.is_empty() {
            return;
        }
        eprintln!("Parser errors = {}", self.errors.len());
        for (i, error) in self.errors.iter().enumerate() {
            eprintln!("\t{}. {:?}", i, error);
        }
    }
}

#[cfg(test)]
mod test_parser {
    use super::*;
    use std::vec;
    #[test]
    fn test_identifier() {
        let input = r"x";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        let expected = vec![Stmt::Expression(Expr::Identifier("x".to_string()))];
        assert_eq!(program.statements, expected);
    }
    #[test]
    fn test_integer() {
        let input = r"7";
        let lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(lexer);
        let program = parser.parse_program();
        parser.check_parser_errors();
        let expected: Vec<Stmt> = vec![Stmt::Expression(Expr::Integer(7))];
        assert_eq!(program.statements, expected);
    }

    #[test]
    fn test_prec() {
        let tests: Vec<(&str, &str)> = vec![
            ("a + b + c", "((a + b) + c)"),
            ("-a * b", "((-a) * b)"),
            (
                "a + b * (x + (y * z + 1))",
                "(a + (b * (x + ((y * z) + 1))))",
            ),
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)"),
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)"),
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))"),
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))"),
            (
                "3 + 4 * 5 == 3 * 1 + 4 * 5",
                "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            ),
            ("true", "true"),
            ("false", "false"),
            ("3 > 5 == false", "((3 > 5) == false)"),
            ("3 < 5 == true", "((3 < 5) == true)"),
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)"),
            ("(5 + 5) * 2", "((5 + 5) * 2)"),
            ("2 / (5 + 5)", "(2 / (5 + 5))"),
            ("-(5 + 5)", "(-(5 + 5))"),
            ("!(true == true)", "(!(true == true))"),
            (
                "print(if (y > x) { 1 } else { 2 })",
                "print(if (y > x) { 1 } else { 2 })",
            ),
            ("fn (x, y) { x + y; }", "fn (x, y) { (x + y) }"),
            ("[1, 2, 3, 4]", "[1, 2, 3, 4]"),
        ];
        for (input, expected) in tests {
            let lexer = Lexer::new(input.to_string());
            let mut parser = Parser::new(lexer);
            let program = parser.parse_program();
            parser.check_parser_errors();
            assert_eq!(program.to_string(), expected.to_string());
        }
    }
}
