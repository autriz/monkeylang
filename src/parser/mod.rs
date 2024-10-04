mod error;
pub use error::{Error, Result};

use crate::{
    ast::{Expression, Program, Statement}, lexer::Lexer, token::Token
};

pub trait Parse<'a>
    where Self: Sized,
{
    fn parse(parser: &mut Parser<'a>, precedence: Option<Precedence>) -> Option<Self>;
}

pub trait InfixParse<'a>
    where Self: Sized,
{
    fn parse(parser: &mut Parser<'a>, left: Expression, precedence: Option<Precedence>) -> Option<Self>;
}

pub struct Parser<'a> {
    lexer: &'a mut Lexer,
    pub current_token: Token,
    pub peek_token: Token,
    pub errors: Vec<Error>,
}

impl<'a> Parser<'a> {
    pub fn new(lexer: &'a mut Lexer) -> Result<Self> {
        let current_token = lexer.next_token()?;
        let peek_token = lexer.next_token()?;

        Ok(Self {
            lexer,
            current_token,
            peek_token,
            errors: vec![],
        })
    }

    pub fn step(&mut self) -> Result<()> {
        self.current_token = self.lexer.next_token()?;
        std::mem::swap(&mut self.current_token, &mut self.peek_token);

        Ok(())
    }

    pub fn parse(&mut self) -> Program {
        let mut statements = vec![];

        while self.current_token != Token::Eof {
            match Statement::parse(self, None) {
                Some(stmt) => statements.push(stmt),
                None => {}
            }

            let _ = self.step();
        }

        Program { statements }
    }

    pub fn current_precedence(&self) -> Precedence {
        Precedence::from(&self.current_token)
    }

    pub fn peek_precedence(&self) -> Precedence {
        Precedence::from(&self.peek_token)
    }

    pub fn skip_to_semicolon(&mut self) {
        while self.current_token != Token::Semicolon && self.current_token != Token::Eof {
            let _ = self.step();
        }
    }

    pub fn expect_peek(&mut self, token: Token) -> bool {
        if self.peek_token == token {
            let _ = self.step();

            return true;
        } else {
            self.errors.push(Error::UnexpectedToken(self.peek_token.to_owned()));
            return false;
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, PartialOrd)]
pub enum Precedence {
    Lowest,
    Equals,
    LessGreater,
    Sum,
    Product,
    Prefix,
    Call,
}

impl From<&Token> for Precedence {
    fn from(value: &Token) -> Self {
        match value {
            Token::Equal | Token::NotEqual => Self::Equals,
            Token::LessThan | Token::GreaterThan | Token::LessThanOrEqual | Token::GreaterThanOrEqual => Self::LessGreater,
            Token::Plus | Token::Dash => Self::Sum,
            Token::ForwardSlash | Token::Asterisk => Self::Product,
            Token::LParen => Self::Call,
            _ => Self::Lowest,
        }
    }
}

#[cfg(test)]
mod test {
    use crate::{block_stmt, bool, expr, expr_stmt, float, hex, ident, infix, int, let_stmt, lexer::Lexer, prefix, return_stmt, string};

    use super::Parser;

    #[test]
    fn test_let_statements() {
        let input = r#"
            let x1 = 10;
            let y = 15;
            let z = 10.54;
            let w = 0xF;
            let foobar = 838383;
        "#;

        let expected = vec![
            let_stmt!("x1" => int!(10)),
            let_stmt!("y" => int!(15)),
            let_stmt!("z" => float!(10.54)),
            let_stmt!("w" => hex!(0xF)),
            let_stmt!("foobar" => int!(838383)),
        ];

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer).unwrap();

        let program = parser.parse();

        assert_eq!(parser.errors.len(), 0, "Parser has {} errors.\n{:?}", parser.errors.len(), parser.errors);

        assert_eq!(program.statements.len(), expected.len(), "Program has not enough statements, expected {}, got {}", expected.len(), program.statements.len());

        expected
            .iter()
            .zip(program.statements.iter())
            .map(|(expected, actual)| 
                assert_eq!(expected, actual, "Unexpected statement. Expected {expected:?}, got {actual:?}")
            )
            .count();
    }

    #[test]
    fn test_return_statements() {
        let input = r#"
            return 5;
            return 10;
            return 993322;
        "#;

        let expected = [
            return_stmt!(int!(5)),
            return_stmt!(int!(10)),
            return_stmt!(int!(993322)),
        ];

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer).unwrap();

        let program = parser.parse();

        assert_eq!(parser.errors.len(), 0, "Parser has {} errors.\n{:?}", parser.errors.len(), parser.errors);

        assert_eq!(program.statements.len(), expected.len(), "Program has not enough statements, expected {}, got {}", expected.len(), program.statements.len());

        expected
            .iter()
            .zip(program.statements.iter())
            .map(|(expected, actual)| 
                assert_eq!(expected, actual, "Unexpected statement. Expected {expected:?}, got {actual:?}")
            )
            .count();
    }

    #[test]
    fn test_block_statements() {
        let input = r#"
            { 
                let a = 5; 
                let b = 10; 
            }
            { 
                let a = 5; 
                let b = 10;
            }
        "#;

        let expected = vec![
            block_stmt!(
                let_stmt!("a" => int!(5));
                let_stmt!("b" => int!(10));
            ),
            block_stmt!(
                let_stmt!("a" => int!(5));
                let_stmt!("b" => int!(10));
            )
        ];

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer).unwrap();

        let program = parser.parse();

        assert_eq!(parser.errors.len(), 0, "Parser has {} errors.\n{:?}", parser.errors.len(), parser.errors);

        assert_eq!(program.statements.len(), expected.len(), "Program has not enough statements, expected {}, got {}", expected.len(), program.statements.len());

        expected
            .iter()
            .zip(program.statements.iter())
            .map(|(expected, actual)| 
                assert_eq!(expected, actual, "Unexpected statement. Expected {expected:?}, got {actual:?}")
            )
            .count();
    }

    #[test]
    fn test_identifier_expressions() {
        let input = r#"
            foo_bar;
            _x1;
            y2;
        "#;

        let expected = vec![
            expr_stmt!(Identifier => ident!("foo_bar")),
            expr_stmt!(Identifier => ident!("_x1")),
            expr_stmt!(Identifier => ident!("y2")),
        ];

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer).unwrap();

        let program = parser.parse();

        assert_eq!(parser.errors.len(), 0, "Parser has {} errors.\n{:?}", parser.errors.len(), parser.errors);

        assert_eq!(program.statements.len(), expected.len(), "Program has not enough statements, expected {}, got {}", expected.len(), program.statements.len());

        expected
            .iter()
            .zip(program.statements.iter())
            .map(|(expected, actual)| 
                assert_eq!(expected, actual, "Unexpected statement. Expected {expected:?}, got {actual:?}")
            )
            .count();
    }

    #[test]
    fn test_primitive_expressions() {
        let input = r#"
            5;
            "foobar";
            true;
            false;
        "#;

        let expected = vec![
            expr_stmt!(int!(5)),
            expr_stmt!(string!("foobar")),
            expr_stmt!(bool!(true)),
            expr_stmt!(bool!(false)),
        ];

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer).unwrap();

        let program = parser.parse();

        assert_eq!(parser.errors.len(), 0, "Parser has {} errors.\n{:?}", parser.errors.len(), parser.errors);

        assert_eq!(program.statements.len(), expected.len(), "Program has not enough statements, expected {}, got {}", expected.len(), program.statements.len());

        expected
            .iter()
            .zip(program.statements.iter())
            .map(|(expected, actual)| 
                assert_eq!(expected, actual, "Unexpected statement. Expected {expected:?}, got {actual:?}")
            )
            .count();
    }

    #[test]
    fn test_prefixed_expressions() {
        let input = r#"
            -5;
            !foobar;
            !true;
        "#;

        let expected = vec![
            expr_stmt!(prefix!(Dash, int!(5))),
            expr_stmt!(prefix!(Bang, expr!(Identifier => ident!("foobar")))),
            expr_stmt!(prefix!(Bang, bool!(true))),
        ];

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer).unwrap();

        let program = parser.parse();

        assert_eq!(parser.errors.len(), 0, "Parser has {} errors.\n{:?}", parser.errors.len(), parser.errors);

        assert_eq!(program.statements.len(), expected.len(), "Program has not enough statements, expected {}, got {}", expected.len(), program.statements.len());

        expected
            .iter()
            .zip(program.statements.iter())
            .map(|(expected, actual)| 
                assert_eq!(expected, actual, "Unexpected statement. Expected {expected:?}, got {actual:?}")
            )
            .count();
    }

    #[test]
    fn test_infix_expressions() {
        let input = r#"
            5 + 5;
            5 - 5;
            5 * 5;
            5 / 5;
            5 > 5;
            5 < 5;
            5 >= 5;
            5 <= 5;
            5 == 5;
            5 != 5;
        "#;

        let expected = vec![
            expr_stmt!(infix!(int!(5), Plus, int!(5))),
            expr_stmt!(infix!(int!(5), Dash, int!(5))),
            expr_stmt!(infix!(int!(5), Asterisk, int!(5))),
            expr_stmt!(infix!(int!(5), ForwardSlash, int!(5))),
            expr_stmt!(infix!(int!(5), GreaterThan, int!(5))),
            expr_stmt!(infix!(int!(5), LessThan, int!(5))),
            expr_stmt!(infix!(int!(5), GreaterThanOrEqual, int!(5))),
            expr_stmt!(infix!(int!(5), LessThanOrEqual, int!(5))),
            expr_stmt!(infix!(int!(5), Equal, int!(5))),
            expr_stmt!(infix!(int!(5), NotEqual, int!(5))),
        ];

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer).unwrap();

        let program = parser.parse();

        assert_eq!(parser.errors.len(), 0, "Parser has {} errors.\n{:?}", parser.errors.len(), parser.errors);

        assert_eq!(program.statements.len(), expected.len(), "Program has not enough statements, expected {}, got {}", expected.len(), program.statements.len());

        expected
            .iter()
            .zip(program.statements.iter())
            .map(|(expected, actual)| 
                assert_eq!(expected, actual, "Unexpected statement. Expected {expected:?}, got {actual:?}")
            )
            .count();
    }

    #[test]
    fn test_operator_precedence_parsing() {
        let input = r#"
            -a * b;
            !-a;
            a + b + c;
            a + b - c;
            a * b * c;
            a * b / c;
            a + b / c;
            a + b * c + d / e -f;
            3 + 4; -5 * 5;
            5 > 4 == 3 < 4;
            5 < 4 != 3 > 4;
            3 + 4 * 5 == 3 * 1 + 4 * 5;
            3 > 5 == false;
            3 < 5 == true;
            10 >= 10 == true;
            9 <= 10 == true;
            1 + (2 + 3) + 4;
            (5 + 5) * 2;
            2 / (5 + 5);
            -(5 + 5);
            !(true == true);
        "#;

        let expected_str = vec![
            "((-a) * b)",
            "(!(-a))",
            "((a + b) + c)",
            "((a + b) - c)",
            "((a * b) * c)",
            "((a * b) / c)",
            "(a + (b / c))",
            "(((a + (b * c)) + (d / e)) - f)",
            "(3 + 4)",
            "((-5) * 5)",
            "((5 > 4) == (3 < 4))",
            "((5 < 4) != (3 > 4))",
            "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))",
            "((3 > 5) == false)",
            "((3 < 5) == true)",
            "((10 >= 10) == true)",
            "((9 <= 10) == true)",
            "((1 + (2 + 3)) + 4)",
            "((5 + 5) * 2)",
            "(2 / (5 + 5))",
            "(-(5 + 5))",
            "(!(true == true))"
        ];

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer).unwrap();

        let program = parser.parse();
        
        assert_eq!(parser.errors.len(), 0, "Parser has {} errors.\n{:?}", parser.errors.len(), parser.errors);
        
        assert_eq!(program.statements.len(), expected_str.len(), "Program has not enough statements, expected {}, got {}", expected_str.len(), program.statements.len());

        expected_str
            .iter()
            .zip(program.statements.iter())
            .map(|(expected, actual)| 
                assert_eq!(expected.to_string(), actual.to_string(), "Unexpected statement. Expected {expected:?}, got {actual:?}")
            )
            .count();
    }

    #[test]
    fn test_if_expressions() {
        let input = r#"
            if (x > y) {
                return x;
            }
        "#;

        let expected_str = vec![
            r"if (x > y) { return x; }"
        ];

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer).unwrap();

        let program = parser.parse();
        
        assert_eq!(parser.errors.len(), 0, "Parser has {} errors.\n{:?}", parser.errors.len(), parser.errors);
        
        assert_eq!(program.statements.len(), expected_str.len(), "Program has not enough statements, expected {}, got {}", expected_str.len(), program.statements.len());
    
        expected_str
            .iter()
            .zip(program.statements.iter())
            .map(|(expected, actual)| 
                assert_eq!(expected.to_string(), actual.to_string(), "Unexpected statement. Expected {expected:?}, got {actual:?}")
            )
            .count();
    }

    #[test]
    fn test_if_else_expressions() {
        let input = r#"
            if (x > y) {
                return x;
            } else {
                return y; 
            }

            let foobar = if (x > y) { x } else { y };
        "#;

        let expected_str = vec![
            r"if (x > y) { return x; } else { return y; }",
            r"let foobar = if (x > y) { x } else { y };"
        ];

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer).unwrap();

        let program = parser.parse();
        
        assert_eq!(parser.errors.len(), 0, "Parser has {} errors.\n{:?}", parser.errors.len(), parser.errors);
        
        assert_eq!(program.statements.len(), expected_str.len(), "Program has not enough statements, expected {}, got {}", expected_str.len(), program.statements.len());
    
        expected_str
            .iter()
            .zip(program.statements.iter())
            .map(|(expected, actual)| 
                assert_eq!(expected.to_string(), actual.to_string(), "Unexpected statement. Expected {expected:?}, got {actual:?}")
            )
            .count();
    }

    #[test]
    fn test_function_defs() {
        let input = r#"
            fn main() {
                return 1;
            }

            fn add(one, two) {
                return one + two;
            }
        "#;

        let expected_str = vec![
            r"fn main() { return 1; }",
            r"fn add(one, two) { return (one + two); }"
        ];

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer).unwrap();

        let program = parser.parse();
        
        assert_eq!(parser.errors.len(), 0, "Parser has {} errors.\n{:?}", parser.errors.len(), parser.errors);
        
        assert_eq!(program.statements.len(), expected_str.len(), "Program has not enough statements, expected {}, got {}", expected_str.len(), program.statements.len());
    
        expected_str
            .iter()
            .zip(program.statements.iter())
            .map(|(expected, actual)| 
                assert_eq!(expected.to_string(), actual.to_string(), "Unexpected statement. Expected {expected:?}, got {actual:?}")
            )
            .count();
    }

    #[test]
    fn test_function_calls() {
        let input = r#"
            add(1, 2);
            add(1 * 4, 2 + 3);
        "#;

        let expected_str = vec![
            r"add(1, 2)",
            r"add((1 * 4), (2 + 3))"
        ];

        let mut lexer = Lexer::new(input.to_string());
        let mut parser = Parser::new(&mut lexer).unwrap();

        let program = parser.parse();
        
        assert_eq!(parser.errors.len(), 0, "Parser has {} errors.\n{:?}", parser.errors.len(), parser.errors);
        
        assert_eq!(program.statements.len(), expected_str.len(), "Program has not enough statements, expected {}, got {}", expected_str.len(), program.statements.len());
    
        expected_str
            .iter()
            .zip(program.statements.iter())
            .map(|(expected, actual)| 
                assert_eq!(expected.to_string(), actual.to_string(), "Unexpected statement. Expected {expected:?}, got {actual:?}")
            )
            .count();
    }
}