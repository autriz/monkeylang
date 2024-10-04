pub mod util;

use std::fmt::Display;

use crate::{parser::{InfixParse, Parse, Precedence}, token::Token};

#[derive(Debug)]
pub struct Program {
    pub statements: Vec<Statement>,
}

impl Display for Program {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = self.statements.iter()
            .map(|stmt| stmt.to_string())
            .collect::<Vec<String>>().join("\n");

        write!(f, "{}", string)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    Let(Let),
    Return(Return),
    Block(Block),
    Expression(Expression),
}

impl<'a> Parse<'a> for Statement {
    fn parse(parser: &mut crate::parser::Parser<'a>, _precedence: Option<Precedence>) -> Option<Self> {
        let stmt = match &parser.current_token {
            Token::Let => Some(Self::Let(Let::parse(parser, None)?)),
            Token::Return => Some(Self::Return(Return::parse(parser, None)?)),
            Token::LBrace => Some(Self::Block(Block::parse(parser, None)?)),
            _ => Some(Self::Expression(Expression::parse(parser, None)?)),
        };

        if parser.peek_token == Token::Semicolon {
            let _ = parser.step();
        }

        stmt
    }
}

impl Display for Statement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Let(stmt) => write!(f, "{}", stmt.to_string()),
            Self::Return(stmt) => write!(f, "{}", stmt.to_string()),
            Self::Block(stmt) => write!(f, "{}", stmt.to_string()),
            Self::Expression(stmt) => write!(f, "{}", stmt.to_string()),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Let {
    pub name: Identifier,
    pub value: Expression
}

impl<'a> Parse<'a> for Let {
    fn parse(parser: &mut crate::parser::Parser<'a>, _precedence: Option<Precedence>) -> Option<Self> {
        if !parser.peek_token.is_ident() {
            parser.errors.push(crate::parser::Error::UnexpectedToken(parser.peek_token.to_owned()));
            parser.skip_to_semicolon();

            return None;
        }

        let _ = parser.step();
        
        let name = Identifier::parse(parser, None)?;

        if !parser.expect_peek(Token::Assign) {
            parser.skip_to_semicolon();

            return None;
        }

        let _ = parser.step();

        let value = Expression::parse(parser, Some(Precedence::Lowest))?;
        // let value = Expression::Primitive(Primitive::Int(0));

        parser.skip_to_semicolon();

        Some(Self { name, value })
    }
}

impl Display for Let {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "let {} = {};", self.name, self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub value: Expression,
}

impl<'a> Parse<'a> for Return {
    fn parse(parser: &mut crate::parser::Parser<'a>, _precedence: Option<Precedence>) -> Option<Self> {
        parser.step().unwrap();

        Some(Self {
            value: Expression::parse(parser, Some(Precedence::Lowest))?,
        })
    }
}

impl Display for Return {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "return {};", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block {
    pub statements: Vec<Statement>
}

impl<'a> Parse<'a> for Block {
    fn parse(parser: &mut crate::parser::Parser<'a>, _precedence: Option<Precedence>) -> Option<Self> {
        let mut statements = vec![];
        
        parser.step().unwrap();

        while parser.current_token != Token::RBrace {
            match Statement::parse(parser, None) {
                Some(stmt) => statements.push(stmt),
                None => {}
            }

            parser.step().unwrap();
        }

        if parser.peek_token == Token::Semicolon {
            parser.step().unwrap();
        }

        Some(Self {
            statements
        })
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let string = self.statements.iter()
            .map(|stmt| stmt.to_string())
            .collect::<Vec<String>>().join("\n\t");

        write!(f, "{{ {} }}", string)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expression {
    Identifier(Identifier),
    Primitive(Primitive),
    Prefix(Prefix),
    Infix(Infix),
    Conditional(Conditional),
    FunctionDefinition(FunctionDefinition),
    FunctionCall(FunctionCall),
}

impl<'a> Parse<'a> for Expression {
    fn parse(parser: &mut crate::parser::Parser<'a>, precedence: Option<Precedence>) -> Option<Self> {
        let mut expr = match &parser.current_token {
            Token::Ident(ident) => {
                Some(Self::Identifier(Identifier { value: ident.to_string() }))
            },
            Token::Int(_) | Token::True | Token::False | Token::String(_) => {
                Some(Self::Primitive(Primitive::parse(parser, None)?))
            },
            Token::Bang | Token::Dash => {
                Some(Self::Prefix(Prefix::parse(parser, None)?))
            },
            Token::LParen => {
                let _ = parser.step();

                let expr = Expression::parse(parser, Some(Precedence::Lowest));

                if !parser.expect_peek(Token::RParen) {
                    return None;
                }

                expr
            },
            Token::If => {
                Some(Self::Conditional(Conditional::parse(parser, None)?))
            },
            Token::Function => {
                Some(Self::FunctionDefinition(FunctionDefinition::parse(parser, None)?))
            },
            _ => {
                // eprintln!("Unreachable current_token: {:?}, peek_token: {:?}", &parser.current_token, &parser.peek_token);
                
                parser.errors.push(crate::parser::Error::NoParserFound(parser.current_token.clone()));
                
                return None;
            },
        };

        while parser.peek_token != Token::Semicolon && precedence.unwrap_or(Precedence::Lowest) < parser.peek_precedence() {
            expr = match &parser.peek_token {
                Token::Plus | Token::Dash | Token::ForwardSlash | 
                Token::Asterisk | Token::Equal | Token::NotEqual | 
                Token::LessThan | Token::GreaterThan | Token::LessThanOrEqual | 
                Token::GreaterThanOrEqual => {
                    let _ = parser.step();
                
                    Some(Self::Infix(Infix::parse(parser, expr?, precedence)?))
                },
                Token::LParen => {
                    let _ = parser.step();

                    Some(Self::FunctionCall(FunctionCall::parse(parser, expr?, precedence)?))
                },
                _ => return expr,
            };
        }

        expr
    }
}

impl Display for Expression {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Identifier(expr) => write!(f, "{}", expr),
            Self::Primitive(expr) => write!(f, "{}", expr),
            Self::Prefix(expr) => write!(f, "{}", expr),
            Self::Infix(expr) => write!(f, "{}", expr),
            Self::Conditional(expr) => write!(f, "{}", expr),
            Self::FunctionDefinition(expr) => write!(f, "{}", expr),
            Self::FunctionCall(expr) => write!(f, "{}", expr),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Identifier {
    pub value: String,
}

impl<'a> Parse<'a> for Identifier {
    fn parse(parser: &mut crate::parser::Parser<'a>, _precedence: Option<Precedence>) -> Option<Self> {
        match &parser.current_token {
            Token::Ident(value) => Some(Self {
                value: value.to_owned(),
            }),
            token => {
                parser.errors.push(crate::parser::Error::UnexpectedToken(token.to_owned()));

                None
            },
        }
    }
}

impl Display for Identifier {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.value)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Primitive {
    Int(i64),
    Float(f64),
    Hex(i64),
    String(String),
    Bool(bool),
}

impl<'a> Parse<'a> for Primitive {
    fn parse(parser: &mut crate::parser::Parser<'a>, _precedence: Option<Precedence>) -> Option<Self> {
        let primitive = match &parser.current_token {
            Token::Int(value) => {
                let parsed = value.parse();

                match parsed {
                    Ok(value) => Self::Int(value),
                    Err(err) => {
                        parser.errors.push(crate::parser::Error::Int(err));

                        return None;
                    }
                }
            },
            Token::Float(value) => {
                let parsed = value.parse();

                match parsed {
                    Ok(value) => Self::Float(value),
                    Err(err) => {
                        parser.errors.push(crate::parser::Error::Float(err));

                        return None;
                    }
                }
            },
            Token::Hex(value) => {
                let parsed = value.parse();

                match parsed {
                    Ok(value) => Self::Hex(value),
                    Err(_) => {
                        parser.errors.push(crate::parser::Error::Hex);

                        return None;
                    }
                }
            }
            Token::String(value) => {
                Self::String(value.to_owned())
            }
            Token::True => Self::Bool(true),
            Token::False => Self::Bool(false),
            token => {
                parser.errors.push(crate::parser::Error::UnexpectedToken(token.to_owned()));

                return None;
            },
        };

        Some(primitive)
    }
}

impl Display for Primitive {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Int(value) => write!(f, "{}", value),
            Self::Float(value) => write!(f, "{}", value),
            Self::Hex(value) => write!(f, "{}", value),
            Self::String(value) => write!(f, "\"{}\"", value),
            Self::Bool(value) => write!(f, "{}", value),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Prefix {
    pub operator: Token,
    pub right: Box<Expression>,
}

impl<'a> Parse<'a> for Prefix {
    fn parse(parser: &mut crate::parser::Parser<'a>, _precedence: Option<Precedence>) -> Option<Self> {
        let token = parser.current_token.clone();

        let _ = parser.step();

        let right = Expression::parse(parser, Some(Precedence::Prefix)).unwrap();

        Some(Prefix::from(token, right))
    }
}

impl Display for Prefix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}{})", self.operator.as_literal(), self.right)
    }
}

impl Prefix {
    pub fn from(operator: Token, right: Expression) -> Self {
        Prefix {
            operator,
            right: Box::new(right),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Infix {
    pub left: Box<Expression>,
    pub operator: Token,
    pub right: Box<Expression>,
}

impl<'a> InfixParse<'a> for Infix {
    fn parse(parser: &mut crate::parser::Parser<'a>, left: Expression, _precedence: Option<Precedence>) -> Option<Self> {
        let precedence = parser.current_precedence();

        let operator = parser.current_token.clone();
        
        let _ = parser.step();

        let right = Expression::parse(parser, Some(precedence));

        Some(Infix::from(left, operator, right?))
    }
}

impl Display for Infix {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({} {} {})", self.left, self.operator.as_literal(), self.right)
    }
}

impl Infix {
    pub fn from(left: Expression, operator: Token, right: Expression) -> Self {
        Infix {
            left: Box::new(left),
            operator,
            right: Box::new(right),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Conditional {
    pub condition: Box<Expression>,
    pub resolution: Block,
    pub alternative: Option<Block>,
}

impl<'a> Parse<'a> for Conditional {
    fn parse(parser: &mut crate::parser::Parser<'a>, _precedence: Option<Precedence>) -> Option<Self> {
        if !parser.expect_peek(Token::LParen) {
            return None;
        }

        let _ = parser.step();

        let condition = Box::new(Expression::parse(parser, Some(Precedence::Lowest))?);

        if !parser.expect_peek(Token::RParen) {
            return None;
        }

        let try_parse_block = |parser: &mut crate::parser::Parser| {
            if !parser.expect_peek(Token::LBrace) {
                return None;
            }

            Block::parse(parser, None)
        };

        let resolution = try_parse_block(parser)?;

        let _ = parser.step();

        let alternative = match &parser.current_token {
            Token::Else => Some(try_parse_block(parser)?),
            _ => None,
        };

        Some(Conditional {
            condition,
            resolution,
            alternative,
        })
    }
}

impl Display for Conditional {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if let Some(alternative) = &self.alternative {
            write!(f, "if {} {} else {}", &self.condition, &self.resolution, alternative)
        } else {
            write!(f, "if {} {}", &self.condition, &self.resolution)
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefinition {
    pub identifier: Identifier,
    pub parameters: Vec<Identifier>,
    pub body: Block,
}

impl<'a> Parse<'a> for FunctionDefinition {
    fn parse(parser: &mut crate::parser::Parser<'a>, _precedence: Option<Precedence>) -> Option<Self> {
        let parse_parameters = |parser: &mut crate::parser::Parser<'a>| {
            if parser.peek_token == Token::RParen {
                let _ = parser.step();

                return vec![];
            }

            let mut vec = vec![];

            let _ = parser.step();
        
            vec.push(Identifier::parse(parser, None).unwrap());

            while parser.peek_token == Token::Comma {
                let _ = parser.step();
                let _ = parser.step();
                
                vec.push(Identifier::parse(parser, None).unwrap());
            };

            vec
        };

        let _ = parser.step();
        
        let identifier = Identifier::parse(parser, None)?;
         
        if !parser.expect_peek(Token::LParen) {
            return None;
        }

        let parameters = parse_parameters(parser);

        if parameters.len() > 0 {
            let _ = parser.step();
        }

        if !parser.expect_peek(Token::LBrace) {
            return None;
        }

        let body = Block::parse(parser, None)?;

        return Some(FunctionDefinition { identifier, parameters, body })
    }
}

impl Display for FunctionDefinition {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let parameters = &self.parameters.iter()
            .map(|x| x.value.clone())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "fn {}({}) {}", &self.identifier, parameters, &self.body)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionCall {
    pub function: Box<Expression>,
    pub arguments: Vec<Expression>,
}

impl<'a> InfixParse<'a> for FunctionCall {
    fn parse(parser: &mut crate::parser::Parser<'a>, function: Expression, _precedence: Option<Precedence>) -> Option<Self> {
        let parse_arguments = |parser: &mut crate::parser::Parser<'a>| {
            if parser.peek_token == Token::RParen {
                let _ = parser.step();

                return vec![];
            }

            let mut vec = vec![];

            let _ = parser.step();
        
            match Expression::parse(parser, None) {
                Some(expr) => vec.push(expr),
                None => {},
            }

            while parser.peek_token == Token::Comma {
                let _ = parser.step();
                let _ = parser.step();
                
                vec.push(Expression::parse(parser, None).unwrap());
            };

            vec
        };

        let arguments = parse_arguments(parser);

        let _ = parser.step();

        if parser.peek_token == Token::Semicolon {
            let _ = parser.step();
        }

        Some(FunctionCall { function: Box::new(function), arguments })
    }
}

impl Display for FunctionCall {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let arguments = &self.arguments.iter()
            .map(|x| x.to_string())
            .collect::<Vec<String>>()
            .join(", ");

        write!(f, "{}({})", &self.function, arguments)
    }
}

#[cfg(test)]
mod test {
    use crate::{let_stmt, int};

    use super::Program;

    #[test]
    fn format_program() {
        let program = Program {
            statements: vec![
                let_stmt!("a" => int!(5))
            ]
        };

        assert_eq!(program.to_string(), "let a = 5;".to_owned(), "Invalid formatting, expected `{}`, got `{}`", "let a = 5", program.to_string());
    }
}