use std::{cell::RefCell, fmt::{Debug, Display}, rc::Rc};

use crate::ast::{Block, Identifier, FunctionDefinition};

use super::environment::Environment;

pub const NULL: Object = Object::Null(Null);
pub const TRUE: Object = Object::Boolean(Boolean { value: true });
pub const FALSE: Object = Object::Boolean(Boolean { value: false });

#[derive(Debug, PartialEq)]
pub enum ObjectType {
    Integer,
    Float,
    Hex,
    String,
    Boolean,
    Return,
    Function,
    Error,
    Null,
}

impl Display for ObjectType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Self::Boolean => "Boolean",
            Self::String => "String",
            Self::Integer => "Integer",
            Self::Float => "Float",
            Self::Hex => "Hexadecimal",
            Self::Return => "Return",
            Self::Function => "Function",
            Self::Error => "Error",
            Self::Null => "Null",
        };

        write!(f, "{}", str)
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Object {
    Integer(Integer),
    Float(Float),
    Hex(Hex),
    String(StringObj),
    Boolean(Boolean),
    Return(Return),
    Function(Function),
    Error(Error),
    Null(Null)
}

impl Object {
    pub fn inspect(&self) -> String {
        match self {
            Self::Integer(Integer { value }) => format!("{}", value),
            Self::Float(Float { value }) => format!("{}", value),
            Self::Hex(Hex { value }) => format!("{}", value),
            Self::String(StringObj { value }) => format!("\"{}\"", value),
            Self::Boolean(Boolean { value }) => format!("{}", value),
            Self::Return(Return { value }) => value.inspect(),
            Self::Function(function) => {
                let params = function.parameters.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");

                let statements = function.body.statements.iter()
                    .map(|x| x.to_string())
                    .collect::<Vec<String>>()
                    .join("\n\t");

                format!("fn {}({}) {{\n\t{}\n}}", function.identifier, params, statements)
            },
            Self::Error(Error { message }) => format!("ERR: {message}"),
            Self::Null(_) => String::from("null")
        }
    }

    pub fn _type(&self) -> ObjectType {
        match self {
            Self::Integer(_) => ObjectType::Integer,
            Self::Float(_) => ObjectType::Float,
            Self::Hex(_) => ObjectType::Hex,
            Self::String(_) => ObjectType::String,
            Self::Boolean(_) => ObjectType::Boolean,
            Self::Return(_) => ObjectType::Return,
            Self::Function(_) => ObjectType::Function,
            Self::Error(_) => ObjectType::Error,
            Self::Null(_) => ObjectType::Null,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Integer {
    pub value: i64,
}

impl From<i64> for Integer {
    fn from(value: i64) -> Self {
        Integer { value }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Float {
    pub value: f64,
}

impl From<f64> for Float {
    fn from(value: f64) -> Self {
        Float { value }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Hex {
    pub value: i64,
}

impl From<i64> for Hex {
    fn from(value: i64) -> Self {
        Hex { value }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StringObj {
    pub value: String,
}

impl From<String> for StringObj {
    fn from(value: String) -> Self {
        StringObj { value }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Boolean {
    pub value: bool,
}

impl From<bool> for Boolean {
    fn from(value: bool) -> Self {
        Boolean { value }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Return {
    pub value: Box<Object>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub identifier: String,
    pub parameters: Vec<Identifier>,
    pub body: Block,
    pub env: Rc<RefCell<Environment>>,
}

impl Function {
    pub fn new(def: FunctionDefinition, env: Rc<RefCell<Environment>>) -> Self {
        Function {
            identifier: def.identifier.value,
            parameters: def.parameters,
            body: def.body,
            env: Rc::new(RefCell::new(Environment::new(Some(env)))),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Error {
    pub message: String,
}

impl From<String> for Error {
    fn from(value: String) -> Self {
        Error { message: value }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Null;