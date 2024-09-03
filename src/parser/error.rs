use thiserror::Error;

pub type Result<T> = std::result::Result<T, Error>;

#[derive(Debug, Error)]
pub enum Error {
    #[error(transparent)]
    Int(#[from] std::num::ParseIntError),
    /// 0 - Expected
    /// 1 - Actual
    #[error("Unexpected token: `{0:?}`")]
    UnexpectedToken(crate::token::Token),
    #[error("No parse function for `{0:?}` found")]
    NoParserFound(crate::token::Token),
    #[error(transparent)]
    Lexer(#[from] crate::lexer::Error),
}