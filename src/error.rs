use thiserror::Error;

#[derive(Error, Debug, Clone)]
pub enum Error {
    #[error("Syntax error: {0}")]
    SyntaxError(String),

    #[error("Type error: {0}")]
    TypeError(String),

    #[error("Runtime error: {0}")]
    RuntimeError(String),

    #[error("IO error: {0}")]
    IoError(String),

    #[error("Undefined: {0}")]
    UndefinedError(String),

    #[error("Break statement outside of loop")]
    BreakError,

    #[error("Continue statement outside of loop")]
    ContinueError,
} 