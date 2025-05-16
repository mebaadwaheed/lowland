use std::fmt;
use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ErrorCode {
    // Lexer Errors (Lxxxx)
    L0001, // Unknown character
    L0002, // Unterminated string literal
    L0003, // Invalid number format
    L0004, // Invalid escape sequence
    L0005, // Unterminated multi-line comment

    // Parser Errors (Pxxxx)
    P0000, // Generic parser error (use specific codes when possible)
    P0001, // Unexpected token
    P0002, // Expected token
    P0003, // Expected expression
    P0004, // Expected statement
    P0005, // Expected identifier
    P0006, // Expected type annotation
    P0007, // Invalid assignment target
    P0008, // Too many arguments in function call
    P0009, // Expected parameter list
    P0010, // Expected function body
    P0011, // Return outside function
    P0012, // Break outside loop
    P0013, // Continue outside loop

    // Type Errors (Txxxx)
    T0001, // Type mismatch
    T0002, // Operation not supported for type
    T0003, // Incompatible types for operation
    T0004, // Variable not mutable
    T0005, // Expected boolean condition
    T0006, // Return type mismatch
    T0007, // Argument type mismatch
    T0008, // Invalid property or method access on a given type
    T0009, // Invalid target for a set operation (e.g., not an object)
    T0010, // Attempt to call a method on a type that does not support methods or that specific method

    // Runtime Errors (Rxxxx)
    R0000, // Generic runtime error (e.g., feature not implemented)
    R0001, // Undefined variable
    R0002, // Division by zero
    R0003, // Index out of bounds (generic, can be used for lists, strings, etc.)
    R0004, // Not a function
    R0005, // Incorrect number of arguments
    R0006, // Invalid increment/decrement target
    R0007, // Modulo by zero
    R0008, // Method not found on type
    R0009, // Operation on empty collection (e.g., pop from empty list)
    R0010, // List index out of bounds (specific for list methods, can use R0003 if preferred)

    // IO Errors (Ixxxx)
    I0001, // File not found
    I0002, // Permission denied
    I0003, // Read error
    I0004, // Write error

    // Struct Errors (Sxxxx) - Placeholders for future
    S0001, // Struct not defined
    S0002, // Field not found in struct
    S0003, // Duplicate field name in struct definition
    S0004, // Cannot instantiate abstract struct (if we add them)
    S0005, // Type mismatch in struct field assignment
    S0006, // Missing field in struct instantiation

    // Placeholder for general/unknown errors
    GEN001, // Generic error
}

impl fmt::Display for ErrorCode {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceLocation {
    pub line: usize,
    pub column: usize,
    pub file_path: usize,
    // Optional: could also store file_path or source_line_text
}

impl SourceLocation {
    pub fn new(line: usize, column: usize, file_path: usize) -> Self {
        Self { line, column, file_path }
    }
}

impl fmt::Display for SourceLocation {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "line {}, column {}, file_path {}", self.line, self.column, self.file_path)
    }
}


#[derive(Debug, Clone)]
pub struct ErrorDetails {
    pub code: ErrorCode,
    pub message: String,
    pub location: Option<SourceLocation>,
    // Optional: could store the actual source line text
    // pub source_line_text: Option<String>,
}

impl fmt::Display for ErrorDetails {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        if let Some(loc) = &self.location {
            write!(f, "[{}] {} (at {})", self.code, self.message, loc)
        } else {
            write!(f, "[{}] {}", self.code, self.message)
        }
    }
}


#[derive(Error, Debug, Clone)]
pub enum Error {
    #[error("{0}")]
    SyntaxError(ErrorDetails),

    #[error("{0}")]
    TypeError(ErrorDetails),

    #[error("{0}")]
    RuntimeError(ErrorDetails),

    #[error("{0}")]
    IoError(ErrorDetails),

    #[error("{0}")]
    UndefinedError(ErrorDetails), // Kept if distinct from general RuntimeError for undefined vars

    // Control flow "errors" should ideally not use ErrorDetails if they don't represent actual faults
    // For now, keeping them simple. We might refactor how control flow is handled later.
    #[error("Break")] // No ErrorDetails needed
    BreakControlFlow,

    #[error("Continue")] // No ErrorDetails needed
    ContinueControlFlow,

    #[error("Return: {0:?}")] // This needs special handling for the return value.
    ReturnControlFlow(Box<crate::value::Value>), // Assuming Value is in crate::value and needs to be boxed
}

// Helper constructors (optional, but can be convenient)
impl Error {
    pub fn syntax(code: ErrorCode, message: String, location: Option<SourceLocation>) -> Self {
        Error::SyntaxError(ErrorDetails { code, message, location })
    }

    pub fn type_error(code: ErrorCode, message: String, location: Option<SourceLocation>) -> Self {
        Error::TypeError(ErrorDetails { code, message, location })
    }

    pub fn runtime(code: ErrorCode, message: String, location: Option<SourceLocation>) -> Self {
        Error::RuntimeError(ErrorDetails { code, message, location })
    }

    pub fn io(code: ErrorCode, message: String, location: Option<SourceLocation>) -> Self {
        Error::IoError(ErrorDetails { code, message, location })
    }
    
    pub fn undefined(code: ErrorCode, message: String, location: Option<SourceLocation>) -> Self {
        Error::UndefinedError(ErrorDetails { code, message, location })
    }

    // Keep BreakError and ContinueError simple, as they are for control flow, not "errors" per se.
    // Renamed to avoid confusion if we have actual BreakError/ContinueError codes for "break outside loop".
    // The old BreakError and ContinueError are now P0012 and P0013 for parser to catch misuse.
}

// Example of how to create a new error:
// Error::syntax(
//     ErrorCode::P0001,
//     "Unexpected end of file".to_string(),
//     Some(SourceLocation::new(line, col))
// ) 