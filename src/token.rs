use std::fmt;

/// Represents a token in the Lowland language
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
}

/// Represents the different types of tokens in Lowland
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TokenType {
    // Single-character tokens
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    LeftBracket,
    RightBracket,
    Comma,
    Dot,
    Semicolon,
    Colon,
    Ampersand,  // For let&
    
    // One or two character tokens
    Plus,
    Minus,
    Star,
    Slash,
    Equal,
    EqualEqual,
    Bang,
    BangEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,
    Modulo,     // % (modulo/remainder operator)
    AmpersandAmpersand, // &&
    PipePipe,          // ||
    
    // New compound operators
    PlusPlus,    // ++
    MinusMinus,  // --
    StarStar,    // **
    
    // Literals
    Identifier,
    StringLiteral,
    IntLiteral,
    
    // Keywords
    Let,
    String,
    Int,
    Bool,
    Obj,
    List,
    True,
    False,
    Null,       // Added for the null keyword
    Println,
    Inputln,    // New token for inputln()
    ToInt,      // New token for to_int()
    If,
    Elif,
    Else,
    Func,
    Return,
    For,
    While,
    Break,
    Continue,
    Including,  // Added for including files
    Export,     // Added for exporting functions
    From,       // Added for selective including
    
    // Special tokens
    Eof,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize) -> Self {
        Token {
            token_type,
            lexeme,
            line,
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} '{}'", self.token_type, self.lexeme)
    }
} 