use std::fmt;

/// Represents a token in the Lowland language
#[derive(Debug, Clone, PartialEq)]
pub struct Token {
    pub token_type: TokenType,
    pub lexeme: String,
    pub line: usize,
    pub column: usize,
    pub value: Option<String>, // Store processed string value for StringLiteral
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
    ColonColon, // Added for namespace resolution like std::math
    Ampersand,  // For let&
    
    // One or two character tokens
    Plus,
    Minus,
    Star,
    StarStar, // New token for exponentiation (**)
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
    
    // Literals
    Identifier,
    StringLiteral,
    IntLiteral,
    FloatLiteral,
    
    // Keywords
    Let,
    String,
    Int,
    Bool,
    Obj,
    List,
    Float,
    True,
    False,
    Null,       // Added for the null keyword
    Println,
    Inputln,    // New token for inputln()
    ToInt,      // New token for to_int()
    ToBool,     // New
    ToString,   // New
    ToFloat,    // Added ToFloat
    Sqrt,       // Added Sqrt for math.sqrt
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
    PrintlnBang, // New for println!
    
    // Special tokens
    Eof,
}

impl Token {
    pub fn new(token_type: TokenType, lexeme: String, line: usize, column: usize) -> Self {
        Token {
            token_type,
            lexeme,
            line,
            column,
            value: None,
        }
    }
    
    pub fn new_with_value(token_type: TokenType, lexeme: String, line: usize, column: usize, value: String) -> Self {
        Token {
            token_type,
            lexeme,
            line,
            column,
            value: Some(value),
        }
    }
    
    /// Get the string value for StringLiteral tokens
    /// Returns the processed string value if available, otherwise the lexeme without quotes
    pub fn string_value(&self) -> String {
        if self.token_type == TokenType::StringLiteral {
            match &self.value {
                Some(val) => val.clone(),
                None => {
                    // Remove the quotes from the lexeme as a fallback
                    if self.lexeme.len() >= 2 {
                        self.lexeme[1..self.lexeme.len() - 1].to_string()
                    } else {
                        self.lexeme.clone()
                    }
                }
            }
        } else {
            self.lexeme.clone()
        }
    }
}

impl fmt::Display for Token {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?} '{}'", self.token_type, self.lexeme)
    }
} 