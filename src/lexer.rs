use crate::error::{Error, ErrorCode, SourceLocation};
use crate::token::{Token, TokenType};
use std::collections::HashMap;

/// Lexer for the Lowland language
pub struct Lexer {
    source: String,
    tokens: Vec<Token>,
    start: usize,      // Start of the current lexeme being scanned (byte offset)
    current: usize,    // Current character being looked at (byte offset)
    line: usize,       // Current line number
    column: usize,     // Current column number on the current line
    start_column: usize, // Column number where the current lexeme started
    keywords: HashMap<String, TokenType>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        let mut keywords = HashMap::new();
        keywords.insert("let".to_string(), TokenType::Let);
        keywords.insert("string".to_string(), TokenType::String);
        keywords.insert("int".to_string(), TokenType::Int);
        keywords.insert("float".to_string(), TokenType::Float);
        keywords.insert("bool".to_string(), TokenType::Bool);
        keywords.insert("obj".to_string(), TokenType::Obj);
        keywords.insert("list".to_string(), TokenType::List);
        keywords.insert("true".to_string(), TokenType::True);
        keywords.insert("false".to_string(), TokenType::False);
        keywords.insert("null".to_string(), TokenType::Null);
        keywords.insert("println".to_string(), TokenType::Println);
        keywords.insert("inputln".to_string(), TokenType::Inputln);
        keywords.insert("toInt".to_string(), TokenType::ToInt);
        keywords.insert("toBool".to_string(), TokenType::ToBool);
        keywords.insert("toString".to_string(), TokenType::ToString);
        keywords.insert("toFloat".to_string(), TokenType::ToFloat);
        keywords.insert("sqrt".to_string(), TokenType::Sqrt);
        keywords.insert("if".to_string(), TokenType::If);
        keywords.insert("elif".to_string(), TokenType::Elif);
        keywords.insert("else".to_string(), TokenType::Else);
        keywords.insert("func".to_string(), TokenType::Func);
        keywords.insert("return".to_string(), TokenType::Return);
        keywords.insert("for".to_string(), TokenType::For);
        keywords.insert("while".to_string(), TokenType::While);
        keywords.insert("break".to_string(), TokenType::Break);
        keywords.insert("continue".to_string(), TokenType::Continue);
        keywords.insert("including".to_string(), TokenType::Including);
        keywords.insert("export".to_string(), TokenType::Export);
        keywords.insert("from".to_string(), TokenType::From);

        Lexer {
            source: source.to_string(),
            tokens: Vec::new(),
            start: 0,
            current: 0,
            line: 1,
            column: 1,
            start_column: 1,
            keywords,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, Error> {
        while !self.is_at_end() {
            self.start = self.current;
            self.start_column = self.column; // Capture column at the start of the token
            self.scan_token()?;
        }

        // For EOF token, use the current line and column
        self.tokens.push(Token::new(
            TokenType::Eof,
            "".to_string(),
            self.line,
            self.column, 
        ));

        Ok(self.tokens.clone())
    }

    fn scan_token(&mut self) -> Result<(), Error> {
        let c = self.advance(); // advance() will update self.column

        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            '[' => self.add_token(TokenType::LeftBracket),
            ']' => self.add_token(TokenType::RightBracket),
            ',' => self.add_token(TokenType::Comma),
            '.' => {
                if self.is_digit(self.peek()) {
                    // It's a float starting with a dot, like .123
                    // The self.start is already at the '.'.
                    // We need to consume the digits for the fractional part.
                    // The number() method expects to start *before* the first digit or before the dot if it handles initial dots.
                    // For .123, we consume the dot here (implicitly done by advance that got us here),
                    // then consume digits.
                    while self.is_digit(self.peek()) {
                        self.advance();
                    }
                    self.add_token(TokenType::FloatLiteral); 
                } else {
                    self.add_token(TokenType::Dot);
                }
            },
            ';' => self.add_token(TokenType::Semicolon),
            ':' => {
                if self.match_char(':') {
                    self.add_token(TokenType::ColonColon);
                } else {
                    self.add_token(TokenType::Colon);
                }
            },
            '&' => {
                if self.match_char('&') {
                    self.add_token(TokenType::AmpersandAmpersand);
                } else {
                    self.add_token(TokenType::Ampersand);
                }
            },
            '|' => {
                if self.match_char('|') {
                    self.add_token(TokenType::PipePipe);
                } else {
                    // Error for single '|'
                    return Err(Error::syntax(
                        ErrorCode::L0001, // Example: Using L0001 for general unknown/unexpected chars
                        format!("Unexpected character '{}'. Did you mean '||'?", '|'),
                        Some(SourceLocation::new(self.line, self.start_column)),
                    ));
                }
            },
            '+' => {
                if self.match_char('+') {
                    self.add_token(TokenType::PlusPlus);
                } else {
                    self.add_token(TokenType::Plus);
                }
            },
            '-' => {
                if self.match_char('-') {
                    self.add_token(TokenType::MinusMinus);
                } else {
                    self.add_token(TokenType::Minus);
                }
            },
            '*' => {
                if self.match_char('*') {
                    self.add_token(TokenType::StarStar);
                } else {
                    self.add_token(TokenType::Star);
                }
            },
            '/' => {
                if self.match_char('/') {
                    // A comment goes until the end of the line
                    while self.peek() != '\n' && !self.is_at_end() {
                        self.advance(); // Continue advancing, column will be updated
                    }
                    // No token added for comments
                } else if self.match_char('*') {
                    // Multi-line comment
                    let comment_start_line = self.line;
                    let comment_start_column = self.start_column;
                    let mut nesting_level = 1;
                    
                    while nesting_level > 0 && !self.is_at_end() {
                        if self.peek() == '/' && self.peek_next() == '*' {
                            self.advance(); // Consume '/'
                            self.advance(); // Consume '*'
                            nesting_level += 1;
                        } else if self.peek() == '*' && self.peek_next() == '/' {
                            self.advance(); // Consume '*'
                            self.advance(); // Consume '/'
                            nesting_level -= 1;
                        } else {
                            if self.peek() == '\n' {
                                self.line += 1;
                                self.column = 1;
                            }
                            self.advance();
                        }
                    }
                    
                    if nesting_level > 0 {
                        return Err(Error::syntax(
                            ErrorCode::L0003, // Unterminated multi-line comment
                            "Unterminated multi-line comment.".to_string(),
                            Some(SourceLocation::new(comment_start_line, comment_start_column)),
                        ));
                    }
                    // No token added for comments
                } else {
                    self.add_token(TokenType::Slash);
                }
            }
            '%' => self.add_token(TokenType::Modulo),
            '!' => {
                let token_type = if self.match_char('=') {
                    TokenType::BangEqual
                } else {
                    TokenType::Bang
                };
                self.add_token(token_type);
            }
            '=' => {
                let token_type = if self.match_char('=') {
                    TokenType::EqualEqual
                } else {
                    TokenType::Equal
                };
                self.add_token(token_type);
            }
            '<' => {
                let token_type = if self.match_char('=') {
                    TokenType::LessEqual
                } else {
                    TokenType::Less
                };
                self.add_token(token_type);
            }
            '>' => {
                let token_type = if self.match_char('=') {
                    TokenType::GreaterEqual
                } else {
                    TokenType::Greater
                };
                self.add_token(token_type);
            }
            '"' => self.string()?,
            ' ' | '\r' | '\t' => {
                // Ignore whitespace, column is advanced by advance()
            }
            '\n' => {
                self.line += 1;
                self.column = 1; // Reset column on new line
            }
            _ => {
                if self.is_digit(c) {
                    self.number()?;
                } else if self.is_alpha(c) {
                    self.identifier();
                } else {
                    return Err(Error::syntax(
                        ErrorCode::L0001, // Unknown character
                        format!("Unknown character '{}'", c),
                        Some(SourceLocation::new(self.line, self.start_column)), // Use start_column
                    ));
                }
            }
        }
        Ok(())
    }

    fn identifier(&mut self) {
        while self.is_alphanumeric(self.peek()) {
            self.advance();
        }
        let text = &self.source[self.start..self.current];
        
        // Check if the identifier is "println" and if it's followed by '!'
        if text == "println" && self.peek() == '!' {
            self.advance(); // Consume the '!'
            self.add_token(TokenType::PrintlnBang);
        } else {
            let token_type = self.keywords.get(text).cloned().unwrap_or(TokenType::Identifier);
            self.add_token(token_type);
        }
    }

    fn number(&mut self) -> Result<(), Error> {
        let mut is_float = false;
        while self.is_digit(self.peek()) {
            self.advance();
        }

        // Look for a fractional part OR just a trailing dot making it a float.
        if self.peek() == '.' {
            // Check if it's of the form ".<digit>" (e.g., 1.23) or just a trailing "." (e.g., 1.)
            if self.is_digit(self.peek_next()) { 
                is_float = true;
                self.advance(); // Consume the "."
                while self.is_digit(self.peek()) {
                    self.advance(); // Consume digits after decimal point
                }
            } else {
                // It's a number followed by a dot, but not more digits (e.g., "1.")
                // This should still be a float. The dot makes it a float.
                is_float = true;
                self.advance(); // Consume the trailing "."
            }
        }

        if is_float {
            self.add_token(TokenType::FloatLiteral);
        } else {
            self.add_token(TokenType::IntLiteral);
        }
        Ok(())
    }

    fn string(&mut self) -> Result<(), Error> {
        let string_start_line = self.line;
        let string_start_column = self.start_column;

        // self.current is already past the opening quote because scan_token advanced to get '"'
        // and then called string(). So, self.start is at the opening quote, self.current is after it.
        let content_start_byte_offset = self.current;
        
        let mut has_escape_sequences = false;
        
        // Temporary variables for line/column tracking *during* the string scan
        let mut current_content_line = self.line;
        let mut current_content_column = self.column;


        // Scan for the closing quote
        while self.peek() != '"' && !self.is_at_end() {
            let char_val = self.peek();

            if char_val == '\\' {
                has_escape_sequences = true;
                self.advance(); // Consume '\'
                // current_content_column updated by advance()

                if self.is_at_end() { // String ends with unescaped '\'
                    break; 
                }
                // Also consume the character *after* '\'
                // as it's part of the escape sequence for scanning purposes.
                // Update line/col if the char *after* \ is a newline
                if self.peek() == '\n' {
                    current_content_line += 1;
                    current_content_column = 1; // Reset for newline, advance will fix to 1
                    self.advance(); // Consume the newline
                } else {
                    self.advance(); // Consume the character after '\'
                    // current_content_column updated by advance()
                }
            } else if char_val == '\n' {
                current_content_line += 1;
                current_content_column = 1; // Reset for newline, advance will fix to 1
                self.advance(); // Consume '\n'
            } else {
                self.advance(); // Consume other characters
                // current_content_column updated by advance()
            }
        }
        
        // After the loop, self.current points AT the closing quote or EOF.
        // Update the lexer's main line and column state to reflect the end of the string content scan.
        self.line = current_content_line;
        self.column = current_content_column; // This should be the column of the closing quote or EOF

        if self.is_at_end() { // Check if loop ended due to EOF before finding closing quote
            return Err(Error::syntax(
                ErrorCode::L0002, // Unterminated string
                "Unterminated string literal.".to_string(),
                Some(SourceLocation::new(string_start_line, string_start_column)),
            ));
        }

        // content_end_byte_offset is current position (at the closing quote)
        let content_end_byte_offset = self.current;
        
        let raw_content = self.source[content_start_byte_offset..content_end_byte_offset].to_string();
        
        let processed_content = if has_escape_sequences {
            self.process_escapes(&raw_content, string_start_line, string_start_column + 1)? // +1 for after opening quote
        } else {
            raw_content
        };
        
        self.advance(); // Consume the closing quote. This updates self.current and self.column.
        
        let lexeme = self.source[self.start..self.current].to_string();
        
        self.tokens.push(Token::new_with_value(
            TokenType::StringLiteral,
            lexeme,
            string_start_line,
            string_start_column,
            processed_content
        ));
        
        Ok(())
    }

    /// Process escape sequences in a string
    fn process_escapes(&self, input: &str, error_report_line: usize, error_report_col_start: usize) -> Result<String, Error> {
        let mut result = String::with_capacity(input.len());
        let mut chars = input.chars();
        
        // For more precise error location within the string (future enhancement)
        // let mut current_col_offset = 0; 

        while let Some(c) = chars.next() {
            if c == '\\' {
                // current_col_offset += 1; // '\'
                match chars.next() {
                    Some('n') => { result.push('\n'); /*current_col_offset += 1;*/ }
                    Some('t') => { result.push('\t'); /*current_col_offset += 1;*/ }
                    Some('r') => { result.push('\r'); /*current_col_offset += 1;*/ }
                    Some('\\') => { result.push('\\'); /*current_col_offset += 1;*/ }
                    Some('"') => { result.push('"'); /*current_col_offset += 1;*/ }
                    Some('\'') => { result.push('\''); /*current_col_offset += 1;*/ } // For single quote
                    Some('0') => { result.push('\0'); /*current_col_offset += 1;*/ }
                    // No line continuation '\n' removal here by default.
                    // If '\n' should be removed, add: Some('\n') => { /* current_col_offset +=1; */ }
                    Some(escaped_char) => {
                        return Err(Error::syntax(
                            ErrorCode::L0004,
                            format!("Invalid escape sequence '\\{}'.", escaped_char),
                            Some(SourceLocation::new(error_report_line, error_report_col_start /* + current_col_offset -1 */))
                        ));
                    },
                    None => {
                        return Err(Error::syntax(
                            ErrorCode::L0004,
                            "Incomplete escape sequence at end of string.".to_string(),
                            Some(SourceLocation::new(error_report_line, error_report_col_start /* + current_col_offset -1 */))
                        ));
                    }
                }
            } else {
                result.push(c);
                // current_col_offset += 1;
            }
        }
        Ok(result)
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() || self.peek() != expected {
            return false;
        }
        // self.current is advanced by advance() which is called next if match_char is part of a multi-char token
        // Here, we just consume it for the match logic, so advance needs to be called.
        self.advance(); // Consume the character and update column
        true
    }

    fn peek(&self) -> char {
        if self.current >= self.source.len() {
            return '\0';
        }
        // Safely gets the char starting at the current byte offset
        self.source[self.current..].chars().next().unwrap_or('\0')
    }

    fn peek_next(&self) -> char {
        if self.current >= self.source.len() {
            return '\0';
        }
        // Get current char to find its length
        let current_char = self.source[self.current..].chars().next().unwrap_or('\0');
        if current_char == '\0' {
            return '\0';
        }
        let next_char_byte_offset = self.current + current_char.len_utf8();
        if next_char_byte_offset >= self.source.len() {
            return '\0';
        }
        self.source[next_char_byte_offset..].chars().next().unwrap_or('\0')
    }

    fn is_alpha(&self, c: char) -> bool {
        c.is_ascii_alphabetic() || c == '_'
    }

    fn is_alphanumeric(&self, c: char) -> bool {
        c.is_ascii_alphanumeric() || c == '_'
    }

    fn is_digit(&self, c: char) -> bool {
        c.is_ascii_digit()
    }

    fn advance(&mut self) -> char {
        if self.current >= self.source.len() {
            // self.column +=1; // To make EOF token appear after last char if not already handled
            return '\0';
        }
        
        let c = self.source[self.current..].chars().next().unwrap_or('\0');

        if c == '\0' { // Should ideally not happen if self.current < self.source.len()
             self.current = self.source.len(); // Force to end
             // self.column +=1; // May need adjustment for EOF location
             return '\0';
        }

        self.current += c.len_utf8(); // Advance by its actual byte length

        // Column management:
        // - If c is '\n', the caller (scan_token or string scanning loop) is responsible for resetting self.column and incrementing self.line.
        // - Otherwise, increment column.
        if c != '\n' {
            self.column += 1;
        }
        // Note: scan_token's '\n' case will set self.line += 1 and self.column = 1.
        // The string() method's internal loop updates its temporary line/col and then sets self.line/self.column.
        c
    }

    fn add_token(&mut self, token_type: TokenType) {
        let lexeme = self.source[self.start..self.current].to_string();
        self.tokens.push(Token::new(
            token_type,
            lexeme,
            self.line, // Line where the token starts
            self.start_column, // Column where the token starts
        ));
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }
} 