use crate::error::Error;
use crate::token::{Token, TokenType};
use std::collections::HashMap;

/// Lexer for the Lowland language
pub struct Lexer {
    source: String,
    tokens: Vec<Token>,
    start: usize,
    current: usize,
    line: usize,
    keywords: HashMap<String, TokenType>,
}

impl Lexer {
    pub fn new(source: &str) -> Self {
        let mut keywords = HashMap::new();
        keywords.insert("let".to_string(), TokenType::Let);
        keywords.insert("string".to_string(), TokenType::String);
        keywords.insert("int".to_string(), TokenType::Int);
        keywords.insert("bool".to_string(), TokenType::Bool);
        keywords.insert("obj".to_string(), TokenType::Obj);
        keywords.insert("list".to_string(), TokenType::List);
        keywords.insert("true".to_string(), TokenType::True);
        keywords.insert("false".to_string(), TokenType::False);
        keywords.insert("null".to_string(), TokenType::Null);
        keywords.insert("println".to_string(), TokenType::Println);
        keywords.insert("inputln".to_string(), TokenType::Inputln);
        keywords.insert("to_int".to_string(), TokenType::ToInt);
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
            keywords,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, Error> {
        while !self.is_at_end() {
            // Beginning of the next lexeme
            self.start = self.current;
            self.scan_token()?;
        }

        self.tokens.push(Token::new(
            TokenType::Eof,
            "".to_string(),
            self.line,
        ));

        Ok(self.tokens.clone())
    }

    fn scan_token(&mut self) -> Result<(), Error> {
        let c = self.advance();

        match c {
            '(' => self.add_token(TokenType::LeftParen),
            ')' => self.add_token(TokenType::RightParen),
            '{' => self.add_token(TokenType::LeftBrace),
            '}' => self.add_token(TokenType::RightBrace),
            '[' => self.add_token(TokenType::LeftBracket),
            ']' => self.add_token(TokenType::RightBracket),
            ',' => self.add_token(TokenType::Comma),
            '.' => self.add_token(TokenType::Dot),
            ';' => self.add_token(TokenType::Semicolon),
            ':' => self.add_token(TokenType::Colon),
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
                    return Err(Error::SyntaxError(format!(
                        "Unexpected character '|' at line {}. Did you mean '||'?", self.line
                    )));
                }
            },
            '+' => {
                // Check for ++
                if self.match_char('+') {
                    self.add_token(TokenType::PlusPlus);
                } else {
                    self.add_token(TokenType::Plus);
                }
            },
            '-' => {
                // Check for --
                if self.match_char('-') {
                    self.add_token(TokenType::MinusMinus);
                } else {
                    self.add_token(TokenType::Minus);
                }
            },
            '*' => {
                // Check for **
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
                        self.advance();
                    }
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
                // Ignore whitespace
            }
            '\n' => {
                self.line += 1;
            }
            _ => {
                if self.is_digit(c) {
                    self.number()?;
                } else if self.is_alpha(c) {
                    self.identifier();
                } else {
                    return Err(Error::SyntaxError(format!(
                        "Unexpected character '{}' at line {}",
                        c, self.line
                    )));
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
        let token_type = self.keywords.get(text).cloned().unwrap_or(TokenType::Identifier);

        self.add_token(token_type);
    }

    fn number(&mut self) -> Result<(), Error> {
        while self.is_digit(self.peek()) {
            self.advance();
        }

        // Look for a decimal point
        if self.peek() == '.' && self.is_digit(self.peek_next()) {
            return Err(Error::SyntaxError(
                "Lowland only supports integer numbers".to_string(),
            ));
        }

        self.add_token(TokenType::IntLiteral);
        Ok(())
    }

    fn string(&mut self) -> Result<(), Error> {
        while self.peek() != '"' && !self.is_at_end() {
            if self.peek() == '\n' {
                self.line += 1;
            }
            self.advance();
        }

        if self.is_at_end() {
            return Err(Error::SyntaxError(
                "Unterminated string literal".to_string(),
            ));
        }

        // The closing ".
        self.advance();

        // We don't need the value here, just the token type
        self.add_token(TokenType::StringLiteral);

        Ok(())
    }

    fn match_char(&mut self, expected: char) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.char_at(self.current) != expected {
            return false;
        }

        self.current += 1;
        true
    }

    fn peek(&self) -> char {
        if self.is_at_end() {
            return '\0';
        }
        self.char_at(self.current)
    }

    fn peek_next(&self) -> char {
        if self.current + 1 >= self.source.len() {
            return '\0';
        }
        self.char_at(self.current + 1)
    }

    fn is_alpha(&self, c: char) -> bool {
        c.is_alphabetic() || c == '_'
    }

    fn is_alphanumeric(&self, c: char) -> bool {
        self.is_alpha(c) || self.is_digit(c)
    }

    fn is_digit(&self, c: char) -> bool {
        c.is_ascii_digit()
    }

    fn advance(&mut self) -> char {
        let c = self.char_at(self.current);
        self.current += 1;
        c
    }

    fn add_token(&mut self, token_type: TokenType) {
        let text = self.source[self.start..self.current].to_string();
        self.tokens.push(Token::new(token_type, text, self.line));
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn char_at(&self, index: usize) -> char {
        self.source.chars().nth(index).unwrap_or('\0')
    }
} 