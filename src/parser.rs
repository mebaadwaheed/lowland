use crate::ast::{Expr, Program, Stmt, Parameter};
use crate::error::Error;
use crate::token::{Token, TokenType};
use crate::value::{Value};
use crate::types::Type;

/// Parser for the Lowland language
pub struct Parser {
    tokens: Vec<Token>,
    current: usize,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens, current: 0 }
    }

    pub fn parse(&mut self) -> Result<Program, Error> {
        let mut statements = Vec::new();

        while !self.is_at_end() {
            statements.push(self.statement()?);
        }

        Ok(Program { statements })
    }

    fn statement(&mut self) -> Result<Stmt, Error> {
        if self.match_token(TokenType::Export) {
            self.consume(TokenType::Func, "Expect 'func' after 'export'.")?;
            return self.function_declaration(true);
        }

        if self.match_token(TokenType::Func) {
            return self.function_declaration(false);
        }
        
        if self.match_token(TokenType::Return) {
            return self.return_statement();
        }
        
        if self.match_token(TokenType::Break) {
            return self.break_statement();
        }
        
        if self.match_token(TokenType::Continue) {
            return self.continue_statement();
        }
        
        if self.match_token(TokenType::While) {
            return self.while_statement();
        }
        
        if self.match_token(TokenType::For) {
            return self.for_statement();
        }
        
        if self.match_token(TokenType::If) {
            return self.if_statement();
        }
        
        if self.match_token(TokenType::Let) {
            return self.var_declaration();
        }
        
        if self.match_token(TokenType::Println) {
            return self.println_statement();
        }
        
        if self.match_token(TokenType::Including) {
            return self.including_statement();
        }
        
        if self.match_token(TokenType::LeftBrace) {
            return Ok(Stmt::Block(self.block()?));
        }

        self.expression_statement()
    }

    fn including_statement(&mut self) -> Result<Stmt, Error> {
        let imports: Option<Vec<Token>>;
        let path_str: String;

        if self.match_token(TokenType::LeftBracket) {
            // Form: including [ id1, id2 ] from "path.lln";
            let mut id_tokens = Vec::new();
            if !self.check(TokenType::RightBracket) {
                loop {
                    let id_token = self.consume(TokenType::Identifier, "Expect identifier in import list.")?;
                    id_tokens.push(id_token.clone());
                    if !self.match_token(TokenType::Comma) {
                        break;
                    }
                    if self.check(TokenType::RightBracket) { // Allow trailing comma
                        break;
                    }
                }
            }
            self.consume(TokenType::RightBracket, "Expect ']' after import list.")?;
            imports = Some(id_tokens);

            self.consume(TokenType::From, "Expect 'from' after import list.")?;
            let path_token = self.consume(TokenType::StringLiteral, "Expect file path (string literal) after 'from'.")?;
            let raw_path = path_token.lexeme.clone();
            path_str = raw_path[1..raw_path.len() - 1].to_string();
        } else {
            // Form: including "path.lln";
            imports = None;
            let path_token = self.consume(TokenType::StringLiteral, "Expect file path (string literal) after 'including'.")?;
            let raw_path = path_token.lexeme.clone();
            path_str = raw_path[1..raw_path.len() - 1].to_string();
        }

        self.consume(TokenType::Semicolon, "Expect ';' after including statement.")?;
        Ok(Stmt::Including { path: path_str, imports })
    }

    fn println_statement(&mut self) -> Result<Stmt, Error> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'println'.")?;
        
        let arguments = if !self.check(TokenType::RightParen) {
            self.parse_argument_list()?
        } else {
            Vec::new()
        };
        
        self.consume(TokenType::RightParen, "Expect ')' after println arguments.")?;
        self.consume(TokenType::Semicolon, "Expect ';' after println statement.")?;

        Ok(Stmt::Println(arguments))
    }

    fn expression_statement(&mut self) -> Result<Stmt, Error> {
        let expr = self.expression()?;
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;

        Ok(Stmt::Expression(expr))
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, Error> {
        let expr = self.logical_or()?;
        
        if self.match_token(TokenType::Equal) {
            let equals = self.previous().clone();
            let value = self.assignment()?;
            
            match expr {
                Expr::Variable(name) => {
                    return Ok(Expr::Assign(name, Box::new(value)));
                }
                Expr::Get { object, name } => {
                    return Ok(Expr::Set { object, name, value: Box::new(value) });
                }
                _ => {
                    return Err(Error::SyntaxError(format!(
                        "Invalid assignment target at line {}.",
                        equals.line
                    )));
                }
            }
        }
        
        Ok(expr)
    }

    fn logical_or(&mut self) -> Result<Expr, Error> {
        let mut expr = self.logical_and()?;

        while self.match_token(TokenType::PipePipe) {
            let operator = self.previous().clone();
            let right = self.logical_and()?;
            expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr, Error> {
        let mut expr = self.equality()?;

        while self.match_token(TokenType::AmpersandAmpersand) {
            let operator = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::Logical(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, Error> {
        let mut expr = self.comparison()?;

        while self.match_tokens(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn comparison(&mut self) -> Result<Expr, Error> {
        let mut expr = self.term()?;

        while self.match_tokens(&[
            TokenType::Greater,
            TokenType::GreaterEqual,
            TokenType::Less,
            TokenType::LessEqual,
        ]) {
            let operator = self.previous().clone();
            let right = self.term()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, Error> {
        let mut expr = self.factor()?;

        while self.match_tokens(&[TokenType::Plus, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, Error> {
        let mut expr = self.unary()?;

        while self.match_tokens(&[TokenType::Star, TokenType::Slash, TokenType::StarStar, TokenType::Modulo]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            expr = Expr::Binary(Box::new(expr), operator, Box::new(right));
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, Error> {
        if self.match_tokens(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            return Ok(Expr::Unary(operator, Box::new(right)));
        }
        self.call_or_method_access()
    }

    fn call_or_method_access(&mut self) -> Result<Expr, Error> {
        let mut expr = self.primary()?;

        loop {
            if self.match_token(TokenType::LeftParen) {
                expr = self.finish_function_call(expr)?;
            } else if self.match_token(TokenType::Dot) {
                let name_token = self.consume(TokenType::Identifier, "Expect property or method name after '.'.")?.clone();
                if self.match_token(TokenType::LeftParen) { // It's a method call
                    let arguments = self.parse_argument_list()?;
                    self.consume(TokenType::RightParen, "Expect ')' after method arguments.")?;
                    expr = Expr::MethodCall(Box::new(expr), name_token, arguments);
                } else { // It's a property access
                    expr = Expr::Get { object: Box::new(expr), name: name_token };
                }
            } else if self.match_token(TokenType::PlusPlus) {
                if let Expr::Variable(name) = expr {
                    expr = Expr::Increment(name);
                } else {
                    return Err(Error::SyntaxError(format!(
                        "Operand of postfix '++' must be a variable (at line {}).",
                        self.previous().line
                    )));
                }
            } else if self.match_token(TokenType::MinusMinus) {
                if let Expr::Variable(name) = expr {
                    expr = Expr::Decrement(name);
                } else {
                    return Err(Error::SyntaxError(format!(
                        "Operand of postfix '--' must be a variable (at line {}).",
                        self.previous().line
                    )));
                }
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_function_call(&mut self, callee: Expr) -> Result<Expr, Error> {
        let arguments = self.parse_argument_list()?;
        self.consume(TokenType::RightParen, "Expect ')' after function arguments.")?;

        match callee {
            Expr::Variable(name) => Ok(Expr::Call(name, arguments)),
            _ => Err(Error::SyntaxError(format!(
                "Can only call functions by name (e.g. identifier) at line {}.",
                 self.previous().line
            ))),
        }
    }
    
    fn parse_argument_list(&mut self) -> Result<Vec<Expr>, Error> {
        let mut arguments = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    return Err(Error::SyntaxError(format!(
                        "Cannot have more than 255 arguments (at line {}).",
                        self.peek().line
                    )));
                }
                arguments.push(self.expression()?);
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        Ok(arguments)
    }

    // This is the new primary, parsing only atomic elements
    fn primary(&mut self) -> Result<Expr, Error> {
        if self.match_token(TokenType::False) {
            return Ok(Expr::Literal(Value::Bool(false)));
        }
        if self.match_token(TokenType::True) {
            return Ok(Expr::Literal(Value::Bool(true)));
        }
        if self.match_token(TokenType::Null) {
            return Ok(Expr::Literal(Value::Null));
        }
        if self.match_token(TokenType::IntLiteral) {
            let val_str = &self.previous().lexeme;
            match val_str.parse::<i64>() {
                Ok(value) => return Ok(Expr::Literal(Value::Int(value))),
                Err(_) => return Err(Error::SyntaxError(format!("Invalid integer literal '{}' at line {}", val_str, self.previous().line))),
            }
        }
        if self.match_token(TokenType::StringLiteral) {
            let lexeme = self.previous().lexeme.clone();
            // Remove the quotes
            let value = lexeme[1..lexeme.len() - 1].to_string();
            return Ok(Expr::Literal(Value::String(value)));
        }

        if self.match_token(TokenType::LeftParen) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::Grouping(Box::new(expr)));
        }

        if self.match_token(TokenType::LeftBracket) {
            // List literal e.g., [1, "foo", a + b]
            let mut elements = Vec::new();
            if !self.check(TokenType::RightBracket) {
                loop {
                    elements.push(self.expression()?);
                    if !self.match_token(TokenType::Comma) {
                        break; 
                    }
                    // Allow trailing comma
                    if self.check(TokenType::RightBracket) {
                        break;
                    }
                }
            }
            self.consume(TokenType::RightBracket, "Expect ']' after list elements or to close empty list.")?;
            return Ok(Expr::ListLiteral(elements));
        }
        
        if self.match_token(TokenType::LeftBrace) {
            let mut properties = Vec::new();
            if !self.check(TokenType::RightBrace) {
                loop {
                    // Parse key (Identifier)
                    let key_token = self.consume(TokenType::Identifier, "Expect property name (identifier) in object literal.")?.clone();
                    self.consume(TokenType::Colon, "Expect ':' after property name in object literal.")?;
                    // Parse value (Expression)
                    let value_expr = self.expression()?;
                    properties.push((key_token, value_expr));

                    if !self.match_token(TokenType::Comma) {
                        break; // No comma, so expect RightBrace next or it's an error
                    }
                    // Allow trailing comma
                    if self.check(TokenType::RightBrace) {
                        break;
                    }
                    // If there was a comma but no RightBrace and not another key, it's an error (handled by next consume or loop iteration)
                }
            }
            self.consume(TokenType::RightBrace, "Expect '}' to close object literal.")?;
            return Ok(Expr::ObjectLiteral { properties });
        }


        // Identifier, Inputln, ToInt are potential function names or variables
        if self.match_token(TokenType::Identifier) ||
           self.match_token(TokenType::Inputln) ||    // Treat as a variable name for now
           self.match_token(TokenType::ToInt) {       // Treat as a variable name for now
            let name_token = self.previous().clone();
            // Postfix ++/-- were moved to call_or_method_access
            return Ok(Expr::Variable(name_token.lexeme));
        }
        
        let token_at_error = self.peek().clone();
        Err(Error::SyntaxError(format!(
            "Unexpected token '{}'. Expect expression at line {}.",
            token_at_error.lexeme, token_at_error.line
        )))
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, Error> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            Err(Error::SyntaxError(message.to_string()))
        }
    }

    fn match_token(&mut self, token_type: TokenType) -> bool {
        if self.check(token_type) {
            self.advance();
            true
        } else {
            false
        }
    }

    fn match_tokens(&mut self, token_types: &[TokenType]) -> bool {
        for token_type in token_types {
            if self.check(*token_type) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn check(&self, token_type: TokenType) -> bool {
        if self.is_at_end() {
            return false;
        }
        self.peek().token_type == token_type
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn is_at_end(&self) -> bool {
        self.peek().token_type == TokenType::Eof
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn var_declaration(&mut self) -> Result<Stmt, Error> {
        // Check if the variable is mutable
        let is_mutable = self.match_token(TokenType::Ampersand);
        
        // Parse the variable name
        let name_token = self.consume(TokenType::Identifier, "Expect variable name.")?;
        let name = name_token.lexeme.clone();
        
        // Parse the type annotation
        self.consume(TokenType::Colon, "Expect ':' after variable name.")?;
        let var_type = self.parse_type()?;
        
        // Parse the initializer if present
        let initializer = if self.match_token(TokenType::Equal) {
            Some(self.expression()?)
        } else {
            None
        };
        
        // If it's not mutable, it must have an initializer
        if !is_mutable && initializer.is_none() {
            return Err(Error::SyntaxError(
                "Non-mutable variables must be initialized".to_string(),
            ));
        }
        
        self.consume(TokenType::Semicolon, "Expect ';' after variable declaration.")?;
        
        Ok(Stmt::Var(name, var_type, initializer, is_mutable))
    }
    
    fn parse_type(&mut self) -> Result<Type, Error> {
        if self.match_token(TokenType::String) {
            Ok(Type::String)
        } else if self.match_token(TokenType::Int) {
            Ok(Type::Int)
        } else if self.match_token(TokenType::Bool) {
            Ok(Type::Bool)
        } else if self.match_token(TokenType::Obj) {
            Ok(Type::Object)
        } else if self.match_token(TokenType::List) {
            if self.match_token(TokenType::Less) {
                let element_type = self.parse_type()?;
                self.consume(TokenType::Greater, "Expect '>' after list element type.")?;
                Ok(Type::List(Box::new(element_type)))
            } else {
                Ok(Type::List(Box::new(Type::Unknown)))
            }
        } else {
            Err(Error::SyntaxError("Expect type.".to_string()))
        }
    }

    fn if_statement(&mut self) -> Result<Stmt, Error> {
        // Parse the condition
        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;
        
        // Parse the then branch
        let then_branch = Box::new(self.statement()?);
        
        // Parse elif branches
        let mut elif_branches = Vec::new();
        while self.match_token(TokenType::Elif) {
            self.consume(TokenType::LeftParen, "Expect '(' after 'elif'.")?;
            let elif_condition = self.expression()?;
            self.consume(TokenType::RightParen, "Expect ')' after elif condition.")?;
            
            let elif_branch = Box::new(self.statement()?);
            elif_branches.push((elif_condition, elif_branch));
        }
        
        // Parse else branch
        let else_branch = if self.match_token(TokenType::Else) {
            Some(Box::new(self.statement()?))
        } else {
            None
        };
        
        Ok(Stmt::If(condition, then_branch, elif_branches, else_branch))
    }
    
    fn block(&mut self) -> Result<Vec<Stmt>, Error> {
        let mut statements = Vec::new();
        
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.statement()?);
        }
        
        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        
        Ok(statements)
    }

    fn function_declaration(&mut self, is_exported: bool) -> Result<Stmt, Error> {
        // Parse the function name
        let name_token = self.consume(TokenType::Identifier, "Expect function name.")?;
        let name = name_token.lexeme.clone();
        
        // Parse parameters
        self.consume(TokenType::LeftParen, "Expect '(' after function name.")?;
        let mut parameters = Vec::new();
        
        if !self.check(TokenType::RightParen) {
            loop {
                // Limit the number of parameters
                if parameters.len() >= 255 {
                    return Err(Error::SyntaxError(
                        "Cannot have more than 255 parameters.".to_string()
                    ));
                }
                
                // Parse parameter name
                let param_name_token = self.consume(TokenType::Identifier, "Expect parameter name.")?;
                let param_name = param_name_token.lexeme.clone();
                
                // Parse parameter type
                self.consume(TokenType::Colon, "Expect ':' after parameter name.")?;
                let param_type = self.parse_type()?;
                
                // Add parameter
                parameters.push(Parameter {
                    name: param_name,
                    param_type,
                });
                
                // Check for more parameters
                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        
        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;
        
        // Parse optional return type
        let return_type = if self.match_token(TokenType::Colon) {
            Some(self.parse_type()?)
        } else {
            None
        };
        
        // Parse function body
        self.consume(TokenType::LeftBrace, "Expect '{' before function body.")?;
        let body = Stmt::Block(self.block()?);
        
        Ok(Stmt::Function(name, parameters, return_type, Box::new(body), is_exported))
    }
    
    fn return_statement(&mut self) -> Result<Stmt, Error> {
        // Parse optional return value
        let value = if !self.check(TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        
        self.consume(TokenType::Semicolon, "Expect ';' after return value.")?;
        
        Ok(Stmt::Return(value))
    }
    
    fn while_statement(&mut self) -> Result<Stmt, Error> {
        // Parse the condition
        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after while condition.")?;
        
        // Parse the body
        let body = Box::new(self.statement()?);
        
        Ok(Stmt::While(condition, body))
    }
    
    fn for_statement(&mut self) -> Result<Stmt, Error> {
        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;
        
        // Parse initializer
        let initializer;
        if self.match_token(TokenType::Semicolon) {
            initializer = Stmt::Expression(Expr::Literal(Value::Null)); // Empty initializer
        } else if self.match_token(TokenType::Let) {
            initializer = self.var_declaration()?;
        } else {
            initializer = self.expression_statement()?;
        }
        
        // Parse condition (if omitted, condition is true)
        let condition = if !self.check(TokenType::Semicolon) {
            self.expression()?
        } else {
            Expr::Literal(Value::Bool(true))
        };
        self.consume(TokenType::Semicolon, "Expect ';' after loop condition.")?;
        
        // Parse increment
        let increment = if !self.check(TokenType::RightParen) {
            Box::new(self.expression()?)
        } else {
            Box::new(Expr::Literal(Value::Null))
        };
        self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;
        
        // Parse body
        let body = Box::new(self.statement()?);
        
        Ok(Stmt::For(Box::new(initializer), condition, increment, body))
    }

    fn break_statement(&mut self) -> Result<Stmt, Error> {
        self.consume(TokenType::Semicolon, "Expect ';' after 'break'.")?;
        Ok(Stmt::Break)
    }
    
    fn continue_statement(&mut self) -> Result<Stmt, Error> {
        self.consume(TokenType::Semicolon, "Expect ';' after 'continue'.")?;
        Ok(Stmt::Continue)
    }
} 