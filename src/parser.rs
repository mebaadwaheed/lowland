use crate::ast::{Expr, ExprKind, Program, Stmt, StmtKind, Parameter};
use crate::error::{Error, ErrorCode, SourceLocation};
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
            statements.push(self.declaration_or_statement()?); // Changed to reflect broader scope
        }
        Ok(Program { statements })
    }

    // Renamed from statement to declaration_or_statement to better reflect its role
    fn declaration_or_statement(&mut self) -> Result<Stmt, Error> {
        // For error reporting on unexpected tokens at statement boundaries
        let peeked_token_for_loc = self.peek().clone();
        let default_loc = SourceLocation::new(peeked_token_for_loc.line, peeked_token_for_loc.column, 0);

        if self.match_token(TokenType::Export) {
            let export_keyword = self.previous().clone();
            // Expect func after export for now
            if self.check(TokenType::Func) {
                 let func_keyword = self.advance().clone(); // Consume 'func'
                 return self.function_declaration(true, func_keyword); // Pass 'func' token for loc
            }
            // Potentially other exportable items later (e.g., variables, structs)
            return Err(Error::syntax(ErrorCode::P0004, "Expect 'func' after 'export'.".to_string(), 
                Some(SourceLocation::new(export_keyword.line, export_keyword.column + export_keyword.lexeme.len(), 0))));
        }
        if self.match_token(TokenType::Func) {
            let func_keyword = self.previous().clone();
            return self.function_declaration(false, func_keyword);
        }
        if self.match_token(TokenType::Let) {
            return self.var_declaration(); // var_declaration will create Stmt with its loc
        }
        // Add other top-level declarations like struct definitions here in the future
        
        // If not a declaration, it's a statement
        self.statement(default_loc) 
    }

    // Regular statements (not top-level declarations like func/let)
    fn statement(&mut self, default_loc_for_errors: SourceLocation) -> Result<Stmt, Error> {
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
        if self.match_token(TokenType::Println) {
            return self.println_statement();
        }
        if self.match_token(TokenType::PrintlnBang) {
            return self.print_raw_statement();
        }
        if self.match_token(TokenType::Including) {
            return self.including_statement();
        }
        if self.match_token(TokenType::LeftBrace) {
            let opening_brace = self.previous().clone();
            let loc = SourceLocation::new(opening_brace.line, opening_brace.column, 0);
            let stmts = self.block()?; 
            return Ok(Stmt::new(StmtKind::Block(stmts), loc));
        }

        // Default to expression statement
        self.expression_statement(default_loc_for_errors)
    }

    fn println_statement(&mut self) -> Result<Stmt, Error> {
        let println_token = self.previous().clone(); // 'println' token
        let loc = SourceLocation::new(println_token.line, println_token.column, 0);

        self.consume(TokenType::LeftParen, "Expect '(' after 'println'.")?;
        let arguments = if !self.check(TokenType::RightParen) {
            self.parse_argument_list()?
        } else {
            Vec::new()
        };
        self.consume(TokenType::RightParen, "Expect ')' after println arguments.")?;
        self.consume(TokenType::Semicolon, "Expect ';' after println statement.")?;

        Ok(Stmt::new(StmtKind::Println(arguments), loc))
    }

    fn print_raw_statement(&mut self) -> Result<Stmt, Error> {
        let print_raw_token = self.previous().clone(); // 'println!' token
        let loc = SourceLocation::new(print_raw_token.line, print_raw_token.column, 0);

        self.consume(TokenType::LeftParen, "Expect '(' after 'println!'.")?;
        let arguments = if !self.check(TokenType::RightParen) {
            self.parse_argument_list()?
        } else {
            Vec::new()
        };
        self.consume(TokenType::RightParen, "Expect ')' after println! arguments.")?;
        self.consume(TokenType::Semicolon, "Expect ';' after println! statement.")?;

        Ok(Stmt::new(StmtKind::PrintRaw(arguments), loc))
    }

    fn expression_statement(&mut self, _default_loc_for_errors: SourceLocation) -> Result<Stmt, Error> {
        let expr = self.expression()?;
        let loc = expr.loc.clone(); // Statement location is expression's location
        self.consume(TokenType::Semicolon, "Expect ';' after expression.")?;
        Ok(Stmt::new(StmtKind::Expression(expr), loc))
    }

    fn var_declaration(&mut self) -> Result<Stmt, Error> {
        let let_token = self.previous().clone(); // 'let' token (already consumed by declaration_or_statement)
        let decl_loc = SourceLocation::new(let_token.line, let_token.column, 0);

        let is_mutable = self.match_token(TokenType::Ampersand);

        let name_token = self.consume(TokenType::Identifier, "Expect variable name.")?.clone();
        
        let var_type = if self.match_token(TokenType::Colon) {
            self.parse_type()?
        } else {
            // Type inference could be an option here, or make type annotation mandatory
            // For now, let's make it mandatory as per original design
            let err_loc_token = self.peek();
            return Err(Error::syntax(
                ErrorCode::P0006, // Expected type annotation
                "Expect ':' and type annotation after variable name.".to_string(),
                Some(SourceLocation::new(err_loc_token.line, err_loc_token.column, 0)),
            ));
        };
        
        let initializer = if self.match_token(TokenType::Equal) {
            Some(self.expression()?)
        } else {
            None
        };
        
        self.consume(TokenType::Semicolon, "Expect ';' after variable declaration.")?;
        
        Ok(Stmt::new(
            StmtKind::Var { name_token, var_type, initializer, is_mutable },
            decl_loc, // Location of the 'let' keyword for the statement
        ))
    }

    fn parse_type(&mut self) -> Result<Type, Error> {
        let type_token = self.advance().clone(); // Consume the type token
        let loc = SourceLocation::new(type_token.line, type_token.column, 0);

        match type_token.token_type {
            TokenType::String => Ok(Type::String),
            TokenType::Int => Ok(Type::Int),
            TokenType::Float => Ok(Type::Float),
            TokenType::Bool => Ok(Type::Bool),
            TokenType::Obj => Ok(Type::Object), // Generic Object, specific struct types will need more
            TokenType::List => {
                self.consume(TokenType::Less, "Expect '<' after 'list' type.")?;
                let element_type = self.parse_type()?;
                self.consume(TokenType::Greater, "Expect '>' after list element type.")?;
                Ok(Type::List(Box::new(element_type)))
            }
            // Later, for user-defined types (structs):
            // TokenType::Identifier => Ok(Type::Custom(type_token.lexeme.clone())),
            _ => Err(Error::syntax(
                ErrorCode::P0006, // Expected type annotation or known type name
                format!("Unknown type name '{}'.", type_token.lexeme),
                Some(loc),
            )),
        }
    }

    fn if_statement(&mut self) -> Result<Stmt, Error> {
        let if_token = self.previous().clone(); // 'if' token (already consumed)
        let loc = SourceLocation::new(if_token.line, if_token.column, 0);

        self.consume(TokenType::LeftParen, "Expect '(' after 'if'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after if condition.")?;
        
        let then_branch_start_token = self.peek().clone();
        if !self.check(TokenType::LeftBrace) {
            return Err(Error::syntax(
                ErrorCode::P0004, 
                "Expect '{' to start a block for 'then' branch.".to_string(), 
                Some(SourceLocation::new(then_branch_start_token.line, then_branch_start_token.column, 0))
            ));
        }
        // self.statement() expects a block or a single statement.
        // For if/elif/else, we mandate a block.
        // So, directly parse a block using self.block() after consuming LeftBrace within statement/block logic.
        // The current self.statement() -> self.block() if LeftBrace is fine.
        let then_stmt_outer = self.statement(SourceLocation::new(then_branch_start_token.line, then_branch_start_token.column, 0))?;
        let then_branch = match then_stmt_outer.kind {
            StmtKind::Block(_) => Box::new(then_stmt_outer),
            _ => return Err(Error::syntax(
                ErrorCode::P0004, 
                "'then' branch of an if statement must be a block.".to_string(), 
                Some(then_stmt_outer.loc)
            )),
        };

        let mut elif_branches = Vec::<(Expr, Box<Stmt>)>::new();
        while self.match_token(TokenType::Elif) {
            let _elif_token = self.previous().clone(); // Used _elif_token
            self.consume(TokenType::LeftParen, "Expect '(' after 'elif'.")?;
            let elif_condition = self.expression()?;
            self.consume(TokenType::RightParen, "Expect ')' after elif condition.")?;
            
            let elif_branch_start_token = self.peek().clone();
            if !self.check(TokenType::LeftBrace) {
                 return Err(Error::syntax(
                    ErrorCode::P0004, 
                    "Expect '{' to start a block for 'elif' branch.".to_string(), 
                    Some(SourceLocation::new(elif_branch_start_token.line, elif_branch_start_token.column, 0))
                ));
            }
            let elif_stmt_outer = self.statement(SourceLocation::new(elif_branch_start_token.line, elif_branch_start_token.column, 0))?;
            let elif_branch = match elif_stmt_outer.kind {
                StmtKind::Block(_) => Box::new(elif_stmt_outer),
                _ => return Err(Error::syntax(
                    ErrorCode::P0004, 
                    "'elif' branch must be a block.".to_string(), 
                    Some(elif_stmt_outer.loc)
                )),
            };
            elif_branches.push((elif_condition, elif_branch));
        }
        
        let else_branch = if self.match_token(TokenType::Else) {
            let else_branch_start_token = self.peek().clone();
            if !self.check(TokenType::LeftBrace) {
                 return Err(Error::syntax(
                    ErrorCode::P0004, 
                    "Expect '{' to start a block for 'else' branch.".to_string(), 
                    Some(SourceLocation::new(else_branch_start_token.line, else_branch_start_token.column, 0))
                ));
            }
            let else_stmt = self.statement(SourceLocation::new(else_branch_start_token.line, else_branch_start_token.column, 0))?;
            let else_b = match else_stmt.kind {
                StmtKind::Block(_) => Box::new(else_stmt),
                _ => return Err(Error::syntax(
                    ErrorCode::P0004, 
                    "'else' branch must be a block.".to_string(), 
                    Some(else_stmt.loc)
                )),
            };
            Some(else_b)
        } else {
            None
        };
        
        Ok(Stmt::new(StmtKind::If { condition, then_branch, elif_branches, else_branch }, loc))
    }

    // block() is called when LeftBrace is expected or already consumed (by statement() in declaration_or_statement).
    // It parses statements until RightBrace.
    // The StmtKind::Block node itself is created by the caller, which provides the location of the LeftBrace.
    fn block(&mut self) -> Result<Vec<Stmt>, Error> {
        // Assumes LeftBrace was consumed by the caller (e.g., statement() method)
        let mut statements = Vec::new();
        while !self.check(TokenType::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration_or_statement()?); // Allow declarations inside blocks
        }
        let closing_brace_token = self.peek().clone();
        self.consume(TokenType::RightBrace, "Expect '}' after block.")?;
        // To provide a better error message if is_at_end() was true before RightBrace:
        if closing_brace_token.token_type != TokenType::RightBrace && self.is_at_end() {
             return Err(Error::syntax(ErrorCode::P0001, "Unterminated block; expected '}'.".to_string(), 
                Some(SourceLocation::new(closing_brace_token.line, closing_brace_token.column, 0))));
        }
        Ok(statements)
    }

    // Pass the 'func' or 'export' token for location
    fn function_declaration(&mut self, is_exported: bool, func_keyword_token: Token) -> Result<Stmt, Error> {
        let name = self.consume(TokenType::Identifier, "Expect function name.")?.clone();
        self.consume(TokenType::LeftParen, "Expect '(' after function name.")?;

        let mut parameters: Vec<Parameter> = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                if parameters.len() >= 255 {
                    return Err(Error::syntax(
                        ErrorCode::P0008,
                        "Cannot have more than 255 parameters.".to_string(),
                        Some(SourceLocation::new(self.peek().line, self.peek().column, 0)),
                    ));
                }

                let param_name = self.consume(TokenType::Identifier, "Expect parameter name.")?.clone();
                self.consume(TokenType::Colon, "Expect ':' after parameter name.")?;
                let param_type = self.parse_type()?;
                parameters.push(Parameter { name_token: param_name, param_type });

                if !self.match_token(TokenType::Comma) {
                    break;
                }
            }
        }
        self.consume(TokenType::RightParen, "Expect ')' after parameters.")?;

        let return_type = if self.match_token(TokenType::Colon) {
            self.parse_type()?
        } else {
            Type::Int // Default to Int if no colon and type are specified
        };

        self.consume(TokenType::LeftBrace, "Expect '{' before function body.")?;
        let body_stmts = self.block()?;
        let body_loc_start = if let Some(stmt) = body_stmts.first() {
            stmt.loc.clone()
        } else {
            // Fallback if body is empty, use location of the opening brace '{'.
            // For now, use the func keyword token's location as a rough estimate.
            SourceLocation::new(func_keyword_token.line, func_keyword_token.column, 0)
        };
        let body = Stmt::new(StmtKind::Block(body_stmts), body_loc_start);

        Ok(Stmt::new(
            StmtKind::Function {
                name_token: name.clone(),
                parameters,
                return_type, // No longer Option<Type>
                body: Box::new(body),
                is_exported,
            },
            SourceLocation::new(func_keyword_token.line, func_keyword_token.column, 0),
        ))
    }

    fn return_statement(&mut self) -> Result<Stmt, Error> {
        let return_token = self.previous().clone(); // 'return' token
        let loc = SourceLocation::new(return_token.line, return_token.column, 0);

        let value = if !self.check(TokenType::Semicolon) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(TokenType::Semicolon, "Expect ';' after return value.")?;
        Ok(Stmt::new(StmtKind::Return(value), loc))
    }

    fn while_statement(&mut self) -> Result<Stmt, Error> {
        let while_token = self.previous().clone(); // 'while' token (already consumed)
        let loc = SourceLocation::new(while_token.line, while_token.column, 0);

        self.consume(TokenType::LeftParen, "Expect '(' after 'while'.")?;
        let condition = self.expression()?;
        self.consume(TokenType::RightParen, "Expect ')' after while condition.")?;
        
        let body_start_token = self.peek().clone();
        if !self.check(TokenType::LeftBrace) {
             return Err(Error::syntax(
                ErrorCode::P0004, // Expected statement (block for while)
                "Expect '{' to start a block for while loop body.".to_string(), 
                Some(SourceLocation::new(body_start_token.line, body_start_token.column, 0))
            ));
        }
        let body_stmt = self.statement(SourceLocation::new(body_start_token.line, body_start_token.column, 0))?; // self.statement() will parse the block
        let body = match body_stmt.kind {
            StmtKind::Block(_) => Box::new(body_stmt),
            _ => return Err(Error::syntax(
                ErrorCode::P0004, 
                "While loop body must be a block statement.".to_string(), 
                Some(body_stmt.loc)
            ))
        };

        Ok(Stmt::new(StmtKind::While { condition, body }, loc))
    }

    fn for_statement(&mut self) -> Result<Stmt, Error> {
        let for_token = self.previous().clone(); // 'for' token (already consumed)
        let loc = SourceLocation::new(for_token.line, for_token.column, 0);

        self.consume(TokenType::LeftParen, "Expect '(' after 'for'.")?;
        
        let initializer: Option<Box<Stmt>>;
        if self.match_token(TokenType::Semicolon) { // Empty initializer
            initializer = None;
        } else if self.check(TokenType::Let) { // Check for 'let' without consuming
            self.advance(); // Consume the 'Let' token, making it `previous`.
            let var_decl_stmt = self.var_declaration()?; // var_declaration expects 'let' to be previous
            initializer = Some(Box::new(var_decl_stmt));
        } else { // Initializer is an expression
            let expr = self.expression()?;
            let expr_loc = expr.loc.clone();
            self.consume(TokenType::Semicolon, "Expect ';' after for loop initializer expression.")?;
            initializer = Some(Box::new(Stmt::new(StmtKind::Expression(expr), expr_loc)));
        };

        let condition: Option<Expr>;
        if !self.check(TokenType::Semicolon) {
            condition = Some(self.expression()?);
        } else {
            condition = None; // No condition means true, effectively
        };
        self.consume(TokenType::Semicolon, "Expect ';' after for loop condition.")?;
        
        let increment: Option<Box<Expr>>;
        if !self.check(TokenType::RightParen) {
            increment = Some(Box::new(self.expression()?));
        } else {
            increment = None;
        };
        self.consume(TokenType::RightParen, "Expect ')' after for clauses.")?;
        
        let body_start_token = self.peek().clone();
        if !self.check(TokenType::LeftBrace) {
             return Err(Error::syntax(
                ErrorCode::P0004, 
                "Expect '{' to start a block for for loop body.".to_string(), 
                Some(SourceLocation::new(body_start_token.line, body_start_token.column, 0))
            ));
        }
        let body_stmt = self.statement(SourceLocation::new(body_start_token.line, body_start_token.column, 0))?; // self.statement() will parse the block
        let body = match body_stmt.kind {
            StmtKind::Block(_) => Box::new(body_stmt),
            _ => return Err(Error::syntax(
                ErrorCode::P0004, 
                "For loop body must be a block statement.".to_string(), 
                Some(body_stmt.loc)
            ))
        };
        
        Ok(Stmt::new(StmtKind::For { initializer, condition, increment, body }, loc))
    }

    fn break_statement(&mut self) -> Result<Stmt, Error> {
        let break_token = self.previous().clone(); // 'break' token (already consumed)
        let loc = SourceLocation::new(break_token.line, break_token.column, 0);
        // Check if we are inside a loop would be a semantic check for the interpreter or a later parser pass.
        // For now, just parse it. Error P0012 (Break outside loop) could be used if context is tracked.
        self.consume(TokenType::Semicolon, "Expect ';' after 'break'.")?;
        Ok(Stmt::new(StmtKind::Break(break_token), loc))
    }

    fn continue_statement(&mut self) -> Result<Stmt, Error> {
        let continue_token = self.previous().clone(); // 'continue' token (already consumed)
        let loc = SourceLocation::new(continue_token.line, continue_token.column, 0);
        // Similar to break, P0013 (Continue outside loop) is for contextual check.
        self.consume(TokenType::Semicolon, "Expect ';' after 'continue'.")?;
        Ok(Stmt::new(StmtKind::Continue(continue_token), loc))
    }

    fn including_statement(&mut self) -> Result<Stmt, Error> {
        let including_keyword_token = self.previous().clone();
        let loc = SourceLocation::new(including_keyword_token.line, including_keyword_token.column, 0);

        let mut imports_list: Option<Vec<String>> = None;
        let path_literal_token: Token;
        let path_str_value: String;

        if self.match_token(TokenType::LeftBracket) {
            let mut parsed_imports = Vec::new();
            if !self.check(TokenType::RightBracket) {
                loop {
                    // Parse first component of a qualified name
                    let first_component_token = self.advance().clone();
                    let mut current_import_path = match first_component_token.token_type {
                        TokenType::Identifier | TokenType::Sqrt | TokenType::Float | TokenType::Int | TokenType::String | TokenType::Bool | TokenType::Obj | TokenType::List => first_component_token.lexeme,
                        _ => return Err(Error::syntax(ErrorCode::P0002, "Expect identifier or valid keyword for import path component.".to_string(), Some(SourceLocation::new(first_component_token.line, first_component_token.column, 0))))
                    };
                    
                    while self.match_token(TokenType::ColonColon) {
                        current_import_path.push_str("::");
                        // Parse subsequent components
                        let next_component_token = self.advance().clone();
                        let next_component_lexeme = match next_component_token.token_type {
                            TokenType::Identifier | TokenType::Sqrt | TokenType::Float | TokenType::Int | TokenType::String | TokenType::Bool | TokenType::Obj | TokenType::List => next_component_token.lexeme,
                            _ => return Err(Error::syntax(ErrorCode::P0002, "Expect identifier or valid keyword after '::' in import path.".to_string(), Some(SourceLocation::new(next_component_token.line, next_component_token.column, 0))))
                        };
                        current_import_path.push_str(&next_component_lexeme);
                    }
                    parsed_imports.push(current_import_path);

                    if !self.match_token(TokenType::Comma) {
                        break;
                    }
                    if self.check(TokenType::RightBracket) { // Allow trailing comma
                        break;
                    }
                }
            }
            self.consume(TokenType::RightBracket, "Expect ']' after import list.")?;
            imports_list = Some(parsed_imports);

            self.consume(TokenType::From, "Expect 'from' after import list.")?;
            path_literal_token = self.consume(TokenType::StringLiteral, "Expect file path (string literal) after 'from'.")?.clone();
            path_str_value = path_literal_token.string_value();
        } else {
            path_literal_token = self.consume(TokenType::StringLiteral, "Expect file path (string literal) after 'including'.")?.clone();
            path_str_value = path_literal_token.string_value();
        }

        self.consume(TokenType::Semicolon, "Expect ';' after including statement.")?;
        Ok(Stmt::new(
            StmtKind::Including { 
                path_token: path_literal_token, 
                path_val: path_str_value,       
                imports: imports_list 
            },
            loc 
        ))
    }

    fn expression(&mut self) -> Result<Expr, Error> {
        self.assignment()
    }

    fn assignment(&mut self) -> Result<Expr, Error> {
        let expr = self.logical_or()?;

        if self.match_token(TokenType::Equal) {
            let equals_token = self.previous().clone();
            let value = self.assignment()?;

            let assign_loc = SourceLocation::new(equals_token.line, equals_token.column, 0);

            match expr.kind {
                ExprKind::Variable(name_token) => {
                    Ok(Expr::new(
                        ExprKind::Assign { name_token: name_token.clone(), value: Box::new(value) },
                        assign_loc,
                    ))
                }
                ExprKind::Get { object, name } => {
                    Ok(Expr::new(
                        ExprKind::Set { object, name: name.clone(), value: Box::new(value) },
                        assign_loc,
                    ))
                }
                _ => {
                    Err(Error::syntax(
                        ErrorCode::P0007,
                        "Invalid assignment target.".to_string(),
                        Some(SourceLocation::new(equals_token.line, equals_token.column, 0)),
                    ))
                }
            }
        } else {
            Ok(expr)
        }
    }

    fn logical_or(&mut self) -> Result<Expr, Error> {
        let mut expr = self.logical_and()?;

        while self.match_token(TokenType::PipePipe) {
            let operator = self.previous().clone();
            let right = self.logical_and()?;
            let loc = SourceLocation::new(operator.line, operator.column, 0);
            expr = Expr::new(ExprKind::Logical(Box::new(expr), operator, Box::new(right)), loc);
        }

        Ok(expr)
    }

    fn logical_and(&mut self) -> Result<Expr, Error> {
        let mut expr = self.equality()?;

        while self.match_token(TokenType::AmpersandAmpersand) {
            let operator = self.previous().clone();
            let right = self.equality()?;
            let loc = SourceLocation::new(operator.line, operator.column, 0);
            expr = Expr::new(ExprKind::Logical(Box::new(expr), operator, Box::new(right)), loc);
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr, Error> {
        let mut expr = self.comparison()?;

        while self.match_tokens(&[TokenType::EqualEqual, TokenType::BangEqual]) {
            let operator = self.previous().clone();
            let right = self.comparison()?;
            let loc = SourceLocation::new(operator.line, operator.column, 0);
            expr = Expr::new(ExprKind::Binary(Box::new(expr), operator, Box::new(right)), loc);
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
            let loc = SourceLocation::new(operator.line, operator.column, 0);
            expr = Expr::new(ExprKind::Binary(Box::new(expr), operator, Box::new(right)), loc);
        }

        Ok(expr)
    }

    fn term(&mut self) -> Result<Expr, Error> {
        let mut expr = self.factor()?;

        while self.match_tokens(&[TokenType::Plus, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.factor()?;
            let loc = SourceLocation::new(operator.line, operator.column, 0);
            expr = Expr::new(ExprKind::Binary(Box::new(expr), operator, Box::new(right)), loc);
        }

        Ok(expr)
    }

    fn factor(&mut self) -> Result<Expr, Error> {
        let mut expr = self.unary()?;

        while self.match_tokens(&[TokenType::Star, TokenType::Slash, TokenType::StarStar, TokenType::Modulo]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            let loc = SourceLocation::new(operator.line, operator.column, 0);
            expr = Expr::new(ExprKind::Binary(Box::new(expr), operator, Box::new(right)), loc);
        }

        Ok(expr)
    }

    fn unary(&mut self) -> Result<Expr, Error> {
        if self.match_tokens(&[TokenType::Bang, TokenType::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            let loc = SourceLocation::new(operator.line, operator.column, 0);
            Ok(Expr::new(ExprKind::Unary(operator, Box::new(right)), loc))
        } else {
            self.call_or_method_access()
        }
    }

    fn call_or_method_access(&mut self) -> Result<Expr, Error> {
        let mut expr = self.primary()?;
        let expr_loc_start = expr.loc.clone(); // For overall expression loc

        loop {
            if self.match_token(TokenType::LeftParen) {
                expr = self.finish_function_call(expr)?;
            } else if self.match_token(TokenType::Dot) {
                let name_token: Token;
                if self.check(TokenType::Identifier) {
                    name_token = self.advance().clone();
                } else if self.check(TokenType::Sqrt) { // Allow 'sqrt' keyword as a property/method name
                    name_token = self.advance().clone();
                } else {
                    // Fallback to original error: "Expect property name after '.'"
                    name_token = self.consume(TokenType::Identifier, "Expect property name after '.'.")?.clone();
                }
                let _access_loc = SourceLocation::new(name_token.line, name_token.column, 0); 

                if self.match_token(TokenType::LeftParen) { // Method call: object.name(...)
                    let arguments = self.parse_argument_list()?;
                    self.consume(TokenType::RightParen, "Expect ')' after method arguments.")?;
                    // The MethodCall expression spans from the start of the object to the closing parenthesis.
                    // For point-like SourceLocation, use the start of the object.
                    expr = Expr::new(ExprKind::MethodCall {
                        object: Box::new(expr),       // expr is the object part
                        method_name_token: name_token, // token for the method name
                        arguments: arguments,
                    }, expr_loc_start.clone());     // Location is the start of the object expression
                } else { // Property access: object.name
                    // For point-like SourceLocation, use the start of the object.
                    expr = Expr::new(ExprKind::Get {
                        object: Box::new(expr),
                        name: name_token,
                    }, expr_loc_start.clone());     // Location is the start of the object expression
                }
            } else {
                break;
            }
        }
        Ok(expr)
    }

    fn finish_function_call(&mut self, callee: Expr) -> Result<Expr, Error> {
        let call_loc = callee.loc.clone();

        let mut arguments = Vec::new();
        if !self.check(TokenType::RightParen) {
            arguments = self.parse_argument_list()?;
        }
        self.consume(TokenType::RightParen, "Expect ')' after arguments.")?;

        match callee.kind {
            ExprKind::Variable(name_token) => {
                 Ok(Expr::new(ExprKind::Call { name_token, arguments }, call_loc))
            }
            ExprKind::Get { object, name } => {
                 Ok(Expr::new(ExprKind::MethodCall { object, method_name_token: name, arguments }, call_loc))
            }
            _ => Err(Error::syntax(
                ErrorCode::R0004,
                "Expression is not callable.".to_string(),
                Some(callee.loc),
            )),
        }
    }
    
    fn parse_argument_list(&mut self) -> Result<Vec<Expr>, Error> {
        let mut arguments = Vec::new();
        if !self.check(TokenType::RightParen) {
            loop {
                arguments.push(self.expression()?);
                if !self.match_token(TokenType::Comma) {
                    break;
                }
                if self.check(TokenType::RightParen) {
                    break;
                }
            }
        }
        if arguments.len() > 255 {
            let err_loc = arguments.last().map_or_else(
                || SourceLocation::new(self.peek().line, self.peek().column, 0),
                |arg| arg.loc.clone()
            );
            return Err(Error::syntax(ErrorCode::P0008, "Cannot have more than 255 arguments.".to_string(), Some(err_loc)));
        }
        Ok(arguments)
    }

    fn primary(&mut self) -> Result<Expr, Error> {
        let token = self.peek().clone();
        let loc = SourceLocation::new(token.line, token.column, 0);

        if self.match_token(TokenType::False) {
            return Ok(Expr::new(ExprKind::Literal(Value::Bool(false)), loc));
        }
        if self.match_token(TokenType::True) {
            return Ok(Expr::new(ExprKind::Literal(Value::Bool(true)), loc));
        }
        if self.match_token(TokenType::Null) {
            return Ok(Expr::new(ExprKind::Literal(Value::Null), loc));
        }
        if self.match_token(TokenType::IntLiteral) {
            let literal_token = self.previous().clone();
            match literal_token.lexeme.parse::<i64>() {
                Ok(val) => return Ok(Expr::new(ExprKind::Literal(Value::Int(val)), loc)),
                Err(_) => {
                    return Err(Error::syntax(
                        ErrorCode::P0001, // Invalid literal format
                        format!("Invalid integer literal format: '{}'.", literal_token.lexeme),
                        Some(loc),
                    ));
                }
            }
        }
        if self.match_token(TokenType::FloatLiteral) {
            let literal_token = self.previous().clone();
            match literal_token.lexeme.parse::<f64>() {
                Ok(val) => return Ok(Expr::new(ExprKind::Literal(Value::Float(val)), loc)),
                Err(_) => {
                    return Err(Error::syntax(
                        ErrorCode::P0001, // Invalid literal format
                        format!("Invalid float literal format: '{}'.", literal_token.lexeme),
                        Some(loc),
                    ));
                }
            }
        }
        if self.match_token(TokenType::StringLiteral) {
            let literal_token = self.previous().clone();
            // Use the processed string value (unescaped and without quotes) from the token.
            return Ok(Expr::new(ExprKind::Literal(Value::String(literal_token.string_value())), loc));
        }

        // Check for Identifier first due to specific ++/-- logic that only applies to Identifiers.
        if self.match_token(TokenType::Identifier) {
            let token = self.previous().clone(); // This is the Identifier token
            // 'loc' is already defined at the start of primary() as the location of this token.

            // Postfix increment/decrement only for true identifiers
            if self.match_token(TokenType::PlusPlus) { // match_token advances
                let op_token = self.previous().clone(); // op_token is '++'
                let op_loc = SourceLocation::new(op_token.line, op_token.column, 0);
                // ExprKind::Increment takes the variable token. Its loc should be the operator.
                return Ok(Expr::new(ExprKind::Increment(token), op_loc));
            }
            if self.match_token(TokenType::MinusMinus) { // match_token advances
                let op_token = self.previous().clone(); // op_token is '--'
                let op_loc = SourceLocation::new(op_token.line, op_token.column, 0);
                return Ok(Expr::new(ExprKind::Decrement(token), op_loc));
            }
            // If not inc/dec, it's a plain variable.
            return Ok(Expr::new(ExprKind::Variable(token), loc)); // loc is the Identifier's loc
        }

        // Now check for built-in function keywords that can act as primaries for calls
        // These are TokenType::Inputln, TokenType::ToInt, TokenType::ToBool, TokenType::ToString, TokenType::ToFloat, TokenType::Sqrt
        if self.match_tokens(&[TokenType::Inputln, TokenType::ToInt, TokenType::ToBool, TokenType::ToString, TokenType::ToFloat, TokenType::Sqrt]) {
            let token = self.previous().clone(); 
            // These are treated like a variable name that will be called.
            // `loc` here refers to the location of the keyword token itself.
            return Ok(Expr::new(ExprKind::Variable(token), loc)); // loc is the keyword's loc
        }

        if self.match_token(TokenType::LeftParen) {
            let expr = self.expression()?;
            self.consume(TokenType::RightParen, "Expect ')' after expression.")?;
            return Ok(Expr::new(ExprKind::Grouping(Box::new(expr)), loc));
        }
        if self.match_token(TokenType::LeftBracket) {
            let opening_bracket_loc = SourceLocation::new(self.previous().line, self.previous().column, 0);
            let mut elements = Vec::new();
            if !self.check(TokenType::RightBracket) {
                loop {
                    elements.push(self.expression()?);
                    if !self.match_token(TokenType::Comma) {
                        break;
                    }
                    if self.check(TokenType::RightBracket) {
                        break;
                    }
                }
            }
            self.consume(TokenType::RightBracket, "Expect ']' after list elements.")?;
            return Ok(Expr::new(ExprKind::ListLiteral(elements), opening_bracket_loc));
        }

        if self.match_token(TokenType::LeftBrace) {
            let opening_brace_loc = SourceLocation::new(self.previous().line, self.previous().column, 0);
            let mut properties = Vec::new();
            if !self.check(TokenType::RightBrace) {
                loop {
                    let key_token = self.consume(TokenType::Identifier, "Expect property name (identifier) in object literal.")?.clone();
                    self.consume(TokenType::Colon, "Expect ':' after property name.")?;
                    let value_expr = self.expression()?;
                    properties.push((key_token, value_expr));

                    if !self.match_token(TokenType::Comma) {
                        break;
                    }
                    if self.check(TokenType::RightBrace) {
                        break;
                    }
                }
            }
            self.consume(TokenType::RightBrace, "Expect '}' after object properties.")?;
            return Ok(Expr::new(ExprKind::ObjectLiteral { properties }, opening_brace_loc));
        }

        let unexpected_token = self.peek().clone();
        Err(Error::syntax(
            ErrorCode::P0002,
            format!("Unexpected token '{}'. Expect expression at line {}.", unexpected_token.lexeme, unexpected_token.line),
            Some(SourceLocation::new(unexpected_token.line, unexpected_token.column, 0)),
        ))
    }

    fn consume(&mut self, token_type: TokenType, message: &str) -> Result<&Token, Error> {
        if self.check(token_type) {
            Ok(self.advance())
        } else {
            let peeked_token = self.peek().clone(); // Clone to get ownership for location
            Err(Error::syntax(
                ErrorCode::P0002, // Expected token
                message.to_string(),
                Some(SourceLocation::new(peeked_token.line, peeked_token.column, 0)),
            ))
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
}