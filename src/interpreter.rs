use crate::ast::{Expr, ExprKind, Program, Stmt, StmtKind, Parameter};
use crate::error::{Error, ErrorCode, SourceLocation};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::{TokenType, Token};
use crate::types::Type;
use crate::value::{Value, ListRef, ObjectRef};
use colored::Colorize;
use std::collections::{HashMap};
use std::{fs};
use std::path::{PathBuf};
use std::rc::Rc;
use std::cell::RefCell;
use std::io::{self, Write};

/// Represents a variable with its type, value, and mutability
#[derive(Clone, Debug)]
struct Variable {
    /// The type of the variable
    var_type: Type,
    /// The current value of the variable
    value: Value,
    /// Whether the variable can be reassigned
    is_mutable: bool,
}

/// Represents a user-defined function
#[derive(Clone, Debug)]
struct Function {
    /// The name of the function
    name: String,
    /// The function's parameters
    parameters: Vec<Parameter>,
    /// The function's return type
    return_type: Type,
    /// The function's body
    body: Box<Stmt>,
    /// Whether the function is exported from its module
    is_exported: bool,
}

#[derive(Clone, Debug)]
struct StructDef {
    name: String,
    fields: HashMap<String, Type>
}

/// Interpreter for the Lowland language
pub struct Interpreter {
    /// Environment for storing variables
    environment: HashMap<String, Variable>,
    
    /// Functions defined in the program
    functions: HashMap<String, Function>,

    /// The absolute path of the file currently being interpreted (if any)
    current_file_path: Option<PathBuf>,

    /// Cache for module exports. Maps absolute file path to its exported functions.
    module_cache: Rc<RefCell<HashMap<PathBuf, Vec<Function>>>>,

    /// Stores imported standard library symbols, mapping alias to canonical name
    imported_std_symbols: HashMap<String, String>,
}

impl Interpreter {
    /// Creates a new interpreter instance with empty environment and function tables
    pub fn new() -> Self {
        let mut interpreter = Interpreter {
            environment: HashMap::new(),
            functions: HashMap::new(),
            current_file_path: None,
            module_cache: Rc::new(RefCell::new(HashMap::new())),
            imported_std_symbols: HashMap::new(),
        };
        
        // Initialize standard library symbols
        interpreter.init_std_library();
        
        interpreter
    }

    /// Main entry point for interpreting a file
    /// 
    /// # Arguments
    /// * `file_path_str` - Path to the file to interpret
    /// 
    /// # Returns
    /// * `Result<Value, Error>` - The result of interpreting the file
    /// 
    /// # Errors
    /// * `Error::io` - If the file cannot be read or accessed
    pub fn interpret_file(&mut self, file_path_str: &str) -> Result<Value, Error> {
        // Convert string path to PathBuf and get absolute path
        let initial_path = PathBuf::from(file_path_str);
        let canonical_path = fs::canonicalize(&initial_path).map_err(|e| Error::io(
            ErrorCode::I0001,
            format!("Error accessing file {}: {}", file_path_str, e),
            None,
        ))?;

        // Read the file contents
        let source = fs::read_to_string(&canonical_path).map_err(|e| Error::io(
            ErrorCode::I0003,
            format!("Error reading file {}: {}", canonical_path.display(), e),
            None,
        ))?;
        
        // Clear the module cache before interpreting
        {
            let mut module_cache_refmut = (*self.module_cache).borrow_mut();
            module_cache_refmut.clear();
        }
        
        // Interpret the source code and handle any defined functions
        self.interpret_source_with_path(&source, Some(canonical_path.clone()))
            .map(|(val, defined_functions)| {
                // Register all functions defined in this file
                for func_def in defined_functions {
                    self.functions.insert(func_def.name.clone(), func_def);
                }
                val
            })
    }

    /// Helper method to create a SourceLocation from a Token
    fn token_to_location(token: &Token) -> SourceLocation {
        SourceLocation::new(token.line, token.column, 0)
    }

    /// Helper method to create an error with proper location information
    fn create_error(&self, code: ErrorCode, message: String, location: Option<SourceLocation>) -> Error {
        Error::runtime(code, message, location)
    }

    /// Helper method to create a type error with proper location information
    fn create_type_error(&self, code: ErrorCode, message: String, location: Option<SourceLocation>) -> Error {
        Error::type_error(code, message, location)
    }

    /// Interpret a source string, typically for REPL or embedded execution
    /// This is used for interactive mode or when evaluating code strings
    pub fn interpret_repl_input(&mut self, source: &str) -> Result<Value, Error> {
        self.interpret_source_with_path(source, self.current_file_path.clone())
            .map(|(val, _funcs)| val)
    }

    /// Core interpretation logic for a given source string and its optional path
    /// This is the main method that processes source code into executable form
    /// 
    /// Steps:
    /// 1. Save and update current file path
    /// 2. Lex the source into tokens
    /// 3. Parse tokens into AST
    /// 4. Execute the program
    /// 5. Restore original file path
    fn interpret_source_with_path(&mut self, source: &str, path: Option<PathBuf>) -> Result<(Value, Vec<Function>), Error> {
        // Save current path and update to new path
        let old_path = self.current_file_path.clone();
        self.current_file_path = path;

        // Lexical analysis - convert source to tokens
        let mut lexer = Lexer::new(source);
        let tokens = lexer.scan_tokens()?;

        // Parsing - convert tokens to AST
        let mut parser = Parser::new(tokens);
        let program = parser.parse()?;

        // Execute the program
        let result = self.execute_program(&program)?;

        // Restore original path
        self.current_file_path = old_path;
        Ok(result)
    }

    /// Execute a whole program
    /// This processes all statements in the program and handles function definitions
    /// 
    /// The execution happens in two passes:
    /// 1. First pass: Collect all function definitions
    /// 2. Second pass: Execute all statements
    fn execute_program(&mut self, program: &Program) -> Result<(Value, Vec<Function>), Error> {
        let mut last_value = Value::Null;
        let mut defined_functions_in_program = Vec::new();

        // First pass: Collect all function definitions
        for stmt_node in &program.statements {
            if let StmtKind::Function { name_token, parameters, return_type, body, is_exported } = &stmt_node.kind {
                let func_def = Function {
                    name: name_token.lexeme.clone(),
                    parameters: parameters.clone(),
                    return_type: return_type.clone(),
                    body: body.clone(),
                    is_exported: *is_exported,
                };
                // Register function immediately so it's available for calls within this program
                self.functions.insert(func_def.name.clone(), func_def.clone());
                defined_functions_in_program.push(func_def);
            }
        }

        // Second pass: Execute all statements
        for stmt_node in &program.statements { 
            // Skip function definitions as they were handled in first pass
            if let StmtKind::Function {..} = &stmt_node.kind {
                continue; 
            }

            // Execute statement and handle control flow
            match self.execute_statement(stmt_node) {
                Ok(value) => last_value = value,
                Err(err) => {
                    // Handle different types of control flow
                    match err {
                        Error::ReturnControlFlow(_) => return Err(err), // Function return
                        Error::BreakControlFlow => return Err(err),    // Loop break
                        Error::ContinueControlFlow => return Err(err), // Loop continue
                        _ => return Err(err), // Other errors are fatal
                    }
                }
            }
        }
        Ok((last_value, defined_functions_in_program))
    }

    /// Execute a single statement
    /// This method handles all types of statements in the language:
    /// - Variable declarations
    /// - Function definitions
    /// - Control flow (if/else, loops)
    /// - Expression statements
    /// - Print statements
    /// - Import statements
    fn execute_statement(&mut self, stmt_node: &Stmt) -> Result<Value, Error> {
        match &stmt_node.kind {
            // Handle expression statements (e.g., function calls, assignments)
            StmtKind::Expression(expr) => self.evaluate_expression(expr),

            // Handle print statements with newline
            StmtKind::Println(exprs) => {
                let mut values_to_print = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    values_to_print.push(self.evaluate_expression(expr)?);
                }
                if values_to_print.is_empty() {
                    println!();
                } else {
                    let formatted_values: Vec<String> = values_to_print.iter()
                        .map(|v| v.to_string_value().unwrap_or_else(|_| "<unprintable>".to_string()))
                        .collect();
                    println!("{}", formatted_values.join(" ").green());
                }
                Ok(Value::Null)
            },

            // Handle raw print statements (no formatting)
            StmtKind::PrintRaw(exprs) => {
                let mut raw_values_to_print = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    raw_values_to_print.push(self.evaluate_expression(expr)?);
                }
                if raw_values_to_print.is_empty() {
                    println!();
                } else {
                    let formatted_raw_values: Vec<String> = raw_values_to_print.iter()
                        .map(|v| v.to_raw_display_string().unwrap_or_else(|_| "<unprintable_raw>".to_string()))
                        .collect();
                    println!("{}", formatted_raw_values.join(" "));
                }
                Ok(Value::Null)
            },

            // Handle variable declarations
            StmtKind::Var { name_token, var_type, initializer, is_mutable } => {
                // Evaluate initializer expression if present
                let value = match initializer {
                    Some(init_expr) => self.evaluate_expression(init_expr)?,
                    None => Value::Null, // Default to null if no initializer
                };
                
                // Type checking using the helper method
                let loc = initializer.as_ref().map_or_else(|| stmt_node.loc.clone(), |e| e.loc.clone());
                self.check_type_compatibility(var_type, &value, Some(loc))?;
                
                // Store variable in environment
                self.environment.insert(
                    name_token.lexeme.clone(),
                    Variable {
                        var_type: var_type.clone(),
                        value,
                        is_mutable: *is_mutable,
                    },
                );
                Ok(Value::Null)
            },

            // Handle block statements (e.g., function bodies, if blocks)
            StmtKind::Block(statements) => {
                // Save current environment for restoration
                let environment_at_block_start = self.environment.clone();
                let mut last_block_value = Value::Null;

                // Execute each statement in the block
                for inner_stmt_node in statements {
                    match self.execute_statement(inner_stmt_node) {
                        Ok(val) => last_block_value = val,
                        // Handle control flow statements
                        Err(Error::ReturnControlFlow(ret_val)) => {
                            self.environment = environment_at_block_start;
                            return Err(Error::ReturnControlFlow(ret_val));
                        },
                        Err(Error::BreakControlFlow) => {
                            self.environment = environment_at_block_start;
                            return Err(Error::BreakControlFlow);
                        },
                        Err(Error::ContinueControlFlow) => {
                            self.environment = environment_at_block_start;
                            return Err(Error::ContinueControlFlow);
                        },
                        Err(true_err) => {
                            self.environment = environment_at_block_start;
                            return Err(true_err);
                        }
                    }
                }
                Ok(last_block_value)
            },

            // Handle if/elif/else statements
            StmtKind::If { condition, then_branch, elif_branches, else_branch } => {
                // Evaluate condition
                let cond_value = self.evaluate_expression(condition)?;
                let cond_loc = condition.loc.clone();

                if let Value::Bool(cond_bool) = cond_value {
                    if cond_bool {
                        // Execute then branch if condition is true
                        self.execute_statement(then_branch)
                    } else {
                        // Try elif branches
                        for (elif_cond_expr, elif_branch_stmt) in elif_branches {
                            let elif_cond_value = self.evaluate_expression(elif_cond_expr)?;
                            let elif_cond_loc = elif_cond_expr.loc.clone();
                            if let Value::Bool(elif_bool) = elif_cond_value {
                                if elif_bool {
                                    // Execute elif branch directly and return
                                    return self.execute_statement(elif_branch_stmt);
                                }
                            } else {
                                return Err(Error::type_error(
                                    ErrorCode::T0005,
                                    format!("Elif condition must be a boolean, found {}.", elif_cond_value.get_type()),
                                    Some(elif_cond_loc),
                                ));
                            }
                        }
                        // Execute else branch if no elif conditions were true
                        if else_branch.is_some() {
                            self.execute_statement(else_branch.as_ref().unwrap())
                        } else {
                            Ok(Value::Null)
                        }
                    }
                } else {
                    Err(Error::type_error(
                        ErrorCode::T0005,
                        format!("If condition must be a boolean, found {}.", cond_value.get_type()),
                        Some(cond_loc),
                    ))
                }
            },
            
            // Handle function definitions
            StmtKind::Function { name_token: _, parameters: _, return_type: _, body: _, is_exported: _ } => {
                // We don't actually execute anything here, as functions are registered during program execution
                // Just return null as the result of defining a function
                Ok(Value::Null)
            },
            
            // Handle return statement
            StmtKind::Return(return_value) => {
                // Evaluate return expression if present
                let return_value = match return_value {
                    Some(expr) => self.evaluate_expression(expr)?,
                    None => Value::Null,
                };
                Err(Error::ReturnControlFlow(Box::new(return_value)))
            },

            // Handle while loops
            StmtKind::While { condition, body } => {
                // Evaluate condition and execute body in a loop
                loop {
                    // Evaluate condition
                    let cond_value = self.evaluate_expression(condition)?;
                    
                    // Check if condition is boolean
                    let should_continue = match cond_value {
                        Value::Bool(b) => b,
                        _ => return Err(Error::type_error(
                            ErrorCode::T0005,
                            format!("While condition must be a boolean, found {}.", cond_value.get_type()),
                            Some(condition.loc.clone())
                        ))
                    };

                    // Break if condition is false
                    if !should_continue {
                        break;
                    }

                    // Execute body and handle control flow
                    match self.execute_statement(body) {
                        Ok(_) => (), // Continue loop
                        Err(Error::BreakControlFlow) => break,
                        Err(Error::ContinueControlFlow) => continue,
                        Err(e) => return Err(e), // Propagate other errors
                    }
                }
                Ok(Value::Null)
            },

            // Handle for loops
            StmtKind::For { initializer, condition, increment, body } => {
                // Execute initializer if present
                if let Some(init) = initializer {
                    self.execute_statement(init)?;
                }

                // Main loop
                loop {
                    // Check condition if present
                    if let Some(cond) = condition {
                        let cond_value = self.evaluate_expression(cond)?;
                        let should_continue = match cond_value {
                            Value::Bool(b) => b,
                            _ => return Err(Error::type_error(
                                ErrorCode::T0005,
                                format!("For condition must be a boolean, found {}.", cond_value.get_type()),
                                Some(cond.loc.clone())
                            ))
                        };
                        if !should_continue {
                            break;
                        }
                    }

                    // Execute body and handle control flow
                    match self.execute_statement(body) {
                        Ok(_) => (), // Continue loop
                        Err(Error::BreakControlFlow) => break,
                        Err(Error::ContinueControlFlow) => (), // Continue to increment
                        Err(e) => return Err(e), // Propagate other errors
                    }

                    // Execute increment if present
                    if let Some(inc) = increment {
                        self.evaluate_expression(inc)?;
                    }
                }
                Ok(Value::Null)
            },

            // Handle break statement
            StmtKind::Break(_) => Err(Error::BreakControlFlow),

            // Handle continue statement
            StmtKind::Continue(_) => Err(Error::ContinueControlFlow),

            // Handle including statement
            StmtKind::Including { path_token, path_val, imports } => {
                // Resolve file path based on current file's directory
                let path_to_include = if let Some(current_path) = &self.current_file_path {
                    // Get parent directory of current file
                    let parent_dir = current_path.parent().unwrap_or_else(|| std::path::Path::new("."));
                    parent_dir.join(&path_val)
                } else {
                    // If no current file path (e.g., REPL), use path as is
                    PathBuf::from(&path_val)
                };

                // Try to canonicalize the path
                let canonical_path = match fs::canonicalize(&path_to_include) {
                    Ok(p) => p,
                    Err(e) => return Err(Error::io(
                        ErrorCode::I0001,
                        format!("Error resolving path '{}': {}", path_val, e),
                        Some(SourceLocation::new(path_token.line, path_token.column, 0)),
                    )),
                };

                // Check if module is already in cache
                let cached_exports = {
                    let cache = self.module_cache.borrow();
                    cache.get(&canonical_path).cloned()
                };

                let module_exports = if let Some(exports) = cached_exports {
                    // Use cached exports
                    exports
                } else {
                    // Read and interpret the file
                    let source = match fs::read_to_string(&canonical_path) {
                        Ok(content) => content,
                        Err(e) => return Err(Error::io(
                            ErrorCode::I0003,
                            format!("Error reading file '{}': {}", canonical_path.display(), e),
                            Some(SourceLocation::new(path_token.line, path_token.column, 0)),
                        )),
                    };

                    // Save current state
                    let old_path = self.current_file_path.clone();
                    let old_env = self.environment.clone();
                    let old_functions = self.functions.clone();

                    // Set up new state for included file
                    self.current_file_path = Some(canonical_path.clone());
                    self.environment.clear();
                    self.functions.clear();

                    // Parse and execute the included file
                    let mut lexer = Lexer::new(&source);
                    let tokens = match lexer.scan_tokens() {
                        Ok(tokens) => tokens,
                        Err(e) => {
                            // Restore state on error
                            self.current_file_path = old_path;
                            self.environment = old_env;
                            self.functions = old_functions;
                            return Err(e);
                        },
                    };

                    let mut parser = Parser::new(tokens);
                    let program = match parser.parse() {
                        Ok(program) => program,
                        Err(e) => {
                            // Restore state on error
                            self.current_file_path = old_path;
                            self.environment = old_env;
                            self.functions = old_functions;
                            return Err(e);
                        },
                    };

                    // Execute the included program
                    let (_, defined_functions) = match self.execute_program(&program) {
                        Ok(result) => result,
                        Err(e) => {
                            // Restore state on error
                            self.current_file_path = old_path;
                            self.environment = old_env;
                            self.functions = old_functions;
                            return Err(e);
                        },
                    };

                    // Extract exported functions
                    let exports: Vec<Function> = defined_functions.into_iter()
                        .filter(|f| f.is_exported)
                        .collect();

                    // Cache the exports
                    {
                        let mut cache = self.module_cache.borrow_mut();
                        cache.insert(canonical_path.clone(), exports.clone());
                    }

                    // Restore previous state
                    self.current_file_path = old_path;
                    self.environment = old_env;
                    self.functions = old_functions;

                    exports
                };

                // Process imported functions based on imports parameter
                match &imports {
                    Some(import_list) => {
                        // Selective import of specified functions
                        for import_name in import_list {
                            let found = module_exports.iter().find(|f| f.name == *import_name);
                            
                            if let Some(function) = found {
                                self.functions.insert(function.name.clone(), function.clone());
                            } else {
                                return Err(Error::runtime(
                                    ErrorCode::R0004,
                                    format!("Exported function '{}' not found in '{}'", import_name, path_val),
                                    Some(SourceLocation::new(path_token.line, path_token.column, 0)),
                                ));
                            }
                        }
                    },
                    None => {
                        // Import all exported functions
                        for function in module_exports {
                            self.functions.insert(function.name.clone(), function.clone());
                        }
                    }
                }

                Ok(Value::Null)
            },
        }
    }

    /// Evaluate an expression to a value
    /// This method handles all types of expressions in the language:
    /// - Literals (numbers, strings, booleans)
    /// - Variables
    /// - Binary operations (+, -, *, /, etc.)
    /// - Unary operations (-, !)
    /// - Function calls
    /// - Object/List operations
    fn evaluate_expression(&mut self, expr_node: &Expr) -> Result<Value, Error> {
        match &expr_node.kind {
            // Handle literal values
            ExprKind::Literal(value) => Ok(value.clone()),

            // Handle grouped expressions (parentheses)
            ExprKind::Grouping(expr) => self.evaluate_expression(expr),

            // Handle unary operations (-, !)
            ExprKind::Unary(operator_token, right_expr) => {
                let right_val = self.evaluate_expression(right_expr)?;
                let op_loc = SourceLocation::new(operator_token.line, operator_token.column, operator_token.lexeme.len());

                match operator_token.token_type {
                    TokenType::Minus => match right_val {
                        Value::Int(num) => Ok(Value::Int(-num)),
                        Value::Float(num) => Ok(Value::Float(-num)),
                        _ => Err(Error::type_error(
                            ErrorCode::T0002,
                            format!("Unary '-' operator can only be applied to numbers, found {}.", right_val.get_type()),
                            Some(op_loc),
                        )),
                    },
                    TokenType::Bang => {
                        if let Value::Bool(boolean) = right_val {
                            Ok(Value::Bool(!boolean))
                        } else {
                            Err(Error::type_error(
                                ErrorCode::T0002,
                                format!("Unary '!' operator can only be applied to booleans, found {}.", right_val.get_type()),
                                Some(op_loc),
                            ))
                        }
                    }
                    _ => Err(Error::syntax(
                        ErrorCode::P0000,
                        format!("Invalid unary operator '{}'.", operator_token.lexeme),
                        Some(op_loc),
                    )),
                }
            },

            // Handle variable references
            ExprKind::Variable(name_token) => {
                if let Some(var_data) = self.environment.get(&name_token.lexeme) {
                    Ok(var_data.value.clone())
                } else {
                    Err(Error::runtime(
                        ErrorCode::R0001,
                        format!("Undefined variable '{}'.", name_token.lexeme),
                        Some(SourceLocation::new(name_token.line, name_token.column, name_token.lexeme.len())),
                    ))
                }
            },

            // Handle binary operations (+, -, *, /, etc.)
            ExprKind::Binary(left_expr, operator_token, right_expr) => {
                let left_val = self.evaluate_expression(left_expr)?;
                let right_val = self.evaluate_expression(right_expr)?;
                let op_loc = SourceLocation::new(operator_token.line, operator_token.column, operator_token.lexeme.len());

                match operator_token.token_type {
                    TokenType::Plus => self.add_values(&left_val, &right_val, op_loc),
                    TokenType::Minus => self.subtract_values(&left_val, &right_val, op_loc),
                    TokenType::Star => self.multiply_values(&left_val, &right_val, op_loc),
                    TokenType::Slash => self.divide_values(&left_val, &right_val, op_loc),
                    TokenType::EqualEqual => Ok(Value::Bool(left_val == right_val)),
                    TokenType::BangEqual => Ok(Value::Bool(left_val != right_val)),
                    TokenType::Greater => self.compare_values(&left_val, &right_val, |a, b| a > b, op_loc),
                    TokenType::GreaterEqual => self.compare_values(&left_val, &right_val, |a, b| a >= b, op_loc),
                    TokenType::Less => self.compare_values(&left_val, &right_val, |a, b| a < b, op_loc),
                    TokenType::LessEqual => self.compare_values(&left_val, &right_val, |a, b| a <= b, op_loc),
                    TokenType::Modulo => self.modulo_values(&left_val, &right_val, op_loc),
                    TokenType::StarStar => self.power_values(&left_val, &right_val, op_loc),
                    _ => Err(Error::syntax(
                        ErrorCode::P0000,
                        format!("Invalid binary operator '{}'.", operator_token.lexeme),
                        Some(op_loc),
                    )),
                }
            },

            // Handle logical operations (&&, ||)
            ExprKind::Logical(left_expr, operator_token, right_expr) => {
                let left_val = self.evaluate_expression(left_expr)?;
                let op_loc = SourceLocation::new(operator_token.line, operator_token.column, operator_token.lexeme.len());

                match operator_token.token_type {
                    TokenType::AmpersandAmpersand => {
                        if let Value::Bool(false) = left_val {
                            Ok(Value::Bool(false))
                        } else {
                            self.evaluate_expression(right_expr)
                        }
                    },
                    TokenType::PipePipe => {
                        if let Value::Bool(true) = left_val {
                            Ok(Value::Bool(true))
                        } else {
                            self.evaluate_expression(right_expr)
                        }
                    },
                    _ => Err(Error::syntax(
                        ErrorCode::P0000,
                        format!("Invalid logical operator '{}'.", operator_token.lexeme),
                        Some(op_loc),
                    )),
                }
            },

            // Handle function calls
            ExprKind::Call { name_token, arguments } => {
                let mut arg_values = Vec::with_capacity(arguments.len());
                for arg in arguments {
                    arg_values.push(self.evaluate_expression(arg)?);
                }
                self.call_function(&name_token.lexeme, arg_values, &name_token)
            },

            // Handle assignments
            ExprKind::Assign { name_token, value } => {
                let new_value = self.evaluate_expression(value)?;
                let loc = SourceLocation::new(name_token.line, name_token.column, name_token.lexeme.len());
                
                // Get variable information for validation
                if let Some(var) = self.environment.get(&name_token.lexeme) {
                    // Check if variable is mutable
                    if !var.is_mutable {
                        return Err(Error::runtime(
                            ErrorCode::R0002,
                            format!("Cannot assign to immutable variable '{}'.", name_token.lexeme),
                            Some(loc.clone()),
                        ));
                    }
                    
                    // Check if types are compatible
                    let var_type = var.var_type.clone();
                    self.check_type_compatibility(&var_type, &new_value, Some(loc.clone()))?;
                    
                    // Perform the assignment (retrieve mutable reference now)
                    if let Some(var_data) = self.environment.get_mut(&name_token.lexeme) {
                        var_data.value = new_value.clone();
                        Ok(new_value)
                    } else {
                        // This should not happen since we already checked existence
                        Err(Error::runtime(
                            ErrorCode::R0001,
                            format!("Undefined variable '{}'.", name_token.lexeme),
                            Some(loc),
                        ))
                    }
                } else {
                    Err(Error::runtime(
                        ErrorCode::R0001,
                        format!("Undefined variable '{}'.", name_token.lexeme),
                        Some(loc),
                    ))
                }
            },

            // Handle increment/decrement
            ExprKind::Increment(name_token) | ExprKind::Decrement(name_token) => {
                if let Some(var_data) = self.environment.get_mut(&name_token.lexeme) {
                    if !var_data.is_mutable {
                        return Err(Error::runtime(
                            ErrorCode::R0002,
                            format!("Cannot modify immutable variable '{}'.", name_token.lexeme),
                            Some(SourceLocation::new(name_token.line, name_token.column, name_token.lexeme.len())),
                        ));
                    }
                    match &var_data.value {
                        Value::Int(n) => {
                            let new_value = match expr_node.kind {
                                ExprKind::Increment(_) => Value::Int(n + 1),
                                ExprKind::Decrement(_) => Value::Int(n - 1),
                                _ => unreachable!(),
                            };
                            var_data.value = new_value.clone();
                            Ok(new_value)
                        },
                        _ => Err(Error::type_error(
                            ErrorCode::T0002,
                            format!("Cannot increment/decrement non-integer value of type {}.", var_data.value.get_type()),
                            Some(SourceLocation::new(name_token.line, name_token.column, name_token.lexeme.len())),
                        )),
                    }
                } else {
                    Err(Error::runtime(
                        ErrorCode::R0001,
                        format!("Undefined variable '{}'.", name_token.lexeme),
                        Some(SourceLocation::new(name_token.line, name_token.column, name_token.lexeme.len())),
                    ))
                }
            },

            // Handle list literals
            ExprKind::ListLiteral(elements) => {
                let mut list_elements = Vec::with_capacity(elements.len());
                
                // Infer list element type from first non-null element
                let mut inferred_type = Type::Unknown;
                
                for element in elements {
                    let value = self.evaluate_expression(element)?;
                    
                    // Try to infer element type from first non-null value
                    if inferred_type == Type::Unknown && value != Value::Null {
                        inferred_type = value.get_type();
                    }
                    
                    list_elements.push(value);
                }
                
                Ok(Value::List(ListRef::from_vec(list_elements, inferred_type)))
            },

            // Handle method calls
            ExprKind::MethodCall { object, method_name_token, arguments } => {
                let object_value = self.evaluate_expression(object)?;
                let mut arg_values = Vec::with_capacity(arguments.len());
                for arg in arguments {
                    arg_values.push(self.evaluate_expression(arg)?);
                }
                self.call_method(&object_value, &method_name_token, arg_values)
            },

            // Handle object literals
            ExprKind::ObjectLiteral { properties } => {
                let obj = ObjectRef::new();
                let mut obj_properties = obj.properties.borrow_mut();
                for (key_token, value_expr) in properties {
                    let value = self.evaluate_expression(value_expr)?;
                    obj_properties.insert(key_token.lexeme.clone(), value);
                }
                drop(obj_properties); // Release the borrow before returning
                Ok(Value::Object(obj))
            },

            // Handle property access
            ExprKind::Get { object, name } => {
                let object_value = self.evaluate_expression(object)?;
                self.get_property(&object_value, &name)
            },

            // Handle property assignment
            ExprKind::Set { object, name, value } => {
                let object_value = self.evaluate_expression(object)?;
                let value = self.evaluate_expression(value)?;
                self.set_property(&object_value, &name, value)
            },

            // ... rest of expression handling ...
        }
    }

    // Helper methods for binary operations
    fn add_values(&mut self, left: &Value, right: &Value, loc: SourceLocation) -> Result<Value, Error> {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a + b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 + b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a + *b as f64)),
            (Value::String(a), Value::String(b)) => Ok(Value::String(a.clone() + b)),
            _ => Err(Error::type_error(
                ErrorCode::T0002,
                format!("Cannot add values of types {} and {}.", left.get_type(), right.get_type()),
                Some(loc),
            )),
        }
    }

    fn subtract_values(&mut self, left: &Value, right: &Value, loc: SourceLocation) -> Result<Value, Error> {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a - b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a - b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 - b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a - *b as f64)),
            _ => Err(Error::type_error(
                ErrorCode::T0002,
                format!("Cannot subtract values of types {} and {}.", left.get_type(), right.get_type()),
                Some(loc),
            )),
        }
    }

    fn multiply_values(&mut self, left: &Value, right: &Value, loc: SourceLocation) -> Result<Value, Error> {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a * b)),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Float(a * b)),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Float(*a as f64 * b)),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Float(a * *b as f64)),
            _ => Err(Error::type_error(
                ErrorCode::T0002,
                format!("Cannot multiply values of types {} and {}.", left.get_type(), right.get_type()),
                Some(loc),
            )),
        }
    }

    fn divide_values(&mut self, left: &Value, right: &Value, loc: SourceLocation) -> Result<Value, Error> {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    Err(Error::runtime(
                        ErrorCode::R0003,
                        "Division by zero.".to_string(),
                        Some(loc),
                    ))
                } else {
                    Ok(Value::Int(a / b))
                }
            },
            (Value::Float(a), Value::Float(b)) => {
                if *b == 0.0 {
                    Err(Error::runtime(
                        ErrorCode::R0003,
                        "Division by zero.".to_string(),
                        Some(loc),
                    ))
                } else {
                    Ok(Value::Float(a / b))
                }
            },
            (Value::Int(a), Value::Float(b)) => {
                if *b == 0.0 {
                    Err(Error::runtime(
                        ErrorCode::R0003,
                        "Division by zero.".to_string(),
                        Some(loc),
                    ))
                } else {
                    Ok(Value::Float(*a as f64 / b))
                }
            },
            (Value::Float(a), Value::Int(b)) => {
                if *b == 0 {
                    Err(Error::runtime(
                        ErrorCode::R0003,
                        "Division by zero.".to_string(),
                        Some(loc),
                    ))
                } else {
                    Ok(Value::Float(a / *b as f64))
                }
            },
            _ => Err(Error::type_error(
                ErrorCode::T0002,
                format!("Cannot divide values of types {} and {}.", left.get_type(), right.get_type()),
                Some(loc),
            )),
        }
    }

    fn modulo_values(&mut self, left: &Value, right: &Value, loc: SourceLocation) -> Result<Value, Error> {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => {
                if *b == 0 {
                    Err(Error::runtime(
                        ErrorCode::R0003,
                        "Modulo by zero.".to_string(),
                        Some(loc),
                    ))
                } else {
                    Ok(Value::Int(a % b))
                }
            },
            _ => Err(Error::type_error(
                ErrorCode::T0002,
                format!("Cannot perform modulo on values of types {} and {}.", left.get_type(), right.get_type()),
                Some(loc),
            )),
        }
    }

    fn power_values(&mut self, base: &Value, exponent: &Value, loc: SourceLocation) -> Result<Value, Error> {
        match (base, exponent) {
            (Value::Int(b), Value::Int(e)) => {
                if *e < 0 {
                    // Negative exponent, result is float
                    Ok(Value::Float((*b as f64).powi(*e as i32)))
                } else {
                    // Positive integer exponent
                    Ok(Value::Int(b.pow(*e as u32)))
                }
            }
            (Value::Float(b), Value::Float(e)) => Ok(Value::Float(b.powf(*e))),
            (Value::Int(b), Value::Float(e)) => Ok(Value::Float((*b as f64).powf(*e))),
            (Value::Float(b), Value::Int(e)) => Ok(Value::Float(b.powi(*e as i32))),
            _ => Err(Error::type_error(
                ErrorCode::T0002,
                format!("Cannot perform exponentiation on values of types {} and {}.", base.get_type(), exponent.get_type()),
                Some(loc),
            )),
        }
    }

    fn compare_values<F>(&mut self, left: &Value, right: &Value, compare_fn: F, loc: SourceLocation) -> Result<Value, Error>
    where
        F: FnOnce(f64, f64) -> bool,
    {
        match (left, right) {
            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(compare_fn(*a as f64, *b as f64))),
            (Value::Float(a), Value::Float(b)) => Ok(Value::Bool(compare_fn(*a, *b))),
            (Value::Int(a), Value::Float(b)) => Ok(Value::Bool(compare_fn(*a as f64, *b))),
            (Value::Float(a), Value::Int(b)) => Ok(Value::Bool(compare_fn(*a, *b as f64))),
            _ => Err(Error::type_error(
                ErrorCode::T0002,
                format!("Cannot compare values of types {} and {}.", left.get_type(), right.get_type()),
                Some(loc),
            )),
        }
    }

    // Helper methods for function and method calls
    fn call_function(&mut self, name: &str, args: Vec<Value>, call_token: &Token) -> Result<Value, Error> {
        // First resolve the name in case it's a qualified standard library function
        let resolved_name = self.resolve_std_function(name);

        // Check for built-in functions first
        if resolved_name == "inputln" {
            if !args.is_empty() {
                return Err(Error::runtime(
                    ErrorCode::R0005,
                    format!("Function 'inputln' expects 0 arguments, got {}.", args.len()),
                    Some(SourceLocation::new(call_token.line, call_token.column, 0)),
                ));
            }
            io::stdout().flush().map_err(|e| Error::io(
                ErrorCode::I0002,
                format!("Failed to flush stdout: {}", e),
                Some(SourceLocation::new(call_token.line, call_token.column, 0))
            ))?;

            let mut input = String::new();
            io::stdin().read_line(&mut input).map_err(|e| Error::io(
                ErrorCode::I0001,
                format!("Failed to read line: {}", e),
                Some(SourceLocation::new(call_token.line, call_token.column, 0))
            ))?;
            Ok(Value::String(input.trim_end_matches(|c| c == '\r' || c == '\n').to_string()))
        } else if resolved_name == "toFloat" {
            if args.len() != 1 {
                return Err(Error::runtime(
                    ErrorCode::R0005,
                    format!("Function 'toFloat' expects 1 argument, got {}.", args.len()),
                    Some(SourceLocation::new(call_token.line, call_token.column, 0)),
                ));
            }
            let arg_val = &args[0];
            match arg_val {
                Value::Float(f) => Ok(Value::Float(*f)), // Already a float
                Value::Int(i) => Ok(Value::Float(*i as f64)), // Convert Int to Float (e.g., 10 becomes 10.0)
                Value::String(s) => {
                    match s.parse::<f64>() {
                        Ok(f_val) => Ok(Value::Float(f_val)),
                        Err(_) => Err(Error::type_error(
                            ErrorCode::T0004,
                            format!("Cannot convert string \"{}\" to Float.", s),
                            Some(SourceLocation::new(call_token.line, call_token.column, 0)),
                        )),
                    }
                },
                Value::Bool(b) => Ok(Value::Float(if *b { 1.0 } else { 0.0 })),
                _ => Err(Error::type_error(
                    ErrorCode::T0002,
                    format!("Cannot convert value of type {} to Float.", arg_val.get_type()),
                    Some(SourceLocation::new(call_token.line, call_token.column, 0)),
                )),
            }
        } else if resolved_name == "toInt" {
            if args.len() != 1 {
                return Err(Error::runtime(
                    ErrorCode::R0005,
                    format!("Function 'toInt' expects 1 argument, got {}.", args.len()),
                    Some(SourceLocation::new(call_token.line, call_token.column, 0)),
                ));
            }
            let arg_val = &args[0];
            match arg_val {
                Value::Int(i) => Ok(Value::Int(*i)), // Already an int
                Value::Float(f) => Ok(Value::Int(*f as i64)), // Truncate float to int
                Value::String(s) => {
                    if let Ok(i) = s.parse::<i64>() {
                        return Ok(Value::Int(i));
                    }
                    if let Ok(f) = s.parse::<f64>() {
                        return Ok(Value::Int(f as i64));
                    }
                    Err(Error::type_error(
                        ErrorCode::T0004,
                        format!("Cannot convert string \"{}\" to Int.", s),
                        Some(SourceLocation::new(call_token.line, call_token.column, 0)),
                    ))
                }
                Value::Bool(b) => Ok(Value::Int(if *b { 1 } else { 0 })),
                _ => Err(Error::type_error(
                    ErrorCode::T0002,
                    format!("Cannot convert value of type {} to Int.", arg_val.get_type()),
                    Some(SourceLocation::new(call_token.line, call_token.column, 0)),
                )),
            }
        } else if resolved_name == "toString" {
            if args.len() != 1 {
                return Err(Error::runtime(
                    ErrorCode::R0005,
                    format!("Function 'toString' expects 1 argument, got {}.", args.len()),
                    Some(SourceLocation::new(call_token.line, call_token.column, 0)),
                ));
            }
            let arg_val = &args[0];
            match arg_val {
                Value::String(s) => Ok(Value::String(s.clone())),
                Value::Int(i) => Ok(Value::String(i.to_string())),
                Value::Float(f) => Ok(Value::String(f.to_string())), // Standard f64 to string conversion
                Value::Bool(b) => Ok(Value::String(b.to_string())), // "true" or "false"
                // Handle other types as needed, e.g., if you have Value::Nil:
                // Value::Nil => Ok(Value::String("nil".to_string())),
                _ => {
                    // Fallback: uses the Display trait of Value.
                    // Ensure your Value enum has a Display implementation.
                    Ok(Value::String(format!("{}", arg_val)))
                    // Alternatively, if specific types should not be convertible:
                    // Err(Error::type_error(
                    //     ErrorCode::T000X, // Some "cannot convert to string" error
                    //     format!("Cannot convert value of type {} to String.", arg_val.get_type()),
                    //     Some(SourceLocation::new(call_token.line, call_token.column, 0)),
                    // ))
                }
            }
        } else if resolved_name == "toBool" {
            if args.len() != 1 {
                return Err(Error::runtime(
                    ErrorCode::R0005,
                    format!("Function 'toBool' expects 1 argument, got {}.", args.len()),
                    Some(SourceLocation::new(call_token.line, call_token.column, 0)),
                ));
            }
            let arg_val = &args[0];
            match arg_val {
                Value::Bool(b) => Ok(Value::Bool(*b)), // Already a bool
                Value::Int(i) => Ok(Value::Bool(*i != 0)), // 0 is false, others true
                Value::Float(f) => Ok(Value::Bool(*f != 0.0)), // 0.0 is false, others true
                Value::String(s) => Ok(Value::Bool(!s.is_empty())), // Empty string is false, others true
                Value::Null => Ok(Value::Bool(false)),
                // Handle other types as needed, e.g., if you have Value::Nil:
                // Value::Nil => Ok(Value::Bool(false)),
                _ => Err(Error::type_error(
                    ErrorCode::T0002, // Or a more specific "cannot convert to Bool" error code
                    format!("Cannot convert value of type {} to Bool.", arg_val.get_type()),
                    Some(SourceLocation::new(call_token.line, call_token.column, 0)),
                )),
            }
        } else if let Some(func) = self.functions.get(&resolved_name).cloned() {
            if args.len() != func.parameters.len() {
                return Err(Error::runtime(
                    ErrorCode::R0005,
                    format!("Function '{}' expects {} arguments, got {}.", resolved_name, func.parameters.len(), args.len()),
                    Some(SourceLocation::new(call_token.line, call_token.column, 0)),
                ));
            }

            let old_env = self.environment.clone();
            self.environment.clear(); // Or use a new, nested environment approach

            for (param, arg) in func.parameters.iter().zip(args) {
                self.environment.insert(
                    param.name_token.lexeme.clone(),
                    Variable {
                        var_type: param.param_type.clone(),
                        value: arg,
                        is_mutable: false,
                    },
                );
            }

            let body = func.body.clone();
            let execution_result = self.execute_statement(&body);
            self.environment = old_env;

            match execution_result {
                Ok(value_if_no_explicit_return) => {
                    self.check_type_compatibility(
                        &func.return_type,
                        &value_if_no_explicit_return,
                        Some(SourceLocation::new(call_token.line, call_token.column, call_token.lexeme.len()))
                    )?;
                    Ok(value_if_no_explicit_return)
                }
                Err(Error::ReturnControlFlow(boxed_value)) => {
                    self.check_type_compatibility(
                        &func.return_type,
                        &boxed_value,
                        Some(SourceLocation::new(call_token.line, call_token.column, call_token.lexeme.len()))
                    )?;
                    Ok(*boxed_value)
                }
                Err(other_error) => {
                    Err(other_error)
                }
            }
        } else {
            Err(Error::runtime(
                ErrorCode::R0004,
                format!("Undefined function '{}'.", resolved_name),
                Some(SourceLocation::new(call_token.line, call_token.column, 0)),
            ))
        }
    }


    /// Helper method to handle list method calls
    fn handle_list_method(&mut self, list_ref: ListRef, method_name: &str, args: Vec<Value>, method_token: &Token) -> Result<Value, Error> {
        let location = Self::token_to_location(method_token);
        match method_name {
            "push" => self.list_push(list_ref.clone(), args, method_token, &location),
            "len" => self.list_len(list_ref.clone(), args, method_token),
            "pop" => self.list_pop(list_ref.clone(), args, method_token),
            "get" => self.list_get(list_ref.clone(), args, method_token, &location),
            "set" => self.list_set(list_ref.clone(), args, method_token, &location, None),
            _ => Err(self.create_error(
                ErrorCode::R0004,
                format!("List has no method '{}'.", method_name),
                Some(location),
            )),
        }
    }

    /// Helper method to handle object method calls
    fn handle_object_method(&mut self, _obj_ref: ObjectRef, _method_name: &str, _args: Vec<Value>, method_token: &Token) -> Result<Value, Error> {
        let location = Self::token_to_location(method_token);
        // TODO: Implement object methods
        Err(self.create_error(
            ErrorCode::R0004,
            "Object methods not implemented yet.".to_string(),
            Some(location),
        ))
    }

    /// Call a method on a value
    fn call_method(&mut self, object: &Value, method_token: &Token, args: Vec<Value>) -> Result<Value, Error> {
        match object {
            Value::List(list_ref) => self.handle_list_method(list_ref.clone(), &method_token.lexeme, args, method_token),
            Value::Object(obj_ref) => self.handle_object_method(obj_ref.clone(), &method_token.lexeme, args, method_token),
            _ => Err(self.create_type_error(
                ErrorCode::T0003,
                format!("Cannot call methods on value of type {}.", object.get_type()),
                Some(Self::token_to_location(method_token)),
            )),
        }
    }

    /// Get a property from an object
    fn get_property(&mut self, object: &Value, name: &Token) -> Result<Value, Error> {
        match object {
            Value::Object(obj_ref) => {
                let properties = obj_ref.properties.borrow();
                properties.get(&name.lexeme)
                    .cloned()
                    .ok_or_else(|| self.create_error(
                        ErrorCode::R0006,
                        format!("Object has no property '{}'.", name.lexeme),
                        Some(Self::token_to_location(name)),
                    ))
            },
            _ => Err(self.create_type_error(
                ErrorCode::T0003,
                format!("Cannot access properties on value of type {}.", object.get_type()),
                Some(Self::token_to_location(name)),
            )),
        }
    }

    /// Set a property on an object
    fn set_property(&mut self, object: &Value, name: &Token, value: Value) -> Result<Value, Error> {
        match object {
            Value::Object(obj_ref) => {
                let mut properties = obj_ref.properties.borrow_mut();
                properties.insert(name.lexeme.clone(), value.clone());
                Ok(value)
            },
            _ => Err(self.create_type_error(
                ErrorCode::T0003,
                format!("Cannot set properties on value of type {}.", object.get_type()),
                Some(Self::token_to_location(name)),
            )),
        }
    }

    // List and Object Helper Methods

    /// Push a value onto a list
    fn list_push(&mut self, list_ref: ListRef, arg_values: Vec<Value>, method_token: &Token, value_loc: &SourceLocation) -> Result<Value, Error> {
        // Check argument count
        if arg_values.len() != 1 {
            return Err(self.create_error(
                ErrorCode::R0005, 
                format!("List method '{}' expects 1 argument (value to push), got {}.", method_token.lexeme, arg_values.len()),
                Some(Self::token_to_location(method_token)),
            ));
        }

        let value_to_push = &arg_values[0];
        let value_type = value_to_push.get_type();
        let list_element_type_rc = list_ref.element_type.clone();

        // Handle type compatibility and inference
        let type_compatible = {
            let current_list_type_view = list_element_type_rc.borrow();
            if *current_list_type_view == Type::Unknown && value_to_push != &Value::Null {
                // If list type is unknown and value is not null, infer type from value
                drop(current_list_type_view);
                let mut list_type_mut = list_element_type_rc.borrow_mut();
                *list_type_mut = value_type.clone();
                true
            } else {
                // Check if value is compatible with existing list type
                current_list_type_view.is_compatible_with(&value_type) || value_to_push == &Value::Null
            }
        };

        // Return error if type is incompatible
        if !type_compatible {
            let final_list_type = list_element_type_rc.borrow();
            return Err(self.create_type_error(
                ErrorCode::T0001, 
                format!(
                    "Cannot push value of type {} to list of type {}.",
                    value_type, *final_list_type
                ),
                Some(value_loc.clone()),
            ));
        }

        // Add value to list
        let mut elements = list_ref.elements.borrow_mut();
        elements.push(value_to_push.clone());
        Ok(Value::Null) 
    }

    /// Get the length of a list
    fn list_len(&mut self, list_ref: ListRef, arg_values: Vec<Value>, method_token: &Token) -> Result<Value, Error> {
        // Check argument count
        if !arg_values.is_empty() {
            return Err(self.create_error(
                ErrorCode::R0005, 
                format!("List method '{}' expects 0 arguments, got {}.", method_token.lexeme, arg_values.len()),
                Some(Self::token_to_location(method_token)),
            ));
        }
        let elements = list_ref.elements.borrow();
        Ok(Value::Int(elements.len() as i64))
    }

    /// Remove and return the last element from a list
    fn list_pop(&mut self, list_ref: ListRef, arg_values: Vec<Value>, method_token: &Token) -> Result<Value, Error> {
        // Check argument count
        if !arg_values.is_empty() {
            return Err(self.create_error(
                ErrorCode::R0005,
                format!("List method '{}' expects 0 arguments, got {}.", method_token.lexeme, arg_values.len()),
                Some(Self::token_to_location(method_token)),
            ));
        }

        // Check if list is empty
        let mut elements = list_ref.elements.borrow_mut();
        if elements.is_empty() {
            return Err(self.create_error(
                ErrorCode::R0000,
                "Cannot pop from an empty list.".to_string(),
                Some(Self::token_to_location(method_token)),
            ));
        }

        Ok(elements.pop().unwrap_or(Value::Null))
    }

    /// Get an element from a list by index
    fn list_get(&mut self, list_ref: ListRef, arg_values: Vec<Value>, method_token: &Token, index_expr_loc: &SourceLocation) -> Result<Value, Error> {
        // Check argument count
        if arg_values.len() != 1 {
            return Err(self.create_error(
                ErrorCode::R0005,
                format!("List method '{}' expects 1 argument (index), got {}.", method_token.lexeme, arg_values.len()),
                Some(Self::token_to_location(method_token)),
            ));
        }

        // Check index type
        let index_val = &arg_values[0];
        let idx = match index_val {
            Value::Int(i) => *i,
            _ => return Err(self.create_type_error(
                ErrorCode::T0007,
                format!("List index must be an integer, got {}.", index_val.get_type()),
                Some(index_expr_loc.clone()),
            )),
        };

        let elements = list_ref.elements.borrow();
        // Check bounds
        if idx < 0 || idx as usize >= elements.len() {
            return Err(self.create_error(
                ErrorCode::R0000,
                format!("List index {} out of bounds for list of length {}.", idx, elements.len()),
                Some(index_expr_loc.clone()),
            ));
        }
        Ok(elements[idx as usize].clone())
    }

    /// Set an element in a list by index
    fn list_set(&mut self, list_ref: ListRef, arg_values: Vec<Value>, method_token: &Token, index_expr_loc: &SourceLocation, value_expr_loc: Option<&SourceLocation>) -> Result<Value, Error> {
        // Check argument count
        if arg_values.len() != 2 {
            return Err(self.create_error(
                ErrorCode::R0005,
                format!("List method '{}' expects 2 arguments (index, value), got {}.", method_token.lexeme, arg_values.len()),
                Some(Self::token_to_location(method_token)),
            ));
        }

        let index_val = &arg_values[0];
        let value_to_set = &arg_values[1];
        let final_value_loc = value_expr_loc.unwrap_or(index_expr_loc);

        // Check index type
        let idx = match index_val {
            Value::Int(i) => *i,
            _ => return Err(self.create_type_error(
                ErrorCode::T0007,
                format!("List index must be an integer for set operation, got {}.", index_val.get_type()),
                Some(index_expr_loc.clone()),
            )),
        };

        // Type checking for the value
        let value_type = value_to_set.get_type();
        let list_element_type_rc = list_ref.element_type.clone();
        
        {
            let mut list_element_type_ref_mut = list_element_type_rc.borrow_mut();
            if *list_element_type_ref_mut == Type::Unknown && value_to_set != &Value::Null {
                // Infer type from first non-null value
                *list_element_type_ref_mut = value_type.clone();
            } else if !list_element_type_ref_mut.is_compatible_with(&value_type) && value_to_set != &Value::Null {
                return Err(self.create_type_error(
                    ErrorCode::T0001,
                    format!("Cannot set list element of type {} with value of type {}.", *list_element_type_ref_mut, value_type),
                    Some(final_value_loc.clone()),
                ));
            }
        }
        
        // Check bounds and set value
        let mut elements = list_ref.elements.borrow_mut();
        if idx < 0 || idx as usize >= elements.len() {
            return Err(self.create_error(
                ErrorCode::R0000,
                format!("List index {} out of bounds for assignment to list of length {}.", idx, elements.len()),
                Some(index_expr_loc.clone()),
            ));
        }
        elements[idx as usize] = value_to_set.clone();
        Ok(Value::Null)
    }

    /// Check if a value can be assigned to a variable of a specific type
    fn check_type_compatibility(&self, var_type: &Type, value: &Value, location: Option<SourceLocation>) -> Result<(), Error> {
        let value_type = value.get_type();
        
        // Null can be assigned to any type
        if *value == Value::Null {
            return Ok(());
        }
        
        // Check if the types are compatible
        if !var_type.is_compatible_with(&value_type) {
            return Err(Error::type_error(
                ErrorCode::T0001,
                format!("Cannot assign value of type {} to variable of type {}.", 
                    value_type, var_type),
                location,
            ));
        }
        
        Ok(())
    }

    /// Get the type of a variable from the environment
    #[allow(dead_code)]
    fn get_variable_type(&self, var_name: &str) -> Option<Type> {
        self.environment.get(var_name).map(|var| var.var_type.clone())
    }

    /// Initialize standard library symbols
    fn init_std_library(&mut self) {
        // Register standard math functions
        self.imported_std_symbols.insert("math::sqrt".to_string(), "sqrt".to_string());
        self.imported_std_symbols.insert("math::abs".to_string(), "abs".to_string());
        self.imported_std_symbols.insert("math::pow".to_string(), "pow".to_string());
        
        // Register standard conversion functions
        self.imported_std_symbols.insert("std::toString".to_string(), "toString".to_string());
        self.imported_std_symbols.insert("std::toInt".to_string(), "toInt".to_string());
        self.imported_std_symbols.insert("std::toFloat".to_string(), "toFloat".to_string());
        self.imported_std_symbols.insert("std::toBool".to_string(), "toBool".to_string());
    }
    
    /// Resolve a possibly qualified function name using the standard library mapping
    fn resolve_std_function(&self, name: &str) -> String {
        // Check if it's a qualified name that maps to a standard library function
        if let Some(std_func) = self.imported_std_symbols.get(name) {
            return std_func.clone();
        }
        
        // Return the name unchanged if not found in the mapping
        name.to_string()
    }

    /// Get the expected type of an expression before evaluating it
    /// This is used for type checking without executing the expression
    #[allow(dead_code)]
    fn get_expression_type(&self, expr: &Expr) -> Option<Type> {
        match &expr.kind {
            ExprKind::Literal(value) => Some(value.get_type()),
            ExprKind::Variable(name_token) => self.get_variable_type(&name_token.lexeme),
            ExprKind::Grouping(inner_expr) => self.get_expression_type(inner_expr),
            // For more complex expressions, we would need a type system
            // with type inference rules for operations
            _ => None,
        }
    }
}