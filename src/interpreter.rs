use crate::ast::{Expr, Parameter, Program, Stmt};
use crate::error::Error;
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::TokenType;
use crate::types::Type;
use crate::value::{Value, ListRef, ObjectRef};
use colored::Colorize;
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{PathBuf};
use std::rc::Rc;
use std::cell::RefCell;
use std::io;

/// Represents a variable with its type, value, and mutability
#[derive(Clone, Debug)]
struct Variable {
    var_type: Type,
    value: Value,
    is_mutable: bool,
}

/// Represents a user-defined function
#[derive(Clone, Debug)]
struct Function {
    name: String,
    parameters: Vec<Parameter>,
    return_type: Option<Type>,
    body: Box<Stmt>,
    is_exported: bool,
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
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: HashMap::new(),
            functions: HashMap::new(),
            current_file_path: None,
            module_cache: Rc::new(RefCell::new(HashMap::new())),
        }
    }

    /// Main entry point for interpreting a file.
    pub fn interpret_file(&mut self, file_path_str: &str) -> Result<Value, Error> {
        let initial_path = PathBuf::from(file_path_str);
        let canonical_path = match fs::canonicalize(&initial_path) {
            Ok(p) => p,
            Err(e) => return Err(Error::RuntimeError(format!("Error accessing file {}: {}", file_path_str, e))),
        };

        let source = match fs::read_to_string(&canonical_path) {
            Ok(s) => s,
            Err(e) => return Err(Error::RuntimeError(format!("Error reading file {}: {}", canonical_path.display(), e))),
        };
        
        // Clear included_files for a new top-level interpretation run
        self.module_cache.borrow_mut().clear();
        
        match self.interpret_source_with_path(&source, Some(canonical_path.clone())) {
            Ok((val, defined_functions)) => {
                // For the top-level file, all its defined functions are directly available.
                for func_def in defined_functions {
                    self.functions.insert(func_def.name.clone(), func_def);
                }
                Ok(val)
            }
            Err(e) => Err(e),
        }
    }

    /// Interpret a source string, typically for REPL or embedded execution.
    /// For REPL, path is None.
    pub fn interpret_repl_input(&mut self, source: &str) -> Result<Value, Error> {
        // In REPL, current_file_path should ideally be CWD or None.
        // For simplicity, let's assume None for now unless we enhance REPL to have a CWD context.
        self.interpret_source_with_path(source, self.current_file_path.clone()).map(|(val, _funcs)| val)
    }

    /// Core interpretation logic for a given source string and its optional path.
    /// Returns the last evaluated value and a list of functions defined in this source.
    fn interpret_source_with_path(&mut self, source: &str, path: Option<PathBuf>) -> Result<(Value, Vec<Function>), Error> {
        let old_path = self.current_file_path.clone();
        self.current_file_path = path;

        // 1. Tokenize
        let mut lexer = Lexer::new(source);
        let tokens = lexer.scan_tokens()?;

        // 2. Parse
        let mut parser = Parser::new(tokens);
        let program = parser.parse()?;

        // 3. Execute
        let result = self.execute_program(&program)?;

        self.current_file_path = old_path; // Restore old path
        Ok(result) // result is now (Value, Vec<Function>)
    }

    /// Execute a whole program
    /// Returns the last statement's value and a list of all functions defined.
    fn execute_program(&mut self, program: &Program) -> Result<(Value, Vec<Function>), Error> {
        let mut last_value = Value::Null;
        let mut defined_functions_in_program = Vec::new();

        // First pass: Define all functions from this program into a temporary holding
        // This is important because Stmt::Function execution in the main loop below
        // would add them to self.functions, which is not what we want for includes immediately.
        for stmt in &program.statements {
            if let Stmt::Function(name, parameters, return_type, body, is_exported) = stmt {
                let func_def = Function {
                    name: name.clone(),
                    parameters: parameters.clone(),
                    return_type: return_type.clone(),
                    body: body.clone(),
                    is_exported: *is_exported,
                };
                defined_functions_in_program.push(func_def);
            }
        }

        // Second pass: Execute all statements
        // For Stmt::Function, this will now add them to the *current* interpreter's function map.
        // This is fine for the top-level file, and for includes, these will be filtered by the caller.
        for stmt in &program.statements {
            match self.execute_statement(stmt) {
                Ok(value) => last_value = value,
                Err(err) => return Err(err),
            }
        }

        Ok((last_value, defined_functions_in_program))
    }

    /// Execute a single statement
    fn execute_statement(&mut self, stmt: &Stmt) -> Result<Value, Error> {
        match stmt {
            Stmt::Expression(expr) => self.evaluate_expression(expr),
            Stmt::Println(exprs) => {
                // Evaluate all expressions
                let mut values = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    values.push(self.evaluate_expression(expr)?);
                }
                
                // Print all values
                if values.is_empty() {
                    println!();
                } else {
                    // Format and print the values
                    let mut formatted = Vec::with_capacity(values.len());
                    for value in &values {
                        formatted.push(value.to_string_value()?);
                    }
                    let output = formatted.join(" ");
                    println!("{}", output.green());
                }
                
                Ok(Value::Null)
            },
            Stmt::Var(name, var_type, initializer, is_mutable) => {
                // Evaluate the initializer if present
                let value = match initializer {
                    Some(expr) => self.evaluate_expression(expr)?,
                    None => Value::Null, // Default value for mutable variables
                };
                
                // Check if the type matches
                let value_type = value.get_type();
                if !var_type.is_compatible_with(&value_type) && value != Value::Null {
                    return Err(Error::TypeError(format!(
                        "Cannot assign value of type {} to variable of type {}",
                        value_type, var_type
                    )));
                }
                
                // Add the variable to the environment
                self.environment.insert(
                    name.clone(),
                    Variable {
                        var_type: var_type.clone(),
                        value,
                        is_mutable: *is_mutable,
                    },
                );
                
                Ok(Value::Null)
            },
            Stmt::Block(statements) => {
                let environment_at_block_start = self.environment.clone(); // For restoring on "true" errors
                let mut last_value = Value::Null;

                for statement in statements {
                    match self.execute_statement(statement) {
                        Ok(value) => last_value = value,
                        Err(err) => {
                            // Check the type of error
                            match &err {
                                Error::BreakError | Error::ContinueError => {
                                    // For loop control flow, do NOT restore environment here.
                                    // Propagate the error; the loop construct will handle it.
                                    return Err(err);
                                }
                                Error::RuntimeError(msg) if msg.starts_with("return:") => {
                                    // For return statements, also do NOT restore environment here.
                                    // Propagate the error; the function call handler will manage scope.
                                    return Err(err);
                                }
                                _ => {
                                    // For all other "true" errors originating within this block,
                                    // restore the environment to its state at the start of the block.
                                    self.environment = environment_at_block_start;
                                    return Err(err);
                                }
                            }
                        }
                    }
                }
                // On successful completion of the block (no errors propagated out),
                // modifications to the environment (including outer scope variables) persist.
                // The calling context (e.g., a loop or function) is responsible for its own
                // broader scope management if needed.
                Ok(last_value)
            },
            Stmt::If(condition, then_branch, elif_branches, else_branch) => {
                // Evaluate the condition
                let condition_value = self.evaluate_expression(condition)?;
                
                // Check if the condition is a boolean
                if let Value::Bool(is_true) = condition_value {
                    if is_true {
                        // Execute the then branch
                        return self.execute_statement(then_branch);
                    } else {
                        // Try the elif branches
                        for (elif_condition, elif_branch) in elif_branches {
                            let elif_value = self.evaluate_expression(elif_condition)?;
                            
                            if let Value::Bool(elif_is_true) = elif_value {
                                if elif_is_true {
                                    // Execute this elif branch
                                    return self.execute_statement(elif_branch);
                                }
                            } else {
                                return Err(Error::TypeError(
                                    "Elif condition must be a boolean".to_string()
                                ));
                            }
                        }
                        
                        // If no elif branches matched, try the else branch
                        if let Some(else_branch) = else_branch {
                            return self.execute_statement(else_branch);
                        }
                    }
                } else {
                    return Err(Error::TypeError(
                        "If condition must be a boolean".to_string()
                    ));
                }
                
                Ok(Value::Null)
            },
            Stmt::Function(_name, _parameters, _return_type, _body, _is_exported) => {
                // When a Stmt::Function is executed, it doesn't immediately add to self.functions here.
                // The execute_program collects all function definitions, and the caller 
                // (interpret_file for top-level, or Stmt::Including for includes) decides how to use them.
                Ok(Value::Null)
            },
            Stmt::Return(expr) => {
                // Evaluate the return value if present
                let value = match expr {
                    Some(expr) => self.evaluate_expression(expr)?,
                    None => Value::Null,
                };
                
                // Create a special error to unwind the call stack
                Err(Error::RuntimeError(format!("return: {}", value)))
            },
            Stmt::While(condition, body) => {
                // Execute the loop as long as the condition is true
                'outer: loop {
                    // Evaluate the condition
                    let condition_value = self.evaluate_expression(condition)?;
                    
                    // Check if the condition is a boolean
                    if let Value::Bool(is_true) = condition_value {
                        if !is_true {
                            // Exit the loop if the condition is false
                            break 'outer;
                        }
                        
                        // Execute the body
                        match self.execute_statement(body) {
                            Ok(_) => {},
                            // Special case for return statements
                            Err(ref err @ Error::RuntimeError(ref msg)) if msg.starts_with("return: ") => {
                                return Err(err.clone());
                            },
                            // Handle break - this should exit the outer loop
                            Err(Error::BreakError) => {
                                break 'outer;
                            },
                            // Handle continue
                            Err(Error::ContinueError) => continue 'outer,
                            // Other errors
                            Err(err) => return Err(err),
                        }
                    } else {
                        return Err(Error::TypeError(
                            "While condition must be a boolean".to_string()
                        ));
                    }
                }
                
                Ok(Value::Null)
            },
            Stmt::For(initializer, condition, increment, body) => {
                // Create a new scope for the loop
                let old_environment = self.environment.clone();
                
                // Execute the initializer
                match self.execute_statement(initializer) {
                    Ok(_) => {},
                    Err(err) => {
                        // Restore the environment and propagate the error
                        self.environment = old_environment;
                        return Err(err);
                    }
                }
                
                // Execute the loop
                let result = 'outer: loop {
                    // Evaluate the condition
                    let condition_value = self.evaluate_expression(condition)?;
                    
                    // Check if the condition is a boolean
                    if let Value::Bool(is_true) = condition_value {
                        if !is_true {
                            // Exit the loop if the condition is false
                            break 'outer Ok(Value::Null);
                        }
                        
                        // Execute the body
                        let mut continued = false;
                        match self.execute_statement(body) {
                            Ok(_) => {},
                            // Special case for return statements
                            Err(ref err @ Error::RuntimeError(ref msg)) if msg.starts_with("return: ") => {
                                // Restore the environment and propagate the error
                                self.environment = old_environment;
                                return Err(err.clone());
                            },
                            // Handle break
                            Err(Error::BreakError) => {
                                break 'outer Ok(Value::Null);
                            },
                            // Handle continue
                            Err(Error::ContinueError) => {
                                continued = true;
                                // The increment will be handled below, then we will continue.
                            },
                            // Other errors
                            Err(err) => {
                                // Restore the environment and propagate the error
                                self.environment = old_environment;
                                return Err(err);
                            }
                        }
                        
                        // Always execute the increment part of the for loop here
                        if let Err(err) = self.evaluate_expression(increment) {
                            self.environment = old_environment;
                            return Err(err);
                        }

                        // If continue was signaled, jump to the next iteration now
                        if continued {
                            continue 'outer;
                        }

                    } else {
                        // Restore the environment and propagate the error
                        self.environment = old_environment;
                        return Err(Error::TypeError(
                            "For condition must be a boolean".to_string()
                        ));
                    }
                };
                
                // Restore the environment
                self.environment = old_environment;
                
                result
            },
            Stmt::Break => Err(Error::BreakError),
            Stmt::Continue => Err(Error::ContinueError),
            Stmt::Including { path: path_str, imports } => {
                let current_dir = match &self.current_file_path {
                    Some(p) => p.parent().ok_or_else(|| Error::RuntimeError(
                        format!("Could not determine directory of current file: {}", p.display())
                    ))?,
                    None => return Err(Error::RuntimeError(
                        "'including' statement can only be used from within a file, not in REPL without a loaded file context.".to_string()
                    )),
                };

                let included_file_initial_path = current_dir.join(path_str);
                let included_file_abs_path = match fs::canonicalize(&included_file_initial_path) {
                    Ok(p) => p,
                    Err(e) => return Err(Error::RuntimeError(format!(
                        "Error accessing included file '{}' (resolved to '{}'): {}", 
                        path_str, included_file_initial_path.display(), e
                    ))),
                };

                let functions_to_process_for_import: Vec<Function>;
                let mut all_functions_from_module_if_cache_miss: Option<Vec<Function>> = None;

                // Explicitly scope the cache read
                let opt_cached_functions = {
                    let cache_reader = self.module_cache.borrow();
                    cache_reader.get(&included_file_abs_path).cloned() // .cloned() is important if Vec<Function> is to be owned
                }; // cache_reader (Ref) is dropped here

                if let Some(cached_exported_functions) = opt_cached_functions {
                    functions_to_process_for_import = cached_exported_functions;
                } else {
                    // Cache miss: process the file
                    let included_source = match fs::read_to_string(&included_file_abs_path) {
                        Ok(s) => s,
                        Err(e) => return Err(Error::RuntimeError(format!(
                            "Error reading included file '{}': {}", included_file_abs_path.display(), e
                        ))),
                    };
                    
                    // Create a temporary, clean interpreter for the module.
                    // It shares the module_cache so nested includes also populate the main cache.
                    let mut module_interpreter = Interpreter {
                        environment: HashMap::new(), // Fresh environment
                        functions: HashMap::new(),   // Fresh function map
                        current_file_path: None,     // Will be set by interpret_source_with_path
                        module_cache: Rc::clone(&self.module_cache), // Share the cache
                    };

                    match module_interpreter.interpret_source_with_path(&included_source, Some(included_file_abs_path.clone())) {
                        Ok((_value, all_defined_functions_in_module)) => {
                            all_functions_from_module_if_cache_miss = Some(all_defined_functions_in_module.clone());

                            let exported_functions_vec = all_defined_functions_in_module.into_iter()
                                .filter(|f| f.is_exported)
                                .collect::<Vec<Function>>();
                            
                            // This is the critical point for the borrow_mut() call.
                            // The immutable borrow from the initial cache check by 'self' is now definitely gone.
                            self.module_cache.borrow_mut().insert(included_file_abs_path.clone(), exported_functions_vec.clone());
                            functions_to_process_for_import = exported_functions_vec;
                        }
                        Err(e) => {
                            return Err(Error::RuntimeError(format!(
                                "Error in included file '{}':
{}", 
                                included_file_abs_path.display(), e
                            )));
                        }
                    }
                }
                
                // Perform selective import error checking.
                if let Some(ref import_tokens) = imports {
                    for token in import_tokens {
                        let requested_name = &token.lexeme;
                        let found_in_exported = functions_to_process_for_import.iter().any(|f| f.name == *requested_name);

                        if !found_in_exported {
                            let err_msg = if let Some(ref all_funcs) = all_functions_from_module_if_cache_miss {
                                // This was a cache miss path, provide detailed error.
                                let was_defined_not_exported = all_funcs.iter().any(|f| f.name == *requested_name && !f.is_exported);
                                if was_defined_not_exported {
                                    format!("Cannot import '{}' from '{}': it is defined but not exported (line {}).", requested_name, path_str, token.line)
                                } else {
                                    format!("Cannot import '{}' from '{}': it is not defined (line {}).", requested_name, path_str, token.line)
                                }
                            } else {
                                // This was a cache hit path. We only know about exported functions.
                                format!("Cannot import '{}' from '{}': it is not defined or not exported (line {}).", requested_name, path_str, token.line)
                            };
                            return Err(Error::RuntimeError(err_msg));
                        }
                    }
                }

                // Add functions to the current scope's function map.
                let mut functions_to_add_to_current_scope = Vec::new();
                if let Some(import_tokens) = imports {
                    // Selective import: including [ id1, id2 ] from "..."
                    let requested_names: HashSet<String> = import_tokens.iter().map(|t| t.lexeme.clone()).collect();
                    for func_def in functions_to_process_for_import {
                        if requested_names.contains(&func_def.name) {
                            functions_to_add_to_current_scope.push(func_def);
                        }
                    }
                } else {
                    // Import all exported: including "..."
                    functions_to_add_to_current_scope = functions_to_process_for_import;
                }

                for func_to_add in functions_to_add_to_current_scope {
                    self.functions.insert(func_to_add.name.clone(), func_to_add);
                }
                Ok(Value::Null)
            }
        }
    }

    /// Evaluate an expression to a value
    fn evaluate_expression(&mut self, expr: &Expr) -> Result<Value, Error> {
        match expr {
            Expr::Literal(value) => Ok(value.clone()),
            Expr::Grouping(expr) => self.evaluate_expression(expr),
            Expr::Unary(operator, right) => {
                let right = self.evaluate_expression(right)?;

                match operator.token_type {
                    TokenType::Minus => {
                        if let Value::Int(i) = right {
                            return Ok(Value::Int(-i));
                        }
                        return Err(Error::TypeError(
                            "Unary '-' operator can only be applied to integers".to_string(),
                        ));
                    }
                    TokenType::Bang => {
                        if let Value::Bool(b) = right {
                            return Ok(Value::Bool(!b));
                        }
                        return Err(Error::TypeError(
                            "Unary '!' operator can only be applied to booleans".to_string(),
                        ));
                    }
                    _ => Err(Error::SyntaxError(
                        "Invalid unary operator".to_string(),
                    )),
                }
            }
            Expr::Binary(left, operator, right) => {
                let left = self.evaluate_expression(left)?;
                let right = self.evaluate_expression(right)?;

                match operator.token_type {
                    // Arithmetic operators
                    TokenType::Plus => {
                        match (&left, &right) {
                            (Value::Int(a), Value::Int(b)) => Ok(Value::Int(a + b)),
                            (Value::String(a), Value::String(b)) => {
                                Ok(Value::String(format!("{}{}", a, b)))
                            }
                            _ => Err(Error::TypeError(
                                "'+' operator can only be applied to two integers or two strings"
                                    .to_string(),
                            )),
                        }
                    }
                    TokenType::Minus => {
                        if let (Value::Int(a), Value::Int(b)) = (&left, &right) {
                            return Ok(Value::Int(a - b));
                        }
                        Err(Error::TypeError(
                            "'-' operator can only be applied to two integers".to_string(),
                        ))
                    }
                    TokenType::Star => {
                        if let (Value::Int(a), Value::Int(b)) = (&left, &right) {
                            return Ok(Value::Int(a * b));
                        }
                        Err(Error::TypeError(
                            "'*' operator can only be applied to two integers".to_string(),
                        ))
                    }
                    TokenType::StarStar => {
                        if let (Value::Int(a), Value::Int(b)) = (&left, &right) {
                            // Calculate a to the power of b
                            let result = a.pow(*b as u32);
                            return Ok(Value::Int(result));
                        }
                        Err(Error::TypeError(
                            "'**' operator can only be applied to two integers".to_string(),
                        ))
                    }
                    TokenType::Slash => {
                        if let (Value::Int(a), Value::Int(b)) = (&left, &right) {
                            if *b == 0 {
                                return Err(Error::RuntimeError(
                                    "Division by zero".to_string(),
                                ));
                            }
                            return Ok(Value::Int(a / b));
                        }
                        Err(Error::TypeError(
                            "'/' operator can only be applied to two integers".to_string(),
                        ))
                    }
                    TokenType::Modulo => {
                        if let (Value::Int(a), Value::Int(b)) = (&left, &right) {
                            if *b == 0 {
                                return Err(Error::RuntimeError(
                                    "Modulo by zero".to_string(),
                                ));
                            }
                            return Ok(Value::Int(a % b));
                        }
                        Err(Error::TypeError(
                            "'%' operator can only be applied to two integers".to_string(),
                        ))
                    }

                    // Comparison operators
                    TokenType::EqualEqual => {
                        match (&left, &right) {
                            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a == b)),
                            (Value::String(a), Value::String(b)) => Ok(Value::Bool(a == b)),
                            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a == b)),
                            _ => Ok(Value::Bool(false)), // Different types are never equal
                        }
                    }
                    TokenType::BangEqual => {
                        match (&left, &right) {
                            (Value::Int(a), Value::Int(b)) => Ok(Value::Bool(a != b)),
                            (Value::String(a), Value::String(b)) => Ok(Value::Bool(a != b)),
                            (Value::Bool(a), Value::Bool(b)) => Ok(Value::Bool(a != b)),
                            _ => Ok(Value::Bool(true)), // Different types are never equal
                        }
                    }
                    TokenType::Less => {
                        if let (Value::Int(a), Value::Int(b)) = (&left, &right) {
                            return Ok(Value::Bool(a < b));
                        }
                        Err(Error::TypeError(
                            "'<' operator can only be applied to two integers".to_string(),
                        ))
                    }
                    TokenType::LessEqual => {
                        if let (Value::Int(a), Value::Int(b)) = (&left, &right) {
                            return Ok(Value::Bool(a <= b));
                        }
                        Err(Error::TypeError(
                            "'<=' operator can only be applied to two integers".to_string(),
                        ))
                    }
                    TokenType::Greater => {
                        if let (Value::Int(a), Value::Int(b)) = (&left, &right) {
                            return Ok(Value::Bool(a > b));
                        }
                        Err(Error::TypeError(
                            "'>' operator can only be applied to two integers".to_string(),
                        ))
                    }
                    TokenType::GreaterEqual => {
                        if let (Value::Int(a), Value::Int(b)) = (&left, &right) {
                            return Ok(Value::Bool(a >= b));
                        }
                        Err(Error::TypeError(
                            "'>=' operator can only be applied to two integers".to_string(),
                        ))
                    }
                    _ => Err(Error::SyntaxError(
                        "Invalid binary operator".to_string(),
                    )),
                }
            }
            Expr::Logical(left, operator, right) => {
                let left_val = self.evaluate_expression(left)?;

                // Ensure left operand is a boolean for logical operators
                if let Value::Bool(left_bool) = left_val {
                    match operator.token_type {
                        TokenType::PipePipe => {
                            // Short-circuit OR: if left is true, result is true
                            if left_bool {
                                return Ok(Value::Bool(true));
                            }
                            // Otherwise, evaluate right and ensure it's a boolean
                            let right_val = self.evaluate_expression(right)?;
                            if let Value::Bool(right_bool) = right_val {
                                return Ok(Value::Bool(right_bool));
                            } else {
                                return Err(Error::TypeError(
                                    "Right operand of || must be a boolean".to_string(),
                                ));
                            }
                        }
                        TokenType::AmpersandAmpersand => {
                            // Short-circuit AND: if left is false, result is false
                            if !left_bool {
                                return Ok(Value::Bool(false));
                            }
                            // Otherwise, evaluate right and ensure it's a boolean
                            let right_val = self.evaluate_expression(right)?;
                            if let Value::Bool(right_bool) = right_val {
                                return Ok(Value::Bool(right_bool));
                            } else {
                                return Err(Error::TypeError(
                                    "Right operand of && must be a boolean".to_string(),
                                ));
                            }
                        }
                        _ => Err(Error::SyntaxError("Invalid logical operator".to_string())),
                    }
                } else {
                    Err(Error::TypeError(
                        "Left operand of logical operator must be a boolean".to_string(),
                    ))
                }
            }
            Expr::Call(name, args_exprs) => {
                // Evaluate all arguments
                let mut arg_values = Vec::with_capacity(args_exprs.len());
                for arg_expr in args_exprs {
                    arg_values.push(self.evaluate_expression(arg_expr)?);
                }
                
                // Handle built-in functions specifically
                match name.as_str() {
                    "println" => {
                        // Format and print the values
                        if arg_values.is_empty() {
                            println!();
                        } else {
                            let mut formatted = Vec::with_capacity(arg_values.len());
                            for value in &arg_values {
                                formatted.push(value.to_string_value()?);
                            }
                            let output = formatted.join(" ");
                            println!("{}", output.green());
                        }
                        return Ok(Value::Null);
                    }
                    "inputln" => {
                        if !arg_values.is_empty() {
                            return Err(Error::RuntimeError(
                                "inputln() expects 0 arguments".to_string(),
                            ));
                        }
                        // Optionally print a prompt if we decide to support it
                        // For now, let's keep it simple
                        // print!(""); // Ensure any previous output is flushed
                        // io::stdout().flush().map_err(|e| Error::IoError(e.to_string()))?;
                        
                        let mut buffer = String::new();
                        io::stdin().read_line(&mut buffer)
                            .map_err(|e| Error::RuntimeError(format!("Failed to read line: {}", e)))?;
                        
                        // Trim newline characters from the end
                        let input = buffer.trim_end_matches(|c| c == '\r' || c == '\n').to_string();
                        return Ok(Value::String(input));
                    }
                    "to_int" => {
                        if arg_values.len() != 1 {
                            return Err(Error::RuntimeError(
                                "to_int() expects 1 argument".to_string(),
                            ));
                        }
                        match &arg_values[0] {
                            Value::String(s) => {
                                match s.parse::<i64>() {
                                    Ok(num) => return Ok(Value::Int(num)),
                                    Err(_) => {
                                        return Err(Error::RuntimeError(format!(
                                            "Cannot convert string '{}' to int", s
                                        )));
                                    }
                                }
                            }
                            _ => {
                                return Err(Error::TypeError(format!(
                                    "to_int() expects a string argument, got {}", arg_values[0].get_type()
                                )));
                            }
                        }
                    }
                    _ => { // User-defined functions
                        let function = match self.functions.get(name) {
                            Some(func) => func.clone(),
                            None => return Err(Error::UndefinedError(format!("Function '{}' is not defined", name))),
                        };
                        
                        // Check that the argument count matches
                        if arg_values.len() != function.parameters.len() {
                            return Err(Error::RuntimeError(format!(
                                "Expected {} arguments but got {}",
                                function.parameters.len(),
                                arg_values.len()
                            )));
                        }
                        
                        // Create a new environment for the function call
                        let old_environment = self.environment.clone();
                        
                        // Bind arguments to parameters
                        for (param, value) in function.parameters.iter().zip(arg_values.iter()) {
                            // Check that the argument type matches the parameter type
                            let value_type = value.get_type();
                            if !param.param_type.is_compatible_with(&value_type) {
                                // Restore the old environment
                                self.environment = old_environment;
                                
                                return Err(Error::TypeError(format!(
                                    "Cannot pass argument of type {} to parameter of type {}",
                                    value_type, param.param_type
                                )));
                            }
                            
                            // Add the parameter to the environment
                            self.environment.insert(
                                param.name.clone(),
                                Variable {
                                    var_type: param.param_type.clone(),
                                    value: value.clone(),
                                    is_mutable: false, // Parameters are immutable
                                },
                            );
                        }
                        
                        // Execute the function body
                        let result = match self.execute_statement(&function.body) {
                            Ok(value) => value,
                            Err(Error::RuntimeError(msg)) if msg.starts_with("return: ") => {
                                // Extract the return value
                                let value_str = msg.trim_start_matches("return: ");
                                
                                // Parse the value
                                if value_str == "null" {
                                    Value::Null
                                } else if let Ok(value) = value_str.parse::<i64>() {
                                    Value::Int(value)
                                } else if value_str == "true" {
                                    Value::Bool(true)
                                } else if value_str == "false" {
                                    Value::Bool(false)
                                } else {
                                    // Assume it's a string
                                    Value::String(value_str.to_string())
                                }
                            },
                            Err(err) => {
                                // Restore the old environment
                                self.environment = old_environment;
                                
                                return Err(err);
                            }
                        };
                        
                        // Restore the old environment
                        self.environment = old_environment;
                        
                        // Check that the return type matches if one is specified
                        if let Some(return_type) = &function.return_type {
                            let result_type = result.get_type();
                            if !return_type.is_compatible_with(&result_type) && result != Value::Null {
                                return Err(Error::TypeError(format!(
                                    "Function '{}' expected return type {} but got {}",
                                    function.name, return_type, result_type
                                )));
                            }
                        }
                        
                        Ok(result)
                    }
                }
            },
            Expr::Variable(name) => {
                // Look up the variable in the environment
                match self.environment.get(name) {
                    Some(variable) => Ok(variable.value.clone()),
                    None => Err(Error::UndefinedError(format!("Variable '{}' is not defined", name))),
                }
            },
            Expr::Assign(name, value_expr) => {
                // Evaluate the right-hand side
                let value = self.evaluate_expression(value_expr)?;
                
                // Check if the variable exists
                match self.environment.get(name) {
                    Some(variable) => {
                        // Check if the variable is mutable
                        if !variable.is_mutable {
                            return Err(Error::RuntimeError(
                                format!("Cannot assign to immutable variable '{}'", name)
                            ));
                        }
                        
                        // Check if the value type is compatible with the variable type
                        let value_type = value.get_type();
                        if !variable.var_type.is_compatible_with(&value_type) {
                            return Err(Error::TypeError(format!(
                                "Cannot assign value of type {} to variable of type {}",
                                value_type, variable.var_type
                            )));
                        }
                        
                        // Update the variable
                        self.environment.insert(
                            name.clone(),
                            Variable {
                                var_type: variable.var_type.clone(),
                                value: value.clone(),
                                is_mutable: true,
                            },
                        );
                        
                        Ok(value)
                    },
                    None => Err(Error::UndefinedError(format!("Variable '{}' is not defined", name))),
                }
            },
            Expr::Increment(name) => {
                // Get the current value of the variable
                let variable = match self.environment.get(name) {
                    Some(v) => v.clone(),
                    None => return Err(Error::UndefinedError(format!("Variable '{}' is not defined", name))),
                };
                
                // Ensure the variable is mutable
                if !variable.is_mutable {
                    return Err(Error::RuntimeError(
                        format!("Cannot increment immutable variable '{}'", name)
                    ));
                }
                
                // Ensure the variable is an integer
                if let Value::Int(value) = variable.value {
                    // Increment the value
                    let new_value = Value::Int(value + 1);
                    
                    // Update the variable
                    self.environment.insert(
                        name.clone(),
                        Variable {
                            var_type: variable.var_type,
                            value: new_value.clone(),
                            is_mutable: true,
                        },
                    );
                    
                    Ok(new_value)
                } else {
                    Err(Error::TypeError(
                        format!("Cannot increment variable of type {}", variable.var_type)
                    ))
                }
            },
            Expr::Decrement(name) => {
                // Get the current value of the variable
                let variable = match self.environment.get(name) {
                    Some(v) => v.clone(),
                    None => return Err(Error::UndefinedError(format!("Variable '{}' is not defined", name))),
                };
                
                // Ensure the variable is mutable
                if !variable.is_mutable {
                    return Err(Error::RuntimeError(
                        format!("Cannot decrement immutable variable '{}'", name)
                    ));
                }
                
                // Ensure the variable is an integer
                if let Value::Int(value) = variable.value {
                    // Decrement the value
                    let new_value = Value::Int(value - 1);
                    
                    // Update the variable
                    self.environment.insert(
                        name.clone(),
                        Variable {
                            var_type: variable.var_type,
                            value: new_value.clone(),
                            is_mutable: true,
                        },
                    );
                    
                    Ok(new_value)
                } else {
                    Err(Error::TypeError(
                        format!("Cannot decrement variable of type {}", variable.var_type)
                    ))
                }
            },
            Expr::ListLiteral(exprs) => {
                let mut elements = Vec::new();
                let mut inferred_element_type: Option<Type> = None;

                for expr_element in exprs {
                    let value = self.evaluate_expression(expr_element)?;
                    
                    if inferred_element_type.is_none() {
                        // First element (if not Null) determines the type
                        if value.get_type() != Type::Unknown { // Type::Unknown is Value::Null.get_type()
                            inferred_element_type = Some(value.get_type());
                        }
                    } else {
                        // Subsequent elements must match, Null is compatible with any typed list for now
                        if value.get_type() != Type::Unknown && inferred_element_type.as_ref().unwrap() != &value.get_type() {
                            return Err(Error::TypeError(format!(
                                "List literals must contain elements of the same type. Expected {} but found {}.",
                                inferred_element_type.as_ref().unwrap(), value.get_type()
                            )));
                        }
                    }
                    elements.push(value);
                }

                let final_element_type = inferred_element_type.unwrap_or(Type::Unknown); // If list is empty or only Nulls, type is Unknown
                Ok(Value::List(ListRef::from_vec(elements, final_element_type)))
            },
            Expr::MethodCall(object_expr, method_token, arg_exprs) => {
                let object_value = self.evaluate_expression(object_expr)?;
                let method_name = &method_token.lexeme;

                match object_value {
                    Value::List(list_ref) => { // list_ref is ListRef, not &ListRef here
                        let mut args = Vec::with_capacity(arg_exprs.len());
                        for arg_expr in arg_exprs {
                            args.push(self.evaluate_expression(arg_expr)?);
                        }

                        match method_name.as_str() {
                            "push" => self.list_push(list_ref, args, method_token),
                            "pop"  => self.list_pop(list_ref, args, method_token),
                            "len"  => self.list_len(list_ref, args, method_token),
                            "get"  => self.list_get(list_ref, args, method_token),
                            "set"  => self.list_set(list_ref, args, method_token),
                            _ => Err(Error::RuntimeError(format!(
                                "List has no method named '{}' at line {}.",
                                method_name, method_token.line
                            ))),
                        }
                    }
                    _ => Err(Error::TypeError(format!(
                        "Can only call methods on lists. Found type {} at line {}.",
                        object_value.get_type(), method_token.line
                    ))),
                }
            },
            Expr::ObjectLiteral { properties } => {
                let obj_ref = ObjectRef::new();
                for (key_token, value_expr) in properties {
                    let value = self.evaluate_expression(value_expr)?;
                    obj_ref.properties.borrow_mut().insert(key_token.lexeme.clone(), value);
                }
                Ok(Value::Object(obj_ref))
            },
            Expr::Get { object, name } => {
                let object_val = self.evaluate_expression(object)?;
                match object_val {
                    Value::Object(obj_ref) => {
                        let prop_name = &name.lexeme;
                        match obj_ref.properties.borrow().get(prop_name) {
                            Some(value) => Ok(value.clone()),
                            None => Ok(Value::Null), // Properties not found return null
                        }
                    }
                    _ => Err(Error::TypeError(format!(
                        "Can only access properties on objects. Found type {} at line {}.",
                        object_val.get_type(), name.line
                    ))),
                }
            },
            Expr::Set { object, name, value } => {
                let object_val = self.evaluate_expression(object)?;
                match object_val {
                    Value::Object(obj_ref) => {
                        let prop_name = name.lexeme.clone();
                        let val_to_set = self.evaluate_expression(value)?;
                        obj_ref.properties.borrow_mut().insert(prop_name, val_to_set.clone());
                        Ok(val_to_set)
                    }
                    _ => Err(Error::TypeError(format!(
                        "Can only set properties on objects. Found type {} at line {}.",
                        object_val.get_type(), name.line
                    ))),
                }
            },
        }
    }

    // List method implementations
    fn list_push(&mut self, list_ref: ListRef, args: Vec<Value>, token: &crate::token::Token) -> Result<Value, Error> {
        if args.len() != 1 {
            return Err(Error::RuntimeError(format!(
                "Method 'push' expects 1 argument, got {} at line {}.",
                args.len(), token.line
            )));
        }
        let element_to_push = &args[0];
        
        let mut elements_borrow = list_ref.elements.borrow_mut();
        let mut list_element_type_borrow = list_ref.element_type.borrow_mut();

        if *list_element_type_borrow == Type::Unknown {
            // If list type is Unknown (e.g. from `[]`), specialize it with the type of the first non-Null pushed element.
            if element_to_push.get_type() != Type::Unknown { // Type::Unknown is Value::Null.get_type()
                *list_element_type_borrow = element_to_push.get_type();
            }
        }

        // Type check against the (potentially specialized) list element type.
        // Allow pushing Value::Null (which has Type::Unknown) to any list.
        if *list_element_type_borrow != Type::Unknown && 
           element_to_push.get_type() != Type::Unknown &&
           !list_element_type_borrow.is_compatible_with(&element_to_push.get_type()) {
            return Err(Error::TypeError(format!(
                "Cannot push value of type {} to list of type list<{}> at line {}.",
                element_to_push.get_type(), *list_element_type_borrow, token.line
            )));
        }

        elements_borrow.push(element_to_push.clone());
        Ok(Value::Null)
    }

    fn list_pop(&mut self, list_ref: ListRef, args: Vec<Value>, token: &crate::token::Token) -> Result<Value, Error> {
        if !args.is_empty() {
            return Err(Error::RuntimeError(format!(
                "Method 'pop' expects 0 arguments, got {} at line {}.",
                args.len(), token.line
            )));
        }
        match list_ref.elements.borrow_mut().pop() {
            Some(value) => Ok(value),
            None => Err(Error::RuntimeError(format!("Cannot pop from an empty list at line {}.", token.line))),
        }
    }

    fn list_len(&mut self, list_ref: ListRef, args: Vec<Value>, token: &crate::token::Token) -> Result<Value, Error> {
        if !args.is_empty() {
            return Err(Error::RuntimeError(format!(
                "Method 'len' expects 0 arguments, got {} at line {}.",
                args.len(), token.line
            )));
        }
        let len = list_ref.elements.borrow().len();
        Ok(Value::Int(len as i64))
    }

    fn list_get(&mut self, list_ref: ListRef, args: Vec<Value>, token: &crate::token::Token) -> Result<Value, Error> {
        if args.len() != 1 {
            return Err(Error::RuntimeError(format!(
                "Method 'get' expects 1 argument (index), got {} at line {}.",
                args.len(), token.line
            )));
        }
        match &args[0] {
            Value::Int(index) => {
                let i = *index as usize;
                let elements_borrow = list_ref.elements.borrow();
                if i < elements_borrow.len() {
                    Ok(elements_borrow[i].clone())
                } else {
                    Err(Error::RuntimeError(format!(
                        "List index {} out of bounds (length is {}) at line {}.",
                        index, elements_borrow.len(), token.line
                    )))
                }
            }
            _ => Err(Error::TypeError(format!(
                "List index must be an integer, got {} at line {}.",
                args[0].get_type(), token.line
            ))),
        }
    }

    fn list_set(&mut self, list_ref: ListRef, args: Vec<Value>, token: &crate::token::Token) -> Result<Value, Error> {
        if args.len() != 2 {
            return Err(Error::RuntimeError(format!(
                "Method 'set' expects 2 arguments (index, value), got {} at line {}.",
                args.len(), token.line
            )));
        }
        
        let new_element = &args[1];
        let list_element_type_ro_borrow = list_ref.element_type.borrow();

        if *list_element_type_ro_borrow == Type::Unknown {
            // Cannot 'set' into a list of fully unknown type unless the new element is also Null/Unknown.
            // A list must generally acquire its type via declaration or a 'push' operation first.
            // This also implies we cannot set an element that would define the type of an empty unknown list.
            if new_element.get_type() != Type::Unknown {
                return Err(Error::TypeError(format!(
                    "Cannot 'set' a typed value into a list of unestablished element type (list<unknown>) at line {}. Push an element first or declare list type.",
                    token.line
                )));
            }
        } else if new_element.get_type() != Type::Unknown && // Allow setting Null in a typed list
                  !list_element_type_ro_borrow.is_compatible_with(&new_element.get_type()) {
            return Err(Error::TypeError(format!(
                "Cannot set element of type {} in list of type list<{}> at line {}.",
                new_element.get_type(), *list_element_type_ro_borrow, token.line
            )));
        }

        match &args[0] {
            Value::Int(index) => {
                let i = *index as usize;
                let mut elements_borrow = list_ref.elements.borrow_mut();
                if i < elements_borrow.len() {
                    // If list type was Unknown, and we are setting with a Null, it remains Unknown.
                    // If list type becomes known through this set (e.g. list was unknown, empty, set at 0 with typed value)
                    // this is not handled here. `push` is the primary way to define type for `[]`.
                    // For `set`, we assume the type is already established or the element is Null.
                    elements_borrow[i] = new_element.clone();
                    Ok(Value::Null)
                } else {
                    Err(Error::RuntimeError(format!(
                        "List index {} out of bounds for 'set' (length is {}) at line {}.",
                        index, elements_borrow.len(), token.line
                    )))
                }
            }
            _ => Err(Error::TypeError(format!(
                "List index for 'set' must be an integer, got {} at line {}.",
                args[0].get_type(), token.line
            ))),
        }
    }
}