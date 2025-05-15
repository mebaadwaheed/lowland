use crate::ast::{Expr, ExprKind, Program, Stmt, StmtKind, Parameter};
use crate::error::{Error, ErrorCode, SourceLocation};
use crate::lexer::Lexer;
use crate::parser::Parser;
use crate::token::{TokenType, Token};
use crate::types::Type;
use crate::value::{Value, ListRef, ObjectRef};
use colored::Colorize;
use std::collections::{HashMap};
use std::fs;
use std::path::{PathBuf};
use std::rc::Rc;
use std::cell::{RefCell, Ref, RefMut};
use std::io::{self, Write};

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
    return_type: Type,
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

    /// Stores imported standard library symbols, mapping alias to canonical name (e.g. "sqrt" -> "std::math::sqrt")
    imported_std_symbols: HashMap<String, String>,
}

impl Interpreter {
    pub fn new() -> Self {
        Interpreter {
            environment: HashMap::new(),
            functions: HashMap::new(),
            current_file_path: None,
            module_cache: Rc::new(RefCell::new(HashMap::new())),
            imported_std_symbols: HashMap::new(),
        }
    }

    /// Main entry point for interpreting a file.
    pub fn interpret_file(&mut self, file_path_str: &str) -> Result<Value, Error> {
        let initial_path = PathBuf::from(file_path_str);
        let canonical_path = match fs::canonicalize(&initial_path) {
            Ok(p) => p,
            Err(e) => return Err(Error::io(
                ErrorCode::I0001, // File not found or general access error
                format!("Error accessing file {}: {}", file_path_str, e),
                None, // No specific source line for this type of error yet
            )),
        };

        let source = match fs::read_to_string(&canonical_path) {
            Ok(s) => s,
            Err(e) => return Err(Error::io(
                ErrorCode::I0003, // Read error
                format!("Error reading file {}: {}", canonical_path.display(), e),
                None,
            )),
        };
        
        {
            // Scope for module_cache_refmut to ensure it's dropped before mutable borrow of self
            let mut module_cache_refmut = (*self.module_cache).borrow_mut();
            module_cache_refmut.clear();
        }
        // module_cache_refmut is dropped here
        
        match self.interpret_source_with_path(&source, Some(canonical_path.clone())) {
            Ok((val, defined_functions)) => {
                for func_def in defined_functions {
                    // The name token is now in func_def.parameters and func_def.body (via Stmt.loc)
                    // func_def.name is still a String from the AST name_token.lexeme
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

        let mut lexer = Lexer::new(source);
        let tokens = lexer.scan_tokens()?;
        // For debugging tokens:
        // for token in &tokens { println!("{:?}", token); }

        let mut parser = Parser::new(tokens);
        let program = parser.parse()?;

        let result = self.execute_program(&program)?;

        self.current_file_path = old_path;
        Ok(result)
    }

    /// Execute a whole program
    /// Returns the last statement's value and a list of all functions defined.
    fn execute_program(&mut self, program: &Program) -> Result<(Value, Vec<Function>), Error> {
        let mut last_value = Value::Null;
        let mut defined_functions_in_program = Vec::new();

        // First pass: Collect and register all function definitions from the current program
        // into self.functions so they are available for calls within this same program.
        for stmt_node in &program.statements {
            if let StmtKind::Function { name_token, parameters, return_type, body, is_exported } = &stmt_node.kind {
                let func_def = Function {
                    name: name_token.lexeme.clone(),
                    parameters: parameters.clone(),
                    return_type: return_type.clone(),
                    body: body.clone(),
                    is_exported: *is_exported,
                };
                // Add to the interpreter's main function map immediately.
                // Note: This allows simple function redefinition by overwriting. 
                // Consider adding a warning or error for redefinitions if desired.
                self.functions.insert(func_def.name.clone(), func_def.clone());
                
                // Still collect them to return, potentially for module systems or external introspection.
                defined_functions_in_program.push(func_def);
            }
        }

        // Second pass: Execute all statements (including function calls which can now find definitions)
        for stmt_node in &program.statements { 
            // Function definitions are handled in the first pass; they don't "execute" to a value here.
            if let StmtKind::Function {..} = &stmt_node.kind {
                continue; 
            }

            match self.execute_statement(stmt_node) {
                Ok(value) => last_value = value,
                Err(err) => {
                    // Handle control flow errors that should not stop execution of sibling statements
                    // This logic might need refinement based on how ReturnControlFlow is used by execute_statement
                    match err {
                        Error::ReturnControlFlow(_) => return Err(err), // Propagate return upwards immediately
                        Error::BreakControlFlow => return Err(err), // Propagate break upwards
                        Error::ContinueControlFlow => return Err(err), // Propagate continue upwards
                        _ => return Err(err), // Other errors are fatal for the program execution
                    }
                }
            }
        }
        Ok((last_value, defined_functions_in_program))
    }

    /// Execute a single statement
    fn execute_statement(&mut self, stmt_node: &Stmt) -> Result<Value, Error> {
        let stmt_loc = stmt_node.loc.clone(); // Common location for errors from this statement

        match &stmt_node.kind {
            StmtKind::Expression(expr) => self.evaluate_expression(expr),
            StmtKind::Println(exprs) => {
                let mut values_to_print = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    values_to_print.push(self.evaluate_expression(expr)?);
                }
                if values_to_print.is_empty() {
                    println!();
                } else {
                    let formatted_values: Vec<String> = values_to_print.iter()
                        .map(|v| v.to_string_value().unwrap_or_else(|_| "<unprintable>".to_string())) // Handle potential error from to_string_value
                        .collect();
                    println!("{}", formatted_values.join(" ").green());
                }
                Ok(Value::Null)
            },
            StmtKind::PrintRaw(exprs) => { // New handler for PrintRaw
                let mut raw_values_to_print = Vec::with_capacity(exprs.len());
                for expr in exprs {
                    raw_values_to_print.push(self.evaluate_expression(expr)?);
                }
                if raw_values_to_print.is_empty() {
                    // Print a newline, consistent with println!() in Rust if no args
                    println!(); 
                } else {
                    let formatted_raw_values: Vec<String> = raw_values_to_print.iter()
                        .map(|v| v.to_raw_display_string().unwrap_or_else(|_| "<unprintable_raw>".to_string()))
                        .collect();
                    // Print exactly as joined, then a newline. No coloring for raw.
                    println!("{}", formatted_raw_values.join(" "));
                }
                Ok(Value::Null)
            },
            StmtKind::Var { name_token, var_type, initializer, is_mutable } => {
                let value = match initializer {
                    Some(init_expr) => self.evaluate_expression(init_expr)?,
                    None => {
                        // Implicit initialization to null/default only if mutable or if language rules allow
                        // For now, assuming uninitialized non-mutable is fine until first assignment if language allows, or default.
                        // Let's stick to Value::Null as a default for uninitialized.
                        Value::Null 
                    }
                };
                
                let value_type = value.get_type();
                if value != Value::Null && !var_type.is_compatible_with(&value_type) {
                    // If Value is Null, it's compatible with any type upon declaration (represents uninitialized)
                    return Err(Error::type_error(
                        ErrorCode::T0001, // Type mismatch
                        format!("Cannot initialize variable '{}' of type {} with value of type {}.", name_token.lexeme, var_type, value_type),
                        Some(initializer.as_ref().map_or_else(|| stmt_loc, |e| e.loc.clone())),
                    ));
                }
                
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
            StmtKind::Block(statements) => {
                // Environment scoping for blocks: Create a new scope or manage variable lifetimes.
                // The current code clones the entire environment for restoration on error.
                // A more typical approach for lexical scoping is to push a new env layer and pop it.
                // For simplicity, the clone-and-restore-on-error is kept, but this has implications
                // for how variables defined inside a block persist or shadow outer ones.
                // This model does not automatically handle shadowing or popping scopes on exit without error.
                // This needs to be implemented if true lexical scoping is desired for blocks.
                // For now, let's assume this basic environment handling from the original code is intended.
                let environment_at_block_start = self.environment.clone();
                let mut last_block_value = Value::Null;

                for inner_stmt_node in statements {
                    match self.execute_statement(inner_stmt_node) {
                        Ok(val) => last_block_value = val,
                        Err(Error::ReturnControlFlow(ret_val)) => { // Propagate return
                            self.environment = environment_at_block_start; // Restore env before propagating return from block
                            return Err(Error::ReturnControlFlow(ret_val));
                        },
                        Err(Error::BreakControlFlow) => { // Propagate break
                            self.environment = environment_at_block_start;
                            return Err(Error::BreakControlFlow);
                        },
                        Err(Error::ContinueControlFlow) => { // Propagate continue
                            self.environment = environment_at_block_start;
                            return Err(Error::ContinueControlFlow);
                        },
                        Err(true_err) => { // Any other actual error
                            self.environment = environment_at_block_start;
                            return Err(true_err);
                        }
                    }
                }
                // If block completes normally, changes to environment persist (as per current model).
                Ok(last_block_value)
            },
            StmtKind::If { condition, then_branch, elif_branches, else_branch } => {
                let cond_value = self.evaluate_expression(condition)?;
                let cond_loc = condition.loc.clone();

                if let Value::Bool(cond_bool) = cond_value {
                    if cond_bool {
                        self.execute_statement(then_branch)
                    } else {
                        let mut executed_elif = false;
                        for (elif_cond_expr, elif_branch_stmt) in elif_branches {
                            let elif_cond_value = self.evaluate_expression(elif_cond_expr)?;
                            let elif_cond_loc = elif_cond_expr.loc.clone();
                            if let Value::Bool(elif_bool) = elif_cond_value {
                                if elif_bool {
                                    executed_elif = true;
                                    self.execute_statement(elif_branch_stmt)?;
                                    break;
                                }
                            } else {
                                return Err(Error::type_error(
                                    ErrorCode::T0005, // Expected boolean condition
                                    format!("Elif condition must be a boolean, found {}.", elif_cond_value.get_type()),
                                    Some(elif_cond_loc),
                                ));
                            }
                        }
                        if !executed_elif && else_branch.is_some() {
                            self.execute_statement(else_branch.as_ref().unwrap())
                        } else {
                            Ok(Value::Null)
                        }
                    }
                } else {
                    Err(Error::type_error(
                        ErrorCode::T0005, // Expected boolean condition
                        format!("If condition must be a boolean, found {}.", cond_value.get_type()),
                        Some(cond_loc),
                    ))
                }
            },
            StmtKind::Function { name_token, parameters, return_type, body, is_exported } => {
                // Function declaration itself doesn't produce a value at this stage
                // It's registered during execute_program or by an import mechanism
                // However, we need to ensure this new Function struct is created and stored.
                // This is already handled in execute_program. Here, we just acknowledge it.
                let _ = name_token;
                let _ = parameters;
                let _ = return_type;
                let _ = body;
                let _ = is_exported;
                Ok(Value::Null)
            },
            StmtKind::Return(value_expr) => {
                let return_val = match value_expr {
                    Some(expr) => self.evaluate_expression(expr)?,
                    None => Value::Null,
                };
                // Special error to propagate return value
                Err(Error::ReturnControlFlow(Box::new(return_val)))
            },
            StmtKind::While { condition, body } => {
                loop {
                    let cond_value = self.evaluate_expression(condition)?;
                    let cond_loc = condition.loc.clone();

                    if let Value::Bool(cond_bool) = cond_value {
                        if !cond_bool {
                            break; // Exit loop
                        }
                        match self.execute_statement(body) {
                            Ok(_) => {} // Continue loop
                            Err(Error::BreakControlFlow) => break, // Break from while loop
                            Err(Error::ContinueControlFlow) => continue, // Continue to next iteration of while
                            Err(e) => return Err(e), // Propagate other errors
                        }
                    } else {
                        return Err(Error::type_error(
                            ErrorCode::T0005, // Expected boolean condition
                            format!("While condition must be a boolean, found {}.", cond_value.get_type()),
                            Some(cond_loc),
                        ));
                    }
                }
                Ok(Value::Null) 
            },
            StmtKind::For { initializer, condition, increment, body } => {
                // For loop environment scoping can be tricky. 
                // Create a new scope for the initializer and the loop itself.
                let outer_env = self.environment.clone();

                if let Some(init_stmt) = initializer {
                    self.execute_statement(init_stmt)?; // Initializer runs in the new scope
                }

                loop {
                    let loop_cond_val = match condition {
                        Some(cond_expr) => self.evaluate_expression(cond_expr)?,
                        None => Value::Bool(true), // No condition means infinite loop (until break)
                    };
                    let cond_loc = condition.as_ref().map_or_else(|| stmt_loc.clone(), |c| c.loc.clone());

                    if let Value::Bool(continue_loop) = loop_cond_val {
                        if !continue_loop {
                            break; // Exit loop
                        }

                        match self.execute_statement(body) {
                            Ok(_) => {},
                            Err(Error::BreakControlFlow) => break, // Break from for loop
                            Err(Error::ContinueControlFlow) => { // Continue to increment part of for
                                // Execute increment before continuing
                                if let Some(inc_expr) = increment {
                                    self.evaluate_expression(inc_expr)?;
                                }
                                continue;
                            },
                            Err(e) => { 
                                self.environment = outer_env; // Restore env on error
                                return Err(e); 
                            }
                        }

                        if let Some(inc_expr) = increment {
                            self.evaluate_expression(inc_expr)?;
                        }
                    } else {
                        self.environment = outer_env; // Restore env on error
                        return Err(Error::type_error(
                            ErrorCode::T0005, // Expected boolean condition
                            format!("For loop condition must be boolean, found {}.", loop_cond_val.get_type()),
                            Some(cond_loc),
                        ));
                    }
                }
                self.environment = outer_env; // Restore environment after loop
                Ok(Value::Null)
            },
            StmtKind::Break(break_token) => {
                let _ = break_token; // Mark as unused if only its location (implicit in Stmt) matters
                Err(Error::BreakControlFlow)
            },
            StmtKind::Continue(continue_token) => {
                let _ = continue_token; // Mark as unused
                Err(Error::ContinueControlFlow)
            },
            StmtKind::Including { path_token, path_val, imports } => {
                let import_loc = SourceLocation::new(path_token.line, path_token.column);
                if path_val == "standard" {
                    if let Some(import_items) = imports {
                        for qualified_name in import_items {
                            match qualified_name.as_str() {
                                "std::math::sqrt" => {
                                    self.imported_std_symbols.insert("sqrt".to_string(), qualified_name.clone());
                                }
                                "std::math::abs" => {
                                    self.imported_std_symbols.insert("abs".to_string(), qualified_name.clone());
                                }
                                "std::math::pow" => {
                                    self.imported_std_symbols.insert("pow".to_string(), qualified_name.clone());
                                }
                                "std::math::floor" => {
                                    self.imported_std_symbols.insert("floor".to_string(), qualified_name.clone());
                                }
                                "std::math" => {
                                    // Import all math functions
                                    let math_functions = [
                                        ("sqrt", "std::math::sqrt"),
                                        ("abs", "std::math::abs"),
                                        ("pow", "std::math::pow"),
                                        ("floor", "std::math::floor"),
                                    ];
                                    for (alias, canonical) in math_functions {
                                        self.imported_std_symbols.insert(alias.to_string(), canonical.to_string());
                                    }
                                    
                                    // Create a math object with all the functions
                                    let mut math_obj = ObjectRef::new();
                                    {
                                        let mut props = (*math_obj.properties).borrow_mut();
                                        for (alias, canonical) in math_functions {
                                            props.insert(alias.to_string(), Value::StdFunctionLink(canonical.to_string()));
                                        }
                                    }
                                    self.environment.insert("math".to_string(), Variable {
                                        var_type: Type::Object,
                                        value: Value::Object(math_obj),
                                        is_mutable: false,
                                    });
                                }
                                _ => {
                                    return Err(Error::runtime(
                                        ErrorCode::R0000, 
                                        format!("Unknown standard library symbol '{}'.", qualified_name),
                                        Some(import_loc.clone()), 
                                    ));
                                }
                            }
                        }
                    } else {
                        return Err(Error::syntax(
                            ErrorCode::P0000, 
                            "Specific symbols must be listed when including from \"standard\" (e.g., [std::math::sqrt] or [std::math]).".to_string(),
                            Some(import_loc),
                        ));
                    }
                    Ok(Value::Null)
                } else {
                    // Handle local file imports
                    let current_dir = self.current_file_path.as_ref()
                        .and_then(|p| p.parent())
                        .ok_or_else(|| Error::runtime(
                            ErrorCode::R0000,
                            "Cannot determine current directory for relative imports.".to_string(),
                            Some(import_loc.clone())
                        ))?;

                    let import_path = current_dir.join(&path_val);
                    if !import_path.exists() {
                        return Err(Error::runtime(
                            ErrorCode::I0001,
                            format!("File not found: {}", path_val),
                            Some(import_loc.clone())
                        ));
                    }

                    let source = fs::read_to_string(&import_path).map_err(|e| Error::io(
                        ErrorCode::I0003,
                        format!("Failed to read file '{}': {}", path_val, e),
                        Some(import_loc.clone())
                    ))?;

                    // Interpret the imported file
                    let (_, imported_functions) = self.interpret_source_with_path(&source, Some(import_path.clone()))?;

                    // If specific imports are requested, filter the functions
                    if let Some(requested_imports) = imports {
                        for func in imported_functions {
                            if func.is_exported {
                                let func_name = func.name.clone();
                                if requested_imports.contains(&func_name) {
                                    self.functions.insert(func_name, func);
                                }
                            }
                        }
                    } else {
                        // Import all exported functions
                        for func in imported_functions {
                            if func.is_exported {
                                self.functions.insert(func.name.clone(), func);
                            }
                        }
                    }

                    Ok(Value::Null)
                }
            },
        }
    }

    /// Evaluate an expression to a value
    fn evaluate_expression(&mut self, expr_node: &Expr) -> Result<Value, Error> {
        let _expr_loc = expr_node.loc.clone(); // Common location for errors from this expression, marked unused for now

        match &expr_node.kind {
            ExprKind::Literal(value) => Ok(value.clone()),
            ExprKind::Grouping(expr) => self.evaluate_expression(expr),
            ExprKind::Unary(operator_token, right_expr) => {
                let right_val = self.evaluate_expression(right_expr)?;
                let op_loc = SourceLocation::new(operator_token.line, operator_token.column);

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
                                ErrorCode::T0002, // Operation not supported for type
                                format!("Unary '!' operator can only be applied to booleans, found {}.", right_val.get_type()),
                                Some(op_loc),
                            ))
                        }
                    }
                    _ => Err(Error::syntax(
                        ErrorCode::P0000, // Generic syntax error or a new code for invalid unary op
                        format!("Invalid unary operator '{}'.", operator_token.lexeme),
                        Some(op_loc),
                    )),
                }
            },
            ExprKind::Variable(name_token) => {
                if let Some(var_data) = self.environment.get(&name_token.lexeme) {
                    Ok(var_data.value.clone())
                } else {
                    Err(Error::runtime(
                        ErrorCode::R0001, // Undefined variable
                        format!("Undefined variable '{}'.", name_token.lexeme),
                        Some(SourceLocation::new(name_token.line, name_token.column)),
                    ))
                }
            },
            ExprKind::Assign { name_token, value } => {
                let new_value = self.evaluate_expression(value)?;
                let var_name = &name_token.lexeme;
                let assign_loc = SourceLocation::new(name_token.line, name_token.column);

                if let Some(var_data) = self.environment.get_mut(var_name) {
                    if !var_data.is_mutable {
                        return Err(Error::type_error(
                            ErrorCode::T0004, // Variable not mutable
                            format!("Variable '{}' is not mutable and cannot be reassigned.", var_name),
                            Some(assign_loc),
                        ));
                    }
                    let new_value_type = new_value.get_type();
                    if !var_data.var_type.is_compatible_with(&new_value_type) {
                         return Err(Error::type_error(
                            ErrorCode::T0001, // Type mismatch
                            format!("Cannot assign value of type {} to variable '{}' of type {}.", new_value_type, var_name, var_data.var_type),
                            Some(value.loc.clone()), // location of the value being assigned
                        ));
                    }
                    var_data.value = new_value.clone();
                    Ok(new_value)
                } else {
                    Err(Error::runtime(
                        ErrorCode::R0001, // Undefined variable
                        format!("Undefined variable '{}' cannot be assigned to.", var_name),
                        Some(assign_loc),
                    ))
                }
            },
            ExprKind::Binary(left_expr, operator_token, right_expr) => {
                let left_val = self.evaluate_expression(left_expr)?;
                let right_val = self.evaluate_expression(right_expr)?;
                let op_loc = SourceLocation::new(operator_token.line, operator_token.column);

                match operator_token.token_type {
                    TokenType::Plus => match (left_val, right_val) {
                        (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l + r)),
                        (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l + (r as f64))),
                        (Value::Int(l), Value::Float(r)) => Ok(Value::Float((l as f64) + r)),
                        (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l + r)),
                        (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
                        (l, r) => Err(Error::type_error(
                            ErrorCode::T0003,
                            format!("Operator '+' cannot be applied to types {} and {}. Expected two numbers or two strings.", l.get_type(), r.get_type()),
                            Some(op_loc),
                        )),
                    },
                    TokenType::Minus => match (left_val, right_val) {
                        (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l - r)),
                        (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l - (r as f64))),
                        (Value::Int(l), Value::Float(r)) => Ok(Value::Float((l as f64) - r)),
                        (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l - r)),
                        (l, r) => Err(Error::type_error(
                            ErrorCode::T0003,
                            format!("Operator '-' cannot be applied to types {} and {}. Expected two numbers.", l.get_type(), r.get_type()),
                            Some(op_loc),
                        )),
                    },
                    TokenType::Star => match (left_val, right_val) {
                        (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l * r)),
                        (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l * (r as f64))),
                        (Value::Int(l), Value::Float(r)) => Ok(Value::Float((l as f64) * r)),
                        (Value::Int(l), Value::Int(r)) => Ok(Value::Int(l * r)),
                        (l, r) => Err(Error::type_error(
                            ErrorCode::T0003,
                            format!("Operator '*' cannot be applied to types {} and {}. Expected two numbers.", l.get_type(), r.get_type()),
                            Some(op_loc),
                        )),
                    },
                    TokenType::StarStar => match (left_val, right_val) {
                        (Value::Float(l), Value::Float(r)) => Ok(Value::Float(l.powf(r))),
                        (Value::Float(l), Value::Int(r)) => Ok(Value::Float(l.powf(r as f64))),
                        (Value::Int(l), Value::Float(r)) => Ok(Value::Float((l as f64).powf(r))),
                        (Value::Int(l), Value::Int(r)) => {
                            if r < 0 {
                                // Negative exponent for integers would result in a float.
                                // Or we can disallow or use f64::powf. Let's use f64 for this case.
                                Ok(Value::Float((l as f64).powf(r as f64)))
                            } else {
                                Ok(Value::Int(l.pow(r as u32))) // i64::pow expects u32
                            }
                        }
                        (l, r) => Err(Error::type_error(
                            ErrorCode::T0003,
                            format!("Operator '**' cannot be applied to types {} and {}. Expected two numbers.", l.get_type(), r.get_type()),
                            Some(op_loc),
                        )),
                    },
                    TokenType::Slash => match (left_val, right_val) {
                        (Value::Float(l), Value::Float(r)) => {
                            if r == 0.0 {
                                Err(Error::runtime(ErrorCode::R0002, "Division by zero.".to_string(), Some(op_loc)))
                            } else {
                                Ok(Value::Float(l / r))
                            }
                        }
                        (Value::Float(l), Value::Int(r)) => {
                            if r == 0 {
                                Err(Error::runtime(ErrorCode::R0002, "Division by zero.".to_string(), Some(op_loc)))
                            } else {
                                Ok(Value::Float(l / (r as f64)))
                            }
                        }
                        (Value::Int(l), Value::Float(r)) => {
                            if r == 0.0 {
                                Err(Error::runtime(ErrorCode::R0002, "Division by zero.".to_string(), Some(op_loc)))
                            } else {
                                Ok(Value::Float((l as f64) / r))
                            }
                        }
                        (Value::Int(l), Value::Int(r)) => { // Integer division
                            if r == 0 {
                                Err(Error::runtime(ErrorCode::R0002, "Division by zero.".to_string(), Some(op_loc)))
                            } else {
                                Ok(Value::Int(l / r)) // This is integer division
                            }
                        }
                        (l, r) => Err(Error::type_error(
                            ErrorCode::T0003,
                            format!("Operator '/' cannot be applied to types {} and {}. Expected two numbers.", l.get_type(), r.get_type()),
                            Some(op_loc),
                        )),
                    },
                    TokenType::Modulo => match (left_val, right_val) {
                        // Modulo for floats can be fmod, but let's keep it for integers for now,
                        // as its definition for floats can vary. If needed, can add later.
                        (Value::Int(l), Value::Int(r)) => {
                            if r == 0 {
                                Err(Error::runtime(ErrorCode::R0007, "Modulo by zero.".to_string(), Some(op_loc)))
                            } else {
                                Ok(Value::Int(l % r))
                            }
                        }
                        (l, r) => Err(Error::type_error(
                            ErrorCode::T0003,
                            format!("Operator '%' can only be applied to two integers, found {} and {}.", l.get_type(), r.get_type()),
                            Some(op_loc),
                        )),
                    },
                    TokenType::EqualEqual => match (left_val, right_val) {
                        (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l == r)),
                        (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l == (r as f64))),
                        (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) == r)),
                        // For other types, rely on Value::PartialEq (String, Bool, Int, Null, List/Object by ID)
                        (l, r) => Ok(Value::Bool(l == r)), 
                    },
                    TokenType::BangEqual => match (left_val, right_val) {
                        (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l != r)),
                        (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l != (r as f64))),
                        (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) != r)),
                        (l, r) => Ok(Value::Bool(l != r)),
                    },
                    TokenType::Less => match (left_val, right_val) {
                        (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l < r)),
                        (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l < (r as f64))),
                        (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) < r)),
                        (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l < r)),
                        (l, r) => Err(Error::type_error(
                            ErrorCode::T0003,
                            format!("Operator '<' cannot be applied to types {} and {}. Expected two numbers.", l.get_type(), r.get_type()),
                            Some(op_loc),
                        )),
                    },
                    TokenType::LessEqual => match (left_val, right_val) {
                        (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l <= r)),
                        (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l <= (r as f64))),
                        (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) <= r)),
                        (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l <= r)),
                        (l, r) => Err(Error::type_error(
                            ErrorCode::T0003,
                            format!("Operator '<=' cannot be applied to types {} and {}. Expected two numbers.", l.get_type(), r.get_type()),
                            Some(op_loc),
                        )),
                    },
                    TokenType::Greater => match (left_val, right_val) {
                        (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l > r)),
                        (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l > (r as f64))),
                        (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) > r)),
                        (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l > r)),
                        (l, r) => Err(Error::type_error(
                            ErrorCode::T0003,
                            format!("Operator '>' cannot be applied to types {} and {}. Expected two numbers.", l.get_type(), r.get_type()),
                            Some(op_loc),
                        )),
                    },
                    TokenType::GreaterEqual => match (left_val, right_val) {
                        (Value::Float(l), Value::Float(r)) => Ok(Value::Bool(l >= r)),
                        (Value::Float(l), Value::Int(r)) => Ok(Value::Bool(l >= (r as f64))),
                        (Value::Int(l), Value::Float(r)) => Ok(Value::Bool((l as f64) >= r)),
                        (Value::Int(l), Value::Int(r)) => Ok(Value::Bool(l >= r)),
                        (l, r) => Err(Error::type_error(
                            ErrorCode::T0003,
                            format!("Operator '>=' cannot be applied to types {} and {}. Expected two numbers.", l.get_type(), r.get_type()),
                            Some(op_loc),
                        )),
                    },
                    _ => Err(Error::syntax( 
                        ErrorCode::P0000, // Or a new code for invalid binary operator
                        format!("Invalid binary operator '{}'.", operator_token.lexeme),
                        Some(op_loc),
                    )),
                }
            },
            ExprKind::Logical(left_expr, operator_token, right_expr) => {
                let left_val = self.evaluate_expression(left_expr)?;
                let op_loc = SourceLocation::new(operator_token.line, operator_token.column);

                if let Value::Bool(left_bool) = left_val {
                    match operator_token.token_type {
                        TokenType::PipePipe => {
                            if left_bool {
                                Ok(Value::Bool(true))
                            } else {
                                let right_val = self.evaluate_expression(right_expr)?;
                                if let Value::Bool(right_bool) = right_val {
                                    Ok(Value::Bool(right_bool))
                                } else {
                                    Err(Error::type_error(
                                        ErrorCode::T0005,
                                        format!("Right operand of '||' must be a boolean, found {}.", right_val.get_type()),
                                        Some(right_expr.loc.clone()),
                                    ))
                                }
                            }
                        }
                        TokenType::AmpersandAmpersand => {
                            if !left_bool {
                                Ok(Value::Bool(false))
                            } else {
                                let right_val = self.evaluate_expression(right_expr)?;
                                if let Value::Bool(right_bool) = right_val {
                                    Ok(Value::Bool(right_bool))
                                } else {
                                    Err(Error::type_error(
                                        ErrorCode::T0005,
                                        format!("Right operand of '&&' must be a boolean, found {}.", right_val.get_type()),
                                        Some(right_expr.loc.clone()),
                                    ))
                                }
                            }
                        }
                        _ => Err(Error::syntax(
                            ErrorCode::P0000, // Or new code for invalid logical op
                            format!("Invalid logical operator '{}'.", operator_token.lexeme),
                            Some(op_loc),
                        )),
                    }
                } else {
                    Err(Error::type_error(
                        ErrorCode::T0005,
                        format!("Left operand of logical operator '{}' must be a boolean, found {}.", operator_token.lexeme, left_val.get_type()),
                        Some(left_expr.loc.clone()),
                    ))
                }
            },
            ExprKind::Call { name_token, arguments } => {
                let callee_name = name_token.lexeme.clone();
                let call_loc = SourceLocation::new(name_token.line, name_token.column);

                let mut arg_values = Vec::with_capacity(arguments.len());
                for arg_expr in arguments {
                    arg_values.push(self.evaluate_expression(arg_expr)?);
                }

                // 1. Check always-available built-ins first (excluding those that will be std-namespaced)
                match callee_name.as_str() {
                    "inputln" => {
                        if !arg_values.is_empty() { 
                            return Err(Error::runtime(
                                ErrorCode::R0005, 
                                format!("Built-in function 'inputln()' expects 0 arguments, got {}.", arg_values.len()),
                                Some(call_loc), 
                            ));
                        }
                        print!("> "); 
                        io::stdout().flush().map_err(|e| Error::io(
                            ErrorCode::I0002, 
                            format!("Failed to flush stdout: {}", e), 
                            Some(call_loc.clone()))
                        )?;
                        let mut input = String::new();
                        io::stdin().read_line(&mut input).map_err(|e| Error::io(
                            ErrorCode::I0003,
                            format!("Failed to read line: {}", e),
                            Some(call_loc.clone()))
                        )?;
                        return Ok(Value::String(input.trim_end().to_string()));
                    }
                    "toInt" => {
                        if arg_values.len() != 1 {
                            return Err(Error::runtime(
                                ErrorCode::R0005,
                                format!("Built-in function 'toInt()' expects 1 argument, got {}.", arg_values.len()),
                                Some(call_loc),
                            ));
                        }
                        match &arg_values[0] {
                            Value::String(s) => {
                                return s.parse::<i64>().map(Value::Int).or_else(|_| 
                                    s.parse::<f64>().map(|f_val| Value::Int(f_val as i64))
                                ).map_err(|_e| Error::runtime(
                                    ErrorCode::R0000, 
                                    format!("Cannot convert string '{}' to int.", s),
                                    Some(arguments[0].loc.clone()),
                                ));
                            },
                            Value::Int(i) => return Ok(Value::Int(*i)),
                            Value::Float(f) => return Ok(Value::Int(*f as i64)), 
                            Value::Bool(b) => return Ok(Value::Int(if *b { 1 } else { 0 })), 
                            Value::Null => return Ok(Value::Int(0)), 
                            Value::StdFunctionLink(name) => return Err(Error::type_error(
                                ErrorCode::T0007,
                                format!("Built-in function 'toInt()' cannot convert type Value::StdFunctionLink ({}) to int.", name),
                                Some(arguments[0].loc.clone()),
                            )),
                            other => return Err(Error::type_error(
                                ErrorCode::T0007,
                                format!("Built-in function 'toInt()' cannot convert type {} to int.", other.get_type()),
                                Some(arguments[0].loc.clone()),
                            )),
                        }
                    }
                    "toBool" => {
                        if arg_values.len() != 1 {
                            return Err(Error::runtime(
                                ErrorCode::R0005,
                                format!("Built-in function 'toBool()' expects 1 argument, got {}.", arg_values.len()),
                                Some(call_loc),
                            ));
                        }
                        match &arg_values[0] {
                            Value::Bool(b) => return Ok(Value::Bool(*b)), 
                            Value::Int(i) => return Ok(Value::Bool(*i != 0)), 
                            Value::Float(f) => return Ok(Value::Bool(*f != 0.0)), 
                            Value::String(s) => {
                                let s_lower = s.to_lowercase();
                                if s.is_empty() || s_lower == "false" {
                                    return Ok(Value::Bool(false));
                                } else {
                                    return Ok(Value::Bool(true));
                                }
                            },
                            Value::Null => return Ok(Value::Bool(false)), 
                            Value::List(list_ref) => {
                                let elements: Ref<'_, Vec<Value>> = (*list_ref.elements).borrow();
                                return Ok(Value::Bool(!elements.is_empty()));
                            },
                            Value::Object(obj_ref) => {
                                let properties: Ref<'_, HashMap<String, Value>> = (*obj_ref.properties).borrow();
                                return Ok(Value::Bool(!properties.is_empty()));
                            },
                            Value::StdFunctionLink(_) => return Ok(Value::Bool(true)),
                        }
                    }
                    "toString" => {
                        if arg_values.len() != 1 {
                            return Err(Error::runtime(
                                ErrorCode::R0005,
                                format!("Built-in function 'toString()' expects 1 argument, got {}.", arg_values.len()),
                                Some(call_loc),
                            ));
                        }
                        let value = &arg_values[0];
                        match value.to_string_value() {
                            Ok(s) => return Ok(Value::String(s)),
                            Err(e) => return Err(Error::runtime(
                                ErrorCode::R0000, 
                                format!("Cannot convert value to string: {}", e),
                                Some(arguments[0].loc.clone()),
                            )),
                        }
                    }
                    "toFloat" => {
                        if arg_values.len() != 1 {
                            return Err(Error::runtime(
                                ErrorCode::R0005,
                                format!("Built-in function 'toFloat()' expects 1 argument, got {}.", arg_values.len()),
                                Some(call_loc),
                            ));
                        }
                        match &arg_values[0] {
                            Value::Float(f) => return Ok(Value::Float(*f)),
                            Value::Int(i) => return Ok(Value::Float(*i as f64)),
                            Value::String(s) => {
                                return s.parse::<f64>().map(Value::Float).map_err(|_e| Error::runtime(
                                    ErrorCode::R0000, 
                                    format!("Cannot convert string '{}' to float.", s),
                                    Some(arguments[0].loc.clone()),
                                ));
                            },
                            Value::Bool(b) => return Ok(Value::Float(if *b { 1.0 } else { 0.0 })), 
                            Value::Null => return Ok(Value::Float(0.0)), 
                            Value::StdFunctionLink(name) => return Err(Error::type_error(
                                ErrorCode::T0007,
                                format!("Built-in function 'toFloat()' cannot convert type Value::StdFunctionLink ({}) to float.", name),
                                Some(arguments[0].loc.clone()),
                            )),
                            other => return Err(Error::type_error(
                                ErrorCode::T0007,
                                format!("Built-in function 'toFloat()' cannot convert type {} to float.", other.get_type()),
                                Some(arguments[0].loc.clone()),
                            )),
                        }
                    }
                    _ => { /* Not an always-available built-in, proceed to next checks */ }
                }

                // 2. Check user-defined functions
                if let Some(function_data) = self.functions.get(&callee_name) {
                    let func_params = function_data.parameters.clone();
                    let func_return_type = function_data.return_type.clone();
                    let func_body = function_data.body.clone();
                    let func_name = function_data.name.clone();

                    if func_params.len() != arg_values.len() {
                        return Err(Error::runtime(
                            ErrorCode::R0005,
                            format!(
                                "Function '{}' expected {} arguments but got {}.",
                                func_name, func_params.len(), arg_values.len()
                            ),
                            Some(call_loc.clone()),
                        ));
                    }
                    // Environment setup for the function call
                    // Important: Create a truly new scope or manage environment stack
                    // The current model: clone old, use new, restore old.
                    // This is okay for now but consider a stack-based environment for proper lexical scoping.
                    let old_env_for_function_call = self.environment.clone();
                    let mut function_scope_env = self.environment.clone(); // Start with a copy of the current environment for closures

                    for (i, (param, value)) in func_params.iter().zip(arg_values.iter()).enumerate() {
                        let value_type = value.get_type();
                        if !param.param_type.is_compatible_with(&value_type) {
                            self.environment = old_env_for_function_call; // Restore env before erroring
                            return Err(Error::type_error(
                                ErrorCode::T0007, // Argument type mismatch
                                format!(
                                    "Cannot pass argument {} ('{:?}') of type {} to parameter '{}' (expected type {}) in call to '{}'.",
                                    i + 1, arguments[i].kind, value_type, param.name_token.lexeme, param.param_type, func_name
                                ),
                                Some(arguments[i].loc.clone()),
                            ));
                        }
                        // Add parameter to the function's new scope
                        function_scope_env.insert(param.name_token.lexeme.clone(), Variable {
                            var_type: param.param_type.clone(),
                            value: value.clone(),
                            is_mutable: false, // Parameters are immutable by default
                        });
                    }
                    
                    self.environment = function_scope_env; // Activate the function's environment

                    let actual_return_value = match self.execute_statement(&func_body) { 
                        Ok(_) => Value::Null, // Fell off the end of the function body, implicit null return
                        Err(Error::ReturnControlFlow(val)) => *val, // Explicit return statement
                        Err(e) => { 
                            self.environment = old_env_for_function_call; // Restore environment on other errors
                            return Err(e);
                        }
                    };
                    
                    self.environment = old_env_for_function_call; // Restore the previous environment after function execution

                    // Perform return type checking using the (now non-optional) func_return_type
                    let expected_return_type = &func_return_type;
                    let actual_result_type = actual_return_value.get_type();

                    if !expected_return_type.is_compatible_with(&actual_result_type) {
                        // Specific error for implicit null return when a non-nullable type is expected.
                        if actual_return_value == Value::Null &&
                           (*expected_return_type == Type::Int ||
                            *expected_return_type == Type::Float ||
                            *expected_return_type == Type::Bool) {
                            // String allows implicit null to become "null" via to_string_value,
                            // Object allows null. This is for types that strictly cannot be null implicitly.
                            return Err(Error::type_error(
                                ErrorCode::T0006, // Function returned value of incompatible type
                                format!(
                                    "Function '{}' is declared to return type {} but implicitly returned null (by falling off the end of the function). A function that returns {} must explicitly return a value.",
                                    func_name, expected_return_type, expected_return_type
                                ),
                                Some(call_loc.clone()) 
                            ));
                        }
                        // General incompatibility
                        return Err(Error::type_error(
                            ErrorCode::T0006, // Function returned value of incompatible type
                            format!(
                                "Function '{}' expected to return type {} but execution returned type {}.",
                                func_name, expected_return_type, actual_result_type
                            ),
                            Some(call_loc.clone()) 
                        ));
                    }
                    return Ok(actual_return_value);
                }

                // 3. Check imported standard library symbols (direct calls like sqrt() after individual import)
                if let Some(canonical_name) = self.imported_std_symbols.get(&callee_name) {
                    match canonical_name.as_str() {
                        "std::math::sqrt" => {
                            if arg_values.len() != 1 {
                                return Err(Error::runtime(
                                    ErrorCode::R0005,
                                    format!("std::math::sqrt() expects 1 argument, got {}.", arg_values.len()),
                                    Some(call_loc),
                                ));
                            }
                            match &arg_values[0] {
                                Value::Int(i) => {
                                    if *i < 0 {
                                        return Err(Error::runtime(
                                            ErrorCode::R0000, 
                                            format!("Cannot take sqrt of negative number: {}.", i),
                                            Some(arguments[0].loc.clone()),
                                        ));
                                    }
                                    return Ok(Value::Float((*i as f64).sqrt()));
                                },
                                Value::Float(f) => {
                                    if *f < 0.0 {
                                        return Err(Error::runtime(
                                            ErrorCode::R0000, 
                                            format!("Cannot take sqrt of negative number: {}.", f),
                                            Some(arguments[0].loc.clone()),
                                        ));
                                    }
                                    return Ok(Value::Float(f.sqrt()));
                                },
                                other => return Err(Error::type_error(
                                    ErrorCode::T0007,
                                    format!("std::math::sqrt() expects a number argument, got {}.", other.get_type()),
                                    Some(arguments[0].loc.clone()),
                                )),
                            }
                        }
                        "std::math::abs" => {
                            if arg_values.len() != 1 {
                                return Err(Error::runtime(ErrorCode::R0005, format!("std::math::abs() expects 1 argument, got {}.", arg_values.len()), Some(call_loc)));
                            }
                            match &arg_values[0] {
                                Value::Int(i) => Ok(Value::Int(i.abs())),
                                Value::Float(f) => Ok(Value::Float(f.abs())),
                                other => Err(Error::type_error(ErrorCode::T0007, format!("std::math::abs() expects a number argument, got {}.", other.get_type()), Some(arguments[0].loc.clone()))),
                            }
                        }
                        "std::math::pow" => {
                            if arg_values.len() != 2 {
                                return Err(Error::runtime(ErrorCode::R0005, format!("std::math::pow() expects 2 arguments, got {}.", arg_values.len()), Some(call_loc)));
                            }
                            let base = &arg_values[0];
                            let exponent = &arg_values[1];
                            match (base, exponent) {
                                (Value::Float(b), Value::Float(e)) => Ok(Value::Float(b.powf(*e))),
                                (Value::Float(b), Value::Int(e)) => Ok(Value::Float(b.powf(*e as f64))),
                                (Value::Int(b), Value::Float(e)) => Ok(Value::Float((*b as f64).powf(*e))),
                                (Value::Int(b), Value::Int(e)) => { if *e < 0 { Ok(Value::Float((*b as f64).powf(*e as f64))) } else { Ok(Value::Int(b.pow(*e as u32))) } },
                                (b, e) => Err(Error::type_error(ErrorCode::T0003, format!("std::math::pow() expects two number arguments, found {} and {}.", b.get_type(), e.get_type()), Some(call_loc))),
                            }
                        }
                        "std::math::floor" => {
                            if arg_values.len() != 1 {
                                return Err(Error::runtime(ErrorCode::R0005, format!("std::math::floor() expects 1 argument, got {}.", arg_values.len()), Some(call_loc)));
                            }
                            match &arg_values[0] {
                                Value::Float(f) => Ok(Value::Float(f.floor())),
                                Value::Int(i) => Ok(Value::Int(*i)),
                                other => Err(Error::type_error(ErrorCode::T0007, format!("std::math::floor() expects a number argument, got {}.", other.get_type()), Some(arguments[0].loc.clone()))),
                            }
                        }
                        _ => {
                            return Err(Error::runtime(
                                ErrorCode::R0000, // Or a more specific internal error
                                format!("Internal error: Imported standard symbol '{}' (aliased as '{}') has no implementation.", canonical_name, callee_name),
                                Some(call_loc),
                            ));
                        }
                    }
                } else {
                    return Err(Error::runtime(ErrorCode::R0004, format!("Function '{}' is not defined.", callee_name), Some(call_loc)));
                }
            },
            ExprKind::Increment(token) => {
                let _op_loc = expr_node.loc.clone(); // Marked as unused
                let name_token = token; 

                let var_name = &name_token.lexeme;
                if let Some(var_data) = self.environment.get_mut(var_name) {
                    if !var_data.is_mutable {
                        return Err(Error::type_error(
                            ErrorCode::T0004, 
                            format!("Cannot increment non-mutable variable '{}'.", var_name),
                            Some(SourceLocation::new(name_token.line, name_token.column)),
                        ));
                    }
                    if let Value::Int(val) = var_data.value {
                        var_data.value = Value::Int(val + 1);
                        Ok(Value::Int(val + 1))
                    } else {
                        Err(Error::type_error(
                            ErrorCode::R0006, // Invalid inc/dec target type
                            format!("Cannot increment variable '{}' of type {}. Expected integer.", var_name, var_data.value.get_type()),
                            Some(SourceLocation::new(name_token.line, name_token.column)),
                        ))
                    }
                } else {
                    Err(Error::runtime(
                        ErrorCode::R0001,
                        format!("Undefined variable '{}' for increment.", var_name),
                        Some(SourceLocation::new(name_token.line, name_token.column)),
                    ))
                }
            },
            ExprKind::Decrement(token) => {
                let _op_loc = expr_node.loc.clone(); // Marked as unused
                let name_token = token;

                let var_name = &name_token.lexeme;
                if let Some(var_data) = self.environment.get_mut(var_name) {
                    if !var_data.is_mutable {
                        return Err(Error::type_error(
                            ErrorCode::T0004, 
                            format!("Cannot decrement non-mutable variable '{}'.", var_name),
                            Some(SourceLocation::new(name_token.line, name_token.column)),
                        ));
                    }
                    if let Value::Int(val) = var_data.value {
                        var_data.value = Value::Int(val - 1);
                        Ok(Value::Int(val - 1))
                    } else {
                        Err(Error::type_error(
                            ErrorCode::R0006, // Invalid inc/dec target type
                            format!("Cannot decrement variable '{}' of type {}. Expected integer.", var_name, var_data.value.get_type()),
                            Some(SourceLocation::new(name_token.line, name_token.column)),
                        ))
                    }
                } else {
                    Err(Error::runtime(
                        ErrorCode::R0001,
                        format!("Undefined variable '{}' for decrement.", var_name),
                        Some(SourceLocation::new(name_token.line, name_token.column)),
                    ))
                }
            },
            ExprKind::ListLiteral(elements) => {
                let mut list_elements = Vec::new();
                let mut inferred_element_type: Option<Type> = None;
                let _expr_loc = expr_node.loc.clone(); // Marked as unused

                for (i, elem_expr) in elements.iter().enumerate() {
                    let value = self.evaluate_expression(elem_expr)?;
                    let current_element_type = value.get_type();

                    if i == 0 && value != Value::Null {
                        inferred_element_type = Some(current_element_type.clone());
                    } else if let Some(ref expected_type) = inferred_element_type {
                        if !expected_type.is_compatible_with(&current_element_type) && value != Value::Null {
                            return Err(Error::type_error(
                                ErrorCode::T0001, 
                                format!(
                                    "List elements must be of the same type. Inferred type {} but element {} has type {}.",
                                    expected_type, i, current_element_type
                                ),
                                Some(elem_expr.loc.clone()),
                            ));
                        }
                    } else if inferred_element_type.is_none() && value != Value::Null {
                            inferred_element_type = Some(current_element_type.clone());
                    }
                    
                    list_elements.push(value);
                }
                
                let final_element_type = inferred_element_type.unwrap_or(Type::Unknown);
                
                Ok(Value::List(ListRef::from_vec(list_elements, final_element_type)))
            },
            ExprKind::ObjectLiteral { properties } => {
                let mut object_map_for_ref = HashMap::new();
                for (name_token, value_expr) in properties {
                    let value = self.evaluate_expression(value_expr)?;
                    object_map_for_ref.insert(name_token.lexeme.clone(), value);
                }
                let obj_ref = ObjectRef::new();
                {
                    // Scope for props_map to ensure it's dropped before obj_ref is moved
                    let mut props_map: RefMut<'_, HashMap<String, Value>> = (*obj_ref.properties).borrow_mut();
                    props_map.clear();
                    props_map.extend(object_map_for_ref);
                }
                // props_map is dropped here
                Ok(Value::Object(obj_ref))
            },
            ExprKind::Get { object, name } => { 
                let object_val = self.evaluate_expression(object)?;
                let prop_name = &name.lexeme;
                let get_loc = SourceLocation::new(name.line, name.column);

                match object_val {
                    Value::Object(obj_ref) => {
                        let map: Ref<'_, HashMap<String, Value>> = (*obj_ref.properties).borrow();
                        if let Some(value) = map.get(prop_name) {
                            Ok(value.clone())
                        } else {
                            Err(Error::runtime(
                                ErrorCode::R0003, 
                                format!("Property '{}' not found on object.", prop_name),
                                Some(get_loc),
                            ))
                        }
                    }
                    Value::List(list_ref) => { 
                        if prop_name == "len" {
                            let elements: Ref<'_, Vec<Value>> = (*list_ref.elements).borrow();
                            Ok(Value::Int(elements.len() as i64))
                        } else {
                            Err(Error::type_error(
                                ErrorCode::T0008, 
                                format!("Type List has no property '{}'. Did you mean method .len()?", prop_name),
                                Some(get_loc),
                            ))
                        }
                    }
                    other => Err(Error::type_error(
                        ErrorCode::T0008, 
                        format!("Cannot access property '{}' on type {}. Expected object or list.", prop_name, other.get_type()),
                        Some(object.loc.clone()), 
                    )),
                }
            },
            ExprKind::Set { object, name, value } => { 
                let object_val = self.evaluate_expression(object)?;
                let new_prop_value = self.evaluate_expression(value)?;
                let prop_name = &name.lexeme;
                let _set_loc = SourceLocation::new(name.line, name.column); // Reverted to original, can be used for errors if needed

                match object_val {
                    Value::Object(obj_ref) => {
                        let mut props: RefMut<'_, HashMap<String, Value>> = (*obj_ref.properties).borrow_mut();
                        props.insert(prop_name.clone(), new_prop_value.clone());
                        Ok(new_prop_value)
                    }
                    other => Err(Error::type_error(
                        ErrorCode::T0009, 
                        format!("Cannot set property '{}' on type {}. Expected object.", prop_name, other.get_type()),
                        Some(object.loc.clone()),
                    )),
                }
            },
            ExprKind::MethodCall { object, method_name_token, arguments } => {
                let object_value = self.evaluate_expression(object)?;
                let method_name = &method_name_token.lexeme;
                let call_loc = SourceLocation::new(method_name_token.line, method_name_token.column);

                let mut arg_values = Vec::with_capacity(arguments.len());
                let mut arg_locs = Vec::with_capacity(arguments.len());
                for arg_expr in arguments {
                    arg_locs.push(arg_expr.loc.clone());
                    arg_values.push(self.evaluate_expression(arg_expr)?);
                }

                if let Value::Object(obj_ref) = &object_value {
                    let props = obj_ref.properties.borrow();
                    if let Some(Value::StdFunctionLink(canonical_name)) = props.get(method_name) {
                        let canonical_name_clone = canonical_name.clone();
                        match canonical_name_clone.as_str() {
                            "std::math::sqrt" => {
                                if arg_values.len() != 1 { return Err(Error::runtime(ErrorCode::R0005, format!("std::math::sqrt() expects 1 argument, got {}.", arg_values.len()), Some(call_loc))); }
                                match &arg_values[0] {
                                    Value::Int(i) => { if *i < 0 { return Err(Error::runtime(ErrorCode::R0000, format!("Cannot take sqrt of negative number: {}.", i), Some(arg_locs[0].clone()))); } Ok(Value::Float((*i as f64).sqrt())) },
                                    Value::Float(f) => { if *f < 0.0 { return Err(Error::runtime(ErrorCode::R0000, format!("Cannot take sqrt of negative number: {}.", f), Some(arg_locs[0].clone()))); } Ok(Value::Float(f.sqrt())) },
                                    other => Err(Error::type_error(ErrorCode::T0007, format!("std::math::sqrt() expects a number argument, got {}.", other.get_type()), Some(arg_locs[0].clone()))),
                                }
                            }
                            "std::math::abs" => {
                                if arg_values.len() != 1 { return Err(Error::runtime(ErrorCode::R0005, format!("std::math::abs() expects 1 argument, got {}.", arg_values.len()), Some(call_loc))); }
                                match &arg_values[0] {
                                    Value::Int(i) => Ok(Value::Int(i.abs())),
                                    Value::Float(f) => Ok(Value::Float(f.abs())),
                                    other => Err(Error::type_error(ErrorCode::T0007, format!("std::math::abs() expects a number argument, got {}.", other.get_type()), Some(arg_locs[0].clone()))),
                                }
                            }
                            "std::math::pow" => {
                                if arg_values.len() != 2 { return Err(Error::runtime(ErrorCode::R0005, format!("std::math::pow() expects 2 arguments, got {}.", arg_values.len()), Some(call_loc))); }
                                let base_val = &arg_values[0]; let exp_val = &arg_values[1];
                                match (base_val, exp_val) {
                                    (Value::Float(b), Value::Float(e)) => Ok(Value::Float(b.powf(*e))),
                                    (Value::Float(b), Value::Int(e)) => Ok(Value::Float(b.powf(*e as f64))),
                                    (Value::Int(b), Value::Float(e)) => Ok(Value::Float((*b as f64).powf(*e))),
                                    (Value::Int(b), Value::Int(e)) => { if *e < 0 { Ok(Value::Float((*b as f64).powf(*e as f64))) } else { Ok(Value::Int(b.pow(*e as u32))) } },
                                    (b, e) => Err(Error::type_error(ErrorCode::T0003, format!("std::math::pow() expects two number arguments, found {} and {}.", b.get_type(), e.get_type()), Some(call_loc))),
                                }
                            }
                            "std::math::floor" => {
                                if arg_values.len() != 1 { return Err(Error::runtime(ErrorCode::R0005, format!("std::math::floor() expects 1 argument, got {}.", arg_values.len()), Some(call_loc))); }
                                match &arg_values[0] {
                                    Value::Float(f) => Ok(Value::Float(f.floor())),
                                    Value::Int(i) => Ok(Value::Int(*i)),
                                    other => Err(Error::type_error(ErrorCode::T0007, format!("std::math::floor() expects a number argument, got {}.", other.get_type()), Some(arg_locs[0].clone()))),
                                }
                            }
                            _ => Err(Error::runtime(ErrorCode::R0000, format!("Internal error: StdFunctionLink '{}' has no implementation.", canonical_name_clone), Some(call_loc))),
                        }
                    } else {
                        // Object, but the property was not a StdFunctionLink. This means it's not a known module method.
                        // For generic objects, direct method calls like this aren't supported yet.
                        return Err(Error::runtime(
                            ErrorCode::R0008, // Method not found on object
                            format!("Method '{}' not found on object.", method_name),
                            Some(call_loc.clone()),
                        ));
                    }
                } else {
                    // Not an object, try list method call (this part should be fine as object_value is moved)
                    self.handle_list_method_call(object_value, method_name, arg_values, &call_loc, &arg_locs)
                }
            },
            // The following catch-all pattern was previously triggering an unreachable_patterns warning.
            // It's commented out because all ExprKind variants should now be explicitly handled above.
            // If new ExprKind variants are added, they must be handled, or this can be re-enabled (with a specific error code).
            // _ => Err(Error::runtime(ErrorCode::GEN001, "Unsupported expression kind encountered in interpreter.".to_string(), Some(expr_loc)))
        }
    }

    // Helper function to avoid duplicating list method call logic
    fn handle_list_method_call(&mut self, object_value: Value, method_name: &str, arg_values: Vec<Value>, call_loc: &SourceLocation, arg_locs: &[SourceLocation]) -> Result<Value, Error> {
        match object_value {
            Value::List(list_ref) => {
                let dummy_token_for_list_method = Token {token_type: TokenType::Identifier, lexeme: method_name.to_string(), line: call_loc.line, column: call_loc.column, value: None};
                let first_arg_loc = if arg_locs.is_empty() {call_loc} else {&arg_locs[0]};
                let second_arg_loc = if arg_locs.len() > 1 {Some(&arg_locs[1])} else {None};

                match method_name {
                    "push" => self.list_push(list_ref, arg_values, &dummy_token_for_list_method, first_arg_loc),
                    "pop" => self.list_pop(list_ref, arg_values, &dummy_token_for_list_method),
                    "len" => self.list_len(list_ref, arg_values, &dummy_token_for_list_method),
                    "get" => self.list_get(list_ref, arg_values, &dummy_token_for_list_method, first_arg_loc),
                    "set" => self.list_set(list_ref, arg_values, &dummy_token_for_list_method, first_arg_loc, second_arg_loc),
                    _ => Err(Error::runtime(ErrorCode::R0008, format!("List has no method named '{}'.", method_name), Some(call_loc.clone()))),
                }
            }
            _ => Err(Error::type_error(ErrorCode::T0010, format!("Cannot call method '{}' on type {}. Expected list or module object.", method_name, object_value.get_type()), Some(call_loc.clone()))),
        }
    }

    // START: Added List Helper Methods
    fn list_push(&mut self, list_ref: ListRef, arg_values: Vec<Value>, method_token: &Token, value_loc: &SourceLocation) -> Result<Value, Error> {
        if arg_values.len() != 1 {
            return Err(Error::runtime(
                ErrorCode::R0005, 
                format!("List method '{}' expects 1 argument (value to push), got {}.", method_token.lexeme, arg_values.len()),
                Some(SourceLocation::new(method_token.line, method_token.column)),
            ));
        }
        let value_to_push = &arg_values[0];
        let value_type = value_to_push.get_type();
        let list_element_type_rc = list_ref.element_type.clone();

        let type_compatible;
        {
            let current_list_type_view = list_element_type_rc.borrow(); // Immutable borrow
            if *current_list_type_view == Type::Unknown && value_to_push != &Value::Null {
                drop(current_list_type_view); // Explicitly drop immutable before mutable
                let mut list_type_mut = list_element_type_rc.borrow_mut();
                *list_type_mut = value_type.clone();
                type_compatible = true; // Type updated, so it's compatible
            } else {
                // Check compatibility with the existing type (or if value is null)
                if current_list_type_view.is_compatible_with(&value_type) || value_to_push == &Value::Null {
                    type_compatible = true;
                } else {
                    type_compatible = false;
                    // Error will be returned after borrow drops
                }
            }
        } // Immutable borrow current_list_type_view drops here if not dropped earlier

        if !type_compatible { // Check flag set within the block
             // Re-borrow to get the type for the error message, safely.
            let final_list_type = list_element_type_rc.borrow();
            return Err(Error::type_error(
                ErrorCode::T0001, 
                format!(
                    "Cannot push value of type {} to list of type {}.",
                    value_type, *final_list_type
                ),
                Some(value_loc.clone()),
            ));
        }

        let mut elements = list_ref.elements.borrow_mut();
        elements.push(value_to_push.clone());
        Ok(Value::Null) 
    }

    fn list_len(&mut self, list_ref: ListRef, arg_values: Vec<Value>, method_token: &Token) -> Result<Value, Error> {
        if !arg_values.is_empty() {
            return Err(Error::runtime(
                ErrorCode::R0005, 
                format!("List method '{}' expects 0 arguments, got {}.", method_token.lexeme, arg_values.len()),
                Some(SourceLocation::new(method_token.line, method_token.column)),
            ));
        }
        let elements = list_ref.elements.borrow();
        Ok(Value::Int(elements.len() as i64))
    }

    fn list_pop(&mut self, list_ref: ListRef, arg_values: Vec<Value>, method_token: &Token) -> Result<Value, Error> {
        if !arg_values.is_empty() {
            return Err(Error::runtime(
                ErrorCode::R0005,
                format!("List method '{}' expects 0 arguments, got {}.", method_token.lexeme, arg_values.len()),
                Some(SourceLocation::new(method_token.line, method_token.column)),
            ));
        }
        let mut elements = list_ref.elements.borrow_mut();
        if elements.is_empty() {
            return Err(Error::runtime(
                ErrorCode::R0000, // Consider a specific error code for "pop from empty list"
                format!("Cannot pop from an empty list."),
                Some(SourceLocation::new(method_token.line, method_token.column)),
            ));
        }
        Ok(elements.pop().unwrap_or(Value::Null)) // .pop() returns Option, unwrap_or for safety, though check above should prevent None
    }

    fn list_get(&mut self, list_ref: ListRef, arg_values: Vec<Value>, method_token: &Token, index_expr_loc: &SourceLocation) -> Result<Value, Error> {
        if arg_values.len() != 1 {
            return Err(Error::runtime(
                ErrorCode::R0005,
                format!("List method '{}' expects 1 argument (index), got {}.", method_token.lexeme, arg_values.len()),
                Some(SourceLocation::new(method_token.line, method_token.column)),
            ));
        }
        let index_val = &arg_values[0];
        if let Value::Int(idx) = index_val {
            let elements = list_ref.elements.borrow();
            if *idx < 0 || *idx as usize >= elements.len() {
                return Err(Error::runtime(
                    ErrorCode::R0000, // Index out of bounds
                    format!("List index {} out of bounds for list of length {}.", idx, elements.len()),
                    Some(index_expr_loc.clone()),
                ));
            }
            Ok(elements[*idx as usize].clone())
        } else {
            Err(Error::type_error(
                ErrorCode::T0007,
                format!("List index must be an integer, got {}.", index_val.get_type()),
                Some(index_expr_loc.clone()),
            ))
        }
    }

    fn list_set(&mut self, list_ref: ListRef, arg_values: Vec<Value>, method_token: &Token, index_expr_loc: &SourceLocation, value_expr_loc: Option<&SourceLocation>) -> Result<Value, Error> {
        if arg_values.len() != 2 {
            return Err(Error::runtime(
                ErrorCode::R0005,
                format!("List method '{}' expects 2 arguments (index, value), got {}.", method_token.lexeme, arg_values.len()),
                Some(SourceLocation::new(method_token.line, method_token.column)),
            ));
        }
        let index_val = &arg_values[0];
        let value_to_set = &arg_values[1];
        let final_value_loc = value_expr_loc.unwrap_or(index_expr_loc); // Use index loc if value loc not specifically available

        if let Value::Int(idx) = index_val {
            let value_type = value_to_set.get_type();
            let list_element_type_rc = list_ref.element_type.clone();
            
            {
                let mut list_element_type_ref_mut = list_element_type_rc.borrow_mut();
                if *list_element_type_ref_mut == Type::Unknown && value_to_set != &Value::Null {
                    *list_element_type_ref_mut = value_type.clone(); // Infer type from this assignment
                } else if !list_element_type_ref_mut.is_compatible_with(&value_type) && value_to_set != &Value::Null {
                    return Err(Error::type_error(
                        ErrorCode::T0001,
                        format!("Cannot set list element of type {} with value of type {}.", *list_element_type_ref_mut, value_type),
                        Some(final_value_loc.clone()),
                    ));
                }
            } // Mut borrow of list_element_type_rc ends here
            
            let mut elements = list_ref.elements.borrow_mut();
            if *idx < 0 || *idx as usize >= elements.len() {
                return Err(Error::runtime(
                    ErrorCode::R0000, // Index out of bounds
                    format!("List index {} out of bounds for assignment to list of length {}.", idx, elements.len()),
                    Some(index_expr_loc.clone()),
                ));
            }
            elements[*idx as usize] = value_to_set.clone();
            Ok(Value::Null)
        } else {
            Err(Error::type_error(
                ErrorCode::T0007,
                format!("List index must be an integer for set operation, got {}.", index_val.get_type()),
                Some(index_expr_loc.clone()),
            ))
        }
    }
    // END: Added List Helper Methods
}