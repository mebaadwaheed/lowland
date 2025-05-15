use crate::token::Token;
use crate::types::Type;
use crate::value::Value;

/// Represents an expression in the Lowland language
#[derive(Debug, Clone)]
pub enum Expr {
    /// A literal value
    Literal(Value),
    
    /// A grouped expression: (expr)
    Grouping(Box<Expr>),
    
    /// A unary expression: !expr, -expr
    Unary(Token, Box<Expr>),
    
    /// A binary expression: expr + expr, expr == expr
    Binary(Box<Expr>, Token, Box<Expr>),
    
    /// A logical expression: expr && expr, expr || expr
    Logical(Box<Expr>, Token, Box<Expr>),
    
    /// A function call: println(expr, expr, ...)
    Call(String, Vec<Expr>),
    
    /// A variable reference
    Variable(String),
    
    /// An assignment expression: var = expr
    Assign(String, Box<Expr>),
    
    /// Increment expression: var++
    Increment(String),
    
    /// Decrement expression: var--
    Decrement(String),

    /// A list literal e.g. [1, "two", true]
    ListLiteral(Vec<Expr>),

    /// A method call on an object: object.method(args)
    MethodCall(Box<Expr>, Token, Vec<Expr>), // object, method_name_token, arguments

    // Object-related expressions
    /// An object literal e.g. { key1: value1, key2: value2 }
    ObjectLiteral { properties: Vec<(Token, Expr)> }, // key_token, value_expr

    /// Property access: object.property
    Get { object: Box<Expr>, name: Token }, // object_expr, property_name_token

    /// Property assignment: object.property = value
    Set { object: Box<Expr>, name: Token, value: Box<Expr> }, // object_expr, property_name_token, value_expr
}

/// Parameter for a function declaration
#[derive(Debug, Clone)]
pub struct Parameter {
    pub name: String,
    pub param_type: Type,
}

/// Represents a statement in the Lowland language
#[derive(Debug, Clone)]
pub enum Stmt {
    /// An expression statement: expr;
    Expression(Expr),
    
    /// A println statement: println(expr, expr, ...);
    Println(Vec<Expr>),
    
    /// A variable declaration: let var: type = expr;
    Var(String, Type, Option<Expr>, bool),  // name, type, initializer, is_mutable
    
    /// A block statement containing a list of statements
    Block(Vec<Stmt>),
    
    /// An if statement: if (condition) { then_branch } elif (condition) { elif_branch } else { else_branch }
    If(Expr, Box<Stmt>, Vec<(Expr, Box<Stmt>)>, Option<Box<Stmt>>),  // condition, then_branch, elif_branches, else_branch
    
    /// A function declaration: func name(param1: type, param2: type) { body }
    Function(String, Vec<Parameter>, Option<Type>, Box<Stmt>, bool),  // name, parameters, return_type, body, is_exported
    
    /// A return statement: return expr;
    Return(Option<Expr>),  // optional return value

    /// A while loop: while (condition) { body }
    While(Expr, Box<Stmt>),  // condition, body
    
    /// A for loop: for (init; condition; update) { body }
    For(Box<Stmt>, Expr, Box<Expr>, Box<Stmt>),  // initialization, condition, update, body
    
    /// A break statement: break;
    Break,
    
    /// A continue statement: continue;
    Continue,

    /// An including statement: including "path/to/file.lln"; or including [foo, bar] from "path/to/file.lln";
    Including { path: String, imports: Option<Vec<Token>> }, // file path, optional list of specific identifiers (Tokens) to import
}

/// Represents a program - the root of the AST
#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Stmt>,
} 