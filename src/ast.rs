use crate::token::Token;
use crate::types::Type;
use crate::value::Value;
use crate::error::SourceLocation;

/// Represents an expression in the Lowland language
#[derive(Debug, Clone)]
pub struct Expr {
    pub kind: ExprKind,
    pub loc: SourceLocation,
}

impl Expr {
    pub fn new(kind: ExprKind, loc: SourceLocation) -> Self {
        Self { kind, loc }
    }
}

#[derive(Debug, Clone)]
pub enum ExprKind {
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
    Call { name_token: Token, arguments: Vec<Expr> },
    
    /// A variable reference
    Variable(Token),
    
    /// An assignment expression: var = expr
    Assign { name_token: Token, value: Box<Expr> },
    
    /// Increment expression: var++
    Increment(Token),
    
    /// Decrement expression: var--
    Decrement(Token),

    /// A list literal e.g. [1, "two", true]
    ListLiteral(Vec<Expr>),

    /// A method call on an object: object.method(args)
    MethodCall { object: Box<Expr>, method_name_token: Token, arguments: Vec<Expr> },

    // Object-related expressions
    /// An object literal e.g. { key1: value1, key2: value2 }
    ObjectLiteral { properties: Vec<(Token, Expr)> },

    /// Property access: object.property
    Get { object: Box<Expr>, name: Token },

    /// Property assignment: object.property = value
    Set { object: Box<Expr>, name: Token, value: Box<Expr> },
}

/// Parameter for a function declaration
#[derive(Debug, Clone)]
pub struct Parameter {
    pub name_token: Token,
    pub param_type: Type,
}

/// Represents a statement in the Lowland language
#[derive(Debug, Clone)]
pub struct Stmt {
    pub kind: StmtKind,
    pub loc: SourceLocation,
}

impl Stmt {
    pub fn new(kind: StmtKind, loc: SourceLocation) -> Self {
        Self { kind, loc }
    }
}

#[derive(Debug, Clone)]
pub enum StmtKind {
    /// An expression statement: expr;
    Expression(Expr),
    
    /// A println statement: println(expr, expr, ...);
    Println(Vec<Expr>),
    
    /// A variable declaration: let var: type = expr;
    Var { name_token: Token, var_type: Type, initializer: Option<Expr>, is_mutable: bool },
    
    /// A block statement containing a list of statements
    Block(Vec<Stmt>),
    
    /// An if statement: if (condition) { then_branch } elif (condition) { elif_branch } else { else_branch }
    If { condition: Expr, then_branch: Box<Stmt>, elif_branches: Vec<(Expr, Box<Stmt>)>, else_branch: Option<Box<Stmt>> },
    
    /// A function declaration: func name(param1: type, param2: type) { body }
    Function { name_token: Token, parameters: Vec<Parameter>, return_type: Type, body: Box<Stmt>, is_exported: bool },
    
    /// A return statement: return expr;
    Return(Option<Expr>),

    /// A while loop: while (condition) { body }
    While { condition: Expr, body: Box<Stmt> },
    
    /// A for loop: for (init; condition; update) { body }
    For { initializer: Option<Box<Stmt>>, condition: Option<Expr>, increment: Option<Box<Expr>>, body: Box<Stmt> },
    
    /// A break statement: break;
    Break(Token),
    
    /// A continue statement: continue;
    Continue(Token),

    /// An including statement: including "path/to/file.lln"; or including [item1, item2::subitem] from "path/to/file.lln";
    Including { path_token: Token, path_val: String, imports: Option<Vec<String>> },

    /// A println! statement: println!(expr, expr, ...);
    PrintRaw(Vec<Expr>),
}

/// Represents a program - the root of the AST
#[derive(Debug, Clone)]
pub struct Program {
    pub statements: Vec<Stmt>,
} 