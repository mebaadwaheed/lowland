use std::fmt;

/// Represents the static types in the Lowland language
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// String type
    String,
    
    /// Integer type
    Int,
    
    /// Boolean type
    Bool,
    
    /// Object type
    Object,
    
    /// List type with element type
    List(Box<Type>),
    
    /// Unknown or inferred type
    Unknown,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::String => write!(f, "string"),
            Type::Int => write!(f, "int"),
            Type::Bool => write!(f, "bool"),
            Type::Object => write!(f, "obj"),
            Type::List(elem_type) => write!(f, "list<{}>", elem_type),
            Type::Unknown => write!(f, "unknown"),
        }
    }
}

impl Type {
    /// Check if a type is compatible with another type
    pub fn is_compatible_with(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Unknown, _) | (_, Type::Unknown) => true,
            (Type::String, Type::String) => true,
            (Type::Int, Type::Int) => true,
            (Type::Bool, Type::Bool) => true,
            (Type::Object, Type::Object) => true,
            (Type::List(t1), Type::List(t2)) => t1.is_compatible_with(t2),
            _ => false,
        }
    }
} 