use std::fmt;

/// Represents the static types in the Lowland language
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Type {
    /// String type
    String,
    
    /// Integer type
    Int,
    
    /// Float type
    Float,
    
    /// Boolean type
    Bool,
    
    /// Object type
    Object,
    
    /// List type with element type
    List(Box<Type>),
    
    /// Struct type with a name
    Struct(String),
    
    /// Unknown or inferred type
    Unknown,
}

impl fmt::Display for Type {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Type::String => write!(f, "string"),
            Type::Int => write!(f, "int"),
            Type::Float => write!(f, "float"),
            Type::Bool => write!(f, "bool"),
            Type::Object => write!(f, "obj"),
            Type::List(elem_type) => write!(f, "list<{}>", elem_type),
            Type::Struct(name) => write!(f, "struct<{}>", name),
            Type::Unknown => write!(f, "unknown"),
        }
    }
}

impl Type {
    /// Check if a type is compatible with another type
    /// This means: can a value of type 'other' be used where a value of type 'self' is expected?
    pub fn is_compatible_with(&self, other: &Type) -> bool {
        match (self, other) {
            (Type::Unknown, _) | (_, Type::Unknown) => true, // Unknown is compatible with anything
            (Type::String, Type::String) => true,
            (Type::Int, Type::Int) => true,
            (Type::Float, Type::Float) => true,
            (Type::Float, Type::Int) => true, // An Int can be used where a Float is expected
            (Type::Bool, Type::Bool) => true,
            (Type::Object, Type::Object) => true,
            (Type::List(t1), Type::List(t2)) => t1.is_compatible_with(t2),
            (Type::Struct(s1), Type::Struct(s2)) => s1 == s2,
            _ => false, // No other combinations are compatible
        }
    }
}
