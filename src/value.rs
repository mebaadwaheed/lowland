use std::collections::HashMap;
use std::fmt;
use uuid::Uuid;
use crate::types::Type;
use std::cell::RefCell;
use std::rc::Rc;

use crate::error::Error;

/// Represents a runtime value in the Lowland language
#[derive(Clone, Debug, PartialEq)]
pub enum Value {
    String(String),
    Int(i64),
    Float(f64),
    Bool(bool),
    Object(ObjectRef),
    List(ListRef),
    StdFunctionLink(String),
    Null,
}

/// A reference to an object
#[derive(Clone, Debug)]
pub struct ObjectRef {
    pub id: Uuid,
    pub properties: Rc<RefCell<HashMap<String, Value>>>,
}

/// A reference to a list
#[derive(Clone, Debug)]
pub struct ListRef {
    pub id: Uuid,
    pub elements: Rc<RefCell<Vec<Value>>>,
    pub element_type: Rc<RefCell<Type>>,
}

impl PartialEq for ObjectRef {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl PartialEq for ListRef {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl Value {
    /// Get the type of this value
    pub fn get_type(&self) -> Type {
        match self {
            Value::String(_) => Type::String,
            Value::Int(_) => Type::Int,
            Value::Float(_) => Type::Float,
            Value::Bool(_) => Type::Bool,
            Value::Object(_) => Type::Object,
            Value::List(list_ref) => Type::List(Box::new(list_ref.element_type.borrow().clone())),
            Value::StdFunctionLink(_) => Type::Unknown,
            Value::Null => Type::Unknown,
        }
    }

    /// Convert this value to a string representation
    pub fn to_string_value(&self) -> Result<String, Error> {
        Ok(match self {
            Value::String(s) => s.clone(),
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Object(o) => format!("obj@{}", o.id),
            Value::List(l) => {
                let elements_borrow = l.elements.borrow();
                let mut content = String::new();
                for (i, e) in elements_borrow.iter().enumerate() {
                    content.push_str(&e.to_string_value()?);
                    if i < elements_borrow.len() - 1 {
                        content.push_str(", ");
                    }
                }
                format!("[{}]", content)
            }
            Value::StdFunctionLink(name) => format!("<std_function: {}>", name),
            Value::Null => "null".to_string(),
        })
    }

    /// Convert this value to a raw string representation for println!
    pub fn to_raw_display_string(&self) -> Result<String, Error> {
        Ok(match self {
            Value::String(s) => {
                let mut escaped_s = String::with_capacity(s.len());
                for c in s.chars() {
                    match c {
                        '\n' => escaped_s.push_str("\\n"),
                        '\t' => escaped_s.push_str("\\t"),
                        '\r' => escaped_s.push_str("\\r"),
                        '"'  => escaped_s.push_str("\\\""),
                        '\\' => escaped_s.push_str("\\\\"),
                        _ => escaped_s.push(c),
                    }
                }
                escaped_s
            }
            Value::Int(i) => i.to_string(),
            Value::Float(f) => f.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Object(o) => format!("obj@{}", o.id),
            Value::List(l) => {
                let elements_borrow = l.elements.borrow();
                let mut content = String::new();
                for (i, e) in elements_borrow.iter().enumerate() {
                    content.push_str(&e.to_raw_display_string()?);
                    if i < elements_borrow.len() - 1 {
                        content.push_str(", ");
                    }
                }
                format!("[{}]", content)
            }
            Value::StdFunctionLink(name) => format!("<std_function: {}>", name),
            Value::Null => "null".to_string(),
        })
    }
}

impl fmt::Display for Value {
    fn fmt(&self, formatter: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(formatter, "\\\"{}\\\"", s),
            Value::Int(i) => write!(formatter, "{}", i),
            Value::Float(f_val) => write!(formatter, "{}", f_val),
            Value::Bool(b) => write!(formatter, "{}", b),
            Value::Object(o) => write!(formatter, "obj@{}", o.id),
            Value::List(l) => {
                let elements: Vec<String> = l.elements.borrow().iter()
                    .map(|e| e.to_string())
                    .collect();
                write!(formatter, "[{}]", elements.join(", "))
            }
            Value::StdFunctionLink(name) => write!(formatter, "<std_function: {}>", name),
            Value::Null => write!(formatter, "null"),
        }
    }
}

impl ObjectRef {
    pub fn new() -> Self {
        ObjectRef {
            id: Uuid::new_v4(),
            properties: Rc::new(RefCell::new(HashMap::new())),
        }
    }
}

impl ListRef {
    pub fn new(element_type: Type) -> Self {
        ListRef {
            id: Uuid::new_v4(),
            elements: Rc::new(RefCell::new(Vec::new())),
            element_type: Rc::new(RefCell::new(element_type)),
        }
    }

    pub fn from_vec(elements_vec: Vec<Value>, element_type: Type) -> Self {
        ListRef {
            id: Uuid::new_v4(),
            elements: Rc::new(RefCell::new(elements_vec)),
            element_type: Rc::new(RefCell::new(element_type)),
        }
    }
} 