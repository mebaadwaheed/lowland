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
    Bool(bool),
    Object(ObjectRef),
    List(ListRef),
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
            Value::Bool(_) => Type::Bool,
            Value::Object(_) => Type::Object,
            Value::List(list_ref) => Type::List(Box::new(list_ref.element_type.borrow().clone())),
            Value::Null => Type::Unknown,
        }
    }

    /// Convert this value to a string representation
    pub fn to_string_value(&self) -> Result<String, Error> {
        Ok(match self {
            Value::String(s) => s.clone(),
            Value::Int(i) => i.to_string(),
            Value::Bool(b) => b.to_string(),
            Value::Object(o) => format!("obj@{}", o.id),
            Value::List(l) => {
                let elements: Vec<String> = l.elements.borrow().iter()
                    .map(|e| e.to_string_value())
                    .collect::<Result<Vec<String>, Error>>()?;
                format!("[{}]", elements.join(", "))
            }
            Value::Null => "null".to_string(),
        })
    }
}

impl fmt::Display for Value {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Value::String(s) => write!(f, "\"{}\"", s),
            Value::Int(i) => write!(f, "{}", i),
            Value::Bool(b) => write!(f, "{}", b),
            Value::Object(o) => write!(f, "obj@{}", o.id),
            Value::List(l) => {
                let elements: Vec<String> = l.elements.borrow().iter()
                    .map(|e| e.to_string())
                    .collect();
                write!(f, "[{}]", elements.join(", "))
            }
            Value::Null => write!(f, "null"),
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