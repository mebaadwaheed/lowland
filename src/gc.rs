use std::collections::{HashMap, HashSet};
use uuid::Uuid;

use crate::value::{ListRef, ObjectRef, Value};

/// A simple mark-and-sweep garbage collector for Lowland
pub struct GarbageCollector {
    /// Objects being tracked
    objects: HashMap<Uuid, ObjectRef>,
    
    /// Lists being tracked
    lists: HashMap<Uuid, ListRef>,
}

impl GarbageCollector {
    pub fn new() -> Self {
        GarbageCollector {
            objects: HashMap::new(),
            lists: HashMap::new(),
        }
    }
    
    /// Register an object with the garbage collector
    pub fn register_object(&mut self, obj: ObjectRef) {
        self.objects.insert(obj.id, obj);
    }
    
    /// Register a list with the garbage collector
    pub fn register_list(&mut self, list: ListRef) {
        self.lists.insert(list.id, list);
    }
    
    /// Perform garbage collection
    /// 
    /// `roots` are the values currently in scope
    pub fn collect(&mut self, roots: &[Value]) {
        // 1. Mark phase
        let mut marked = HashSet::new();
        for root in roots {
            self.mark(root, &mut marked);
        }
        
        // 2. Sweep phase
        
        // Remove unreachable objects
        self.objects.retain(|id, _| marked.contains(id));
        
        // Remove unreachable lists
        self.lists.retain(|id, _| marked.contains(id));
    }
    
    /// Mark an object as reachable
    fn mark(&self, value: &Value, marked: &mut HashSet<Uuid>) {
        match value {
            Value::Object(obj_ref) => {
                if marked.insert(obj_ref.id) {
                    // If we haven't seen this object before, mark its properties
                    for (_, prop_val) in obj_ref.properties.borrow().iter() {
                        self.mark(prop_val, marked);
                    }
                }
            }
            Value::List(list_ref) => {
                if marked.insert(list_ref.id) {
                    // If we haven't seen this list before, mark its elements
                    for elem in list_ref.elements.borrow().iter() {
                        self.mark(elem, marked);
                    }
                }
            }
            // Other value types don't need to be marked
            _ => {}
        }
    }
} 