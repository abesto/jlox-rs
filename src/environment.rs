use std::collections::HashMap;

use crate::{interpreter::Value, token::Token};

pub struct Environment {
    values: HashMap<String, Value>,
}

impl Environment {
    #[must_use]
    pub fn new() -> Self {
        Self {
            values: HashMap::new(),
        }
    }

    pub fn define<S: ToString>(&mut self, name: S, value: Value) {
        self.values.insert(name.to_string(), value);
    }

    pub fn get(&self, name: &Token) -> Option<&Value> {
        self.values.get(&name.lexeme)
    }
}
