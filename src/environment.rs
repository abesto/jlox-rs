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

    #[must_use]
    pub fn assign(&mut self, name: &Token, value: Value) -> bool {
        if !self.values.contains_key(&name.lexeme) {
            return false;
        }
        self.values.insert(name.lexeme.clone(), value);
        true
    }

    pub fn get(&self, name: &Token) -> Option<&Value> {
        self.values.get(&name.lexeme)
    }
}
