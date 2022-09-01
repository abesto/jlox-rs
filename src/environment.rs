use std::collections::HashMap;

use crate::{interpreter::Value, token::Token};

pub enum Variable {
    Uninitialized,
    Value(Value),
}

impl From<Option<Value>> for Variable {
    fn from(x: Option<Value>) -> Self {
        match x {
            Some(v) => Self::Value(v),
            None => Self::Uninitialized,
        }
    }
}

pub struct Environment {
    enclosing: Option<Box<Environment>>,
    values: HashMap<String, Variable>,
}

impl Environment {
    #[must_use]
    pub fn root() -> Self {
        Self {
            enclosing: None,
            values: HashMap::new(),
        }
    }

    #[must_use]
    pub fn nested(self) -> Self {
        Self {
            enclosing: Some(Box::new(self)),
            values: HashMap::new(),
        }
    }

    #[must_use]
    pub fn unwind(mut self) -> Option<Self> {
        std::mem::take(&mut self.enclosing).map(|e| *e)
    }

    pub fn define<S: ToString>(&mut self, name: S, value: Option<Value>) {
        self.values.insert(name.to_string(), value.into());
    }

    #[must_use]
    pub fn assign(&mut self, name: &Token, value: Value) -> bool {
        if !self.values.contains_key(&name.lexeme) {
            return self
                .enclosing
                .as_mut()
                .map(|enclosing| enclosing.assign(name, value))
                .unwrap_or(false);
        }
        self.values.insert(name.lexeme.clone(), Some(value).into());
        true
    }

    pub fn get(&self, name: &Token) -> Option<&Variable> {
        self.values.get(&name.lexeme).or_else(|| {
            self.enclosing
                .as_ref()
                .and_then(|enclosing| enclosing.get(name))
        })
    }
}
