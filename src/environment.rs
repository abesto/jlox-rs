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
    stack: Vec<HashMap<String, Variable>>,
}

impl Environment {
    #[must_use]
    pub fn root() -> Self {
        Self {
            stack: vec![HashMap::new()],
        }
    }

    pub fn nested<F, T>(&mut self, f: F) -> T
    where
        F: FnOnce(&mut Self) -> T,
    {
        self.stack.push(HashMap::new());
        let ret = f(self);
        self.stack.pop();
        ret
    }

    pub fn define<S: ToString>(&mut self, name: S, value: Option<Value>) {
        self.stack
            .last_mut()
            .expect("Ran out of nested scopes")
            .insert(name.to_string(), value.into());
    }

    #[must_use]
    pub fn assign(&mut self, name: &Token, value: Value) -> bool {
        for scope in self.stack.iter_mut().rev() {
            if scope.contains_key(&name.lexeme) {
                scope.insert(name.lexeme.clone(), Some(value).into());
                return true;
            }
        }
        false
    }

    pub fn get(&self, name: &Token) -> Option<&Variable> {
        for scope in self.stack.iter().rev() {
            if let Some(v) = scope.get(&name.lexeme) {
                return Some(v);
            }
        }
        None
    }
}
