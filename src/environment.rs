use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{interpreter::Value, token::Token};

#[derive(Debug, PartialEq)]
pub enum Variable {
    Uninitialized,
    Value(Rc<RefCell<Value>>),
}

impl From<Option<Rc<RefCell<Value>>>> for Variable {
    fn from(x: Option<Rc<RefCell<Value>>>) -> Self {
        match x {
            Some(v) => Self::Value(v),
            None => Self::Uninitialized,
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct Environment {
    data: HashMap<String, Rc<RefCell<Variable>>>,
    parent: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    #[must_use]
    pub fn root() -> Self {
        Self {
            data: HashMap::new(),
            parent: None,
        }
    }

    pub fn nested<F, T>(parent: &Rc<RefCell<Environment>>, f: F) -> T
    where
        F: FnOnce(Rc<RefCell<Self>>) -> T,
    {
        let child = Self {
            data: HashMap::new(),
            parent: Some(Rc::clone(parent)),
        };
        f(Rc::new(RefCell::new(child)))
    }

    pub fn define<S: ToString>(&mut self, name: S, value: Option<Rc<RefCell<Value>>>) {
        self.data
            .insert(name.to_string(), Rc::new(RefCell::new(value.into())));
    }

    #[must_use]
    pub fn assign(&mut self, name: &Token, value: Rc<RefCell<Value>>) -> bool {
        if self.data.contains_key(&name.lexeme) {
            self.data.insert(
                name.lexeme.clone(),
                Rc::new(RefCell::new(Some(value).into())),
            );
            return true;
        }
        if let Some(parent) = &self.parent {
            parent.borrow_mut().assign(name, value)
        } else {
            false
        }
    }

    pub fn get(&self, name: &Token) -> Option<Rc<RefCell<Variable>>> {
        if let Some(var) = self.data.get(&name.lexeme) {
            return Some(Rc::clone(var));
        }
        if let Some(parent) = &self.parent {
            return parent.borrow().get(name);
        }
        None
    }
}
