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

#[derive(PartialEq, Debug)]
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
    pub fn assign(
        &mut self,
        distance: Option<usize>,
        name: &Token,
        value: Rc<RefCell<Value>>,
    ) -> bool {
        match (distance, self.parent.as_ref()) {
            (None, Some(parent)) => parent.borrow_mut().assign(None, name, value),
            (None, None) | (Some(0), _) => {
                if self.data.contains_key(&name.lexeme) {
                    self.data.insert(
                        name.lexeme.clone(),
                        Rc::new(RefCell::new(Some(value).into())),
                    );
                    true
                } else {
                    false
                }
            }
            (Some(n), Some(parent)) => parent.borrow_mut().assign(Some(n - 1), name, value),
            _ => unreachable!(),
        }
    }

    pub fn get(&self, distance: Option<usize>, name: &Token) -> Option<Rc<RefCell<Variable>>> {
        match (distance, self.parent.as_ref()) {
            (None, Some(parent)) => parent.borrow().get(None, name),
            (None, None) | (Some(0), _) => self.data.get(&name.lexeme).map(Rc::clone),
            (Some(n), Some(parent)) => parent.borrow().get(Some(n - 1), name),
            _ => unreachable!(),
        }
    }
}

impl std::fmt::Display for Environment {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "ENV START")?;
        for (name, var) in self.data.iter() {
            write!(f, "{}={} ", name, var.borrow())?;
        }
        if let Some(parent) = &self.parent {
            write!(f, "{}", parent.borrow())?;
        }
        writeln!(f, "ENV END")?;
        Ok(())
    }
}

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Uninitialized => write!(f, "<uninitialized>"),
            Variable::Value(v) => write!(f, "{}", v.borrow()),
        }
    }
}
