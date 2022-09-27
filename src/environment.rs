use std::{cell::RefCell, collections::HashMap, rc::Rc};

use crate::{interpreter::Value, resolver::Binding, token::Token};

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

impl std::fmt::Display for Variable {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Variable::Uninitialized => write!(f, "<uninitialized>"),
            Variable::Value(v) => write!(f, "{}", v.borrow()),
        }
    }
}
#[derive(PartialEq, Debug, Default)]
pub struct GlobalEnvironment {
    globals: HashMap<String, Rc<RefCell<Variable>>>,
}

#[derive(PartialEq, Debug, Default)]
pub struct LocalEnvironment {
    data: Vec<Rc<RefCell<Variable>>>,
    parent: Option<Rc<RefCell<LocalEnvironment>>>,
}

impl GlobalEnvironment {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn define<S: ToString>(&mut self, name: S, value: Option<Rc<RefCell<Value>>>) {
        self.globals
            .insert(name.to_string(), Rc::new(RefCell::new(value.into())));
    }

    #[must_use]
    pub fn assign(&mut self, name: &Token, value: Option<Rc<RefCell<Value>>>) -> bool {
        if !self.globals.contains_key(&name.lexeme) {
            return false;
        }

        self.globals
            .insert(name.lexeme.clone(), Rc::new(RefCell::new(value.into())));
        true
    }

    pub fn get(&self, name: &Token) -> Option<Rc<RefCell<Variable>>> {
        self.globals.get(&name.lexeme).map(Rc::clone)
    }
}

impl LocalEnvironment {
    pub fn nested<F, T>(parent: Option<Rc<RefCell<LocalEnvironment>>>, f: F) -> T
    where
        F: FnOnce(Rc<RefCell<Self>>) -> T,
    {
        let child = LocalEnvironment {
            data: Default::default(),
            parent,
        };
        f(Rc::new(RefCell::new(child)))
    }

    // Possible optimization: remember (in `Resolver`?) the number of variables per scope,
    // create `self.data` accordingly
    pub fn assign(&mut self, binding: &Binding, value: Option<Rc<RefCell<Value>>>) {
        if binding.scopes_up > 0 {
            return self
                .parent
                .as_mut()
                .expect("Ran out of assign scopes :(")
                .borrow_mut()
                .assign(&binding.less_one_scope(), value);
        }

        // There's room for micro-optimizations here, but w/e TBH
        while self.data.len() <= binding.index_in_scope {
            self.data
                .push(Rc::new(RefCell::new(Variable::Uninitialized)));
        }
        self.data[binding.index_in_scope] = Rc::new(RefCell::new(value.into()));
    }

    pub fn get(&self, binding: &Binding) -> Rc<RefCell<Variable>> {
        if binding.scopes_up > 0 {
            return self
                .parent
                .as_ref()
                .expect("Ran out of get scopes :(")
                .borrow()
                .get(&binding.less_one_scope());
        }
        Rc::clone(&self.data[binding.index_in_scope])
    }
}
