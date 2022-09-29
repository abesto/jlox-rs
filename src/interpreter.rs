use std::{cell::RefCell, collections::HashMap, rc::Rc};

use derivative::Derivative;
use macros::ResolveErrorLocation;
use thiserror::Error;

use crate::{
    ast::{Expr, ExprVisitor, Function, Lambda, Literal, Stmt, StmtVisitor, Walkable},
    environment::{GlobalEnvironment, LocalEnvironment, Variable},
    resolver::{Binding, Bindings, CommandIndex},
    token::{Token, TokenValue},
    types::{Number, SourceLocation},
};

#[derive(Debug, PartialEq, Eq, Clone, Default)]
pub struct Class {
    name: String,
}

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<class '{}'>", self.name)
    }
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone, Default)]
pub enum Value {
    #[derivative(Default)]
    Nil,
    Boolean(bool),
    Number(Number),
    String(String),

    NativeFunction {
        name: String,
        arity: usize,
        #[derivative(
            Debug = "ignore",
            // Treat the implementation as always equal; we discriminate built-in functions by name
            PartialEq(compare_with = "always_equals"),
        )]
        #[allow(clippy::type_complexity)]
        fun: fn(&mut Interpreter, Vec<Rc<RefCell<Value>>>) -> Value,
    },

    Function {
        declaration: Function,
        closure: Locals,
    },

    Lambda {
        declaration: Lambda,
        closure: Locals,
    },

    Class(Rc<RefCell<Class>>),

    Instance {
        class: Rc<RefCell<Class>>,
        fields: HashMap<String, Rc<RefCell<Value>>>,
    },
}

fn always_equals<T>(_: &T, _: &T) -> bool {
    true
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Nil | Self::Boolean(false))
    }

    pub fn type_of(&self) -> String {
        match self {
            Self::Nil => "Nil".to_string(),
            Self::Boolean(_) => "Boolean".to_string(),
            Self::Number(_) => "Number".to_string(),
            Self::String(_) => "String".to_string(),
            Self::NativeFunction { .. } => "Function".to_string(),
            Self::Function { .. } => "Function".to_string(),
            Self::Lambda { .. } => "Function".to_string(),
            Self::Class(..) => "Class".to_string(),
            Self::Instance { class, .. } => format!("{}", class.borrow()),
        }
    }

    fn repr(&self) -> String {
        match self {
            Self::String(s) => format!("{:?}", s),
            v => format!("{}", v),
        }
    }

    fn arity(&self) -> usize {
        match self {
            Value::NativeFunction { arity, .. } => *arity,
            Value::Function { declaration: Function { params, .. }, .. }
            | Value::Lambda { declaration: Lambda { params, ..}, .. } => params.len(),
            Value::Class(_) => 0,
            _ => panic!("Internal error: tried to check arity of non-callable; this should've been caught sooner")
        }
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Rc<RefCell<Value>>>,
    ) -> Result<Rc<RefCell<Value>>> {
        match self {
            Value::NativeFunction { fun, .. } => Ok(Rc::new(RefCell::new(fun(interpreter, args)))),

            Value::Function {
                declaration: Function { params, body, .. },
                closure,
            }
            | Value::Lambda {
                declaration: Lambda { params, body, .. },
                closure,
            } => LocalEnvironment::nested(OptRc::clone(closure), |function_scope| {
                for (param, arg) in params.iter().zip(args) {
                    function_scope
                        .borrow_mut()
                        .assign(interpreter.binding(param).unwrap(), Some(arg))
                }
                interpreter
                    .execute_block(body, Some(function_scope))
                    .map(Option::unwrap_or_default)
            }),

            Value::Class(class) => Ok(Rc::new(RefCell::new(Value::Instance {
                class: Rc::clone(class),
                fields: Default::default(),
            }))),

            _ => panic!(
                "Internal error: tried to invoke non-callable; this should've been caught sooner"
            ),
        }
    }

    fn get(&self, name: &Token) -> Result<Rc<RefCell<Value>>> {
        match self {
            Value::Instance { fields, .. } => {
                if let Some(value) = fields.get(&name.lexeme) {
                    Ok(Rc::clone(value))
                } else {
                    Err(Error::UndefinedProperty {
                        object: self.clone(),
                        property: name.lexeme.clone(),
                        location: name.location,
                    })
                }
            }
            _ => Err(Error::PropertyOnNonObject {
                non_object: self.clone(),
                property: name.lexeme.clone(),
                location: name.location,
            }),
        }
    }

    fn set(&mut self, name: &Token, value: &Rc<RefCell<Value>>) {
        match self {
            Value::Instance { fields, .. } => {
                fields.insert(name.lexeme.clone(), Rc::clone(value));
            },
            _ => unreachable!("Internal error: tried to call set() on non-Instance; this should've been caught sooner")
        }
    }
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => f.write_str("nil"),
            Self::Boolean(b) => b.fmt(f),
            Self::Number(n) => n.fmt(f),
            Self::String(s) => s.fmt(f),
            Self::NativeFunction { name, .. } => write!(f, "<function {}>", name),
            Self::Function { declaration, .. } => {
                write!(f, "<function {}>", declaration.name.lexeme)
            }
            Self::Lambda { .. } => write!(f, "<anonymous function>"),
            Self::Class(class) => class.borrow().fmt(f),
            Self::Instance { class, .. } => write!(f, "<{} object>", class.borrow().name),
        }
    }
}

#[derive(Error, Debug, ResolveErrorLocation)]
pub enum Error {
    #[error(
        "`{operator}` expected one of: [{}], found {} of type {} at {location}",
        .expected.join(", "),
        .actual.repr(), .actual.type_of())
    ]
    InvalidOperand {
        operator: TokenValue,
        expected: Vec<String>,
        actual: Value,
        location: SourceLocation,
    },

    #[error("Division by zero at {location}")]
    DivisionByZero { location: SourceLocation },

    #[error("Undefined variable {name} at {location}")]
    UndefinedVariable {
        name: String,
        location: SourceLocation,
    },

    #[error("Uninitialized variable {name} at {location}")]
    UninitializedVariable {
        name: String,
        location: SourceLocation,
    },

    #[error("`break` executed without enclosing loop o.0")]
    Break { location: SourceLocation },

    #[error("Can only call functions and classes, tried to call: `{what}` of type `{}` at {location}", .what.type_of())]
    NotCallable {
        what: Value,
        location: SourceLocation,
    },

    #[error("Expected {expected} arguments but got {actual} at {location}")]
    WrongArity {
        expected: usize,
        actual: usize,
        location: SourceLocation,
    },

    #[error("`return` outside function at {location}")]
    Return {
        location: SourceLocation,
        value: Rc<RefCell<Value>>,
    },

    #[error("Tried to access property `{property}` on non-object `{non_object}` of type `{}` at {location}", .non_object.type_of())]
    PropertyOnNonObject {
        non_object: Value,
        property: String,
        location: SourceLocation,
    },

    #[error("Undefined property `{property}` on `{object}` at {location}")]
    UndefinedProperty {
        object: Value,
        property: String,
        location: SourceLocation,
    },
}

pub type Result<V = Option<Rc<RefCell<Value>>>, E = Error> = std::result::Result<V, E>;
type Globals = Rc<RefCell<GlobalEnvironment>>;
type Locals = Option<Rc<RefCell<LocalEnvironment>>>;

// Convenience syntax: `OptRc::clone`, to remain explicit about how cloning a `Locals`
// establishes a new `Rc` reference
struct OptRc {}
impl OptRc {
    fn clone(x: &Locals) -> Locals {
        x.as_ref().cloned()
    }
}

#[derive(Default)]
pub struct Interpreter {
    pub command: CommandIndex,
    bindings: Bindings,
    globals: Globals,
}

impl Interpreter {
    #[must_use]
    pub fn new(globals: Globals) -> Self {
        Self {
            globals,
            ..Default::default()
        }
    }

    pub fn update_bindings(&mut self, bindings: Bindings) {
        self.bindings.extend(bindings.into_iter());
    }

    pub fn binding(&self, name: &Token) -> Option<&Binding> {
        self.bindings.get(&name.location)
    }

    fn evaluate(&mut self, expr: &Expr, locals: Locals) -> Result<Rc<RefCell<Value>>> {
        expr.walk(self, locals)
    }

    pub fn interpret(&mut self, program: &[Stmt]) -> Result {
        let mut ret = None;
        for stmt in program {
            ret = self.execute(stmt, None)?;
        }
        Ok(ret)
    }

    fn execute(&mut self, stmt: &Stmt, locals: Locals) -> Result {
        stmt.walk(self, locals)
    }

    fn err_invalid_operand<V, S: ToString>(
        &self,
        op: &Token,
        expected: &[S],
        actual: Value,
    ) -> Result<V> {
        Err(Error::InvalidOperand {
            operator: op.value.clone(),
            expected: Vec::from_iter(expected.iter().map(ToString::to_string)),
            actual,
            location: op.location,
        })
    }

    fn execute_block(&mut self, statements: &[Stmt], locals: Locals) -> Result {
        statements
            .iter()
            .map(|stmt| self.execute(stmt, OptRc::clone(&locals)))
            .try_fold(None, |_, x| x)
    }
}

impl ExprVisitor<Result<Rc<RefCell<Value>>>, Locals> for &mut Interpreter {
    fn visit_literal(self, x: &crate::ast::Literal, _: Locals) -> Result<Rc<RefCell<Value>>> {
        Ok(Rc::new(RefCell::new(match x {
            Literal::Nil => Value::Nil,
            Literal::False => Value::Boolean(false),
            Literal::True => Value::Boolean(true),
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
        })))
    }

    fn visit_unary(self, x: &crate::ast::Unary, locals: Locals) -> Result<Rc<RefCell<Value>>> {
        let right = self.evaluate(&x.right, locals)?;

        match x.operator.value {
            TokenValue::Minus => match &*right.borrow() {
                Value::Number(n) => Ok(Rc::new(RefCell::new(Value::Number(-n)))),
                v => self.err_invalid_operand(&x.operator, &["Number"], v.clone()),
            },
            TokenValue::Bang => Ok(Rc::new(RefCell::new(Value::Boolean(
                !right.borrow().is_truthy(),
            )))),
            _ => unreachable!(),
        }
    }

    fn visit_binary(self, x: &crate::ast::Binary, locals: Locals) -> Result<Rc<RefCell<Value>>> {
        let left_rc = self.evaluate(&x.left, OptRc::clone(&locals))?;
        let left = &*left_rc.borrow();
        let right_rc = &*self.evaluate(&x.right, OptRc::clone(&locals))?;
        let right = &*right_rc.borrow();
        let op = &x.operator;

        match &op.value {
            TokenValue::Minus => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    Ok(Rc::new(RefCell::new(Value::Number(l - r))))
                }
                (Value::Number(_), v) => self.err_invalid_operand(op, &["Number"], v.clone()),
                (v, _) => self.err_invalid_operand(op, &["Number"], v.clone()),
            },

            TokenValue::Slash => match (left, right) {
                (Value::Number(_), Value::Number(r)) if *r == 0.0 => Err(Error::DivisionByZero {
                    location: op.location,
                }),
                (Value::Number(l), Value::Number(r)) => {
                    Ok(Rc::new(RefCell::new(Value::Number(l / r))))
                }
                (Value::Number(_), v) => self.err_invalid_operand(op, &["Number"], v.clone()),
                (v, _) => self.err_invalid_operand(op, &["Number"], v.clone()),
            },

            TokenValue::Star => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    Ok(Rc::new(RefCell::new(Value::Number(l * r))))
                }
                (Value::Number(_), v) => self.err_invalid_operand(op, &["Number"], v.clone()),
                (v, _) => self.err_invalid_operand(op, &["Number"], v.clone()),
            },

            TokenValue::Plus => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    Ok(Rc::new(RefCell::new(Value::Number(l + r))))
                }
                (Value::String(_), _) | (_, Value::String(_)) => Ok(Rc::new(RefCell::new(
                    Value::String(format!("{}{}", left, right)),
                ))),
                (Value::Number(_), _) => {
                    self.err_invalid_operand(op, &["Number", "String"], right.clone())
                }
                _ => self.err_invalid_operand(op, &["Number", "String"], left.clone()),
            },

            TokenValue::Greater => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    Ok(Rc::new(RefCell::new(Value::Boolean(l > r))))
                }
                (Value::Number(_), v) => self.err_invalid_operand(op, &["Number"], v.clone()),
                (v, _) => self.err_invalid_operand(op, &["Number"], v.clone()),
            },

            TokenValue::Less => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    Ok(Rc::new(RefCell::new(Value::Boolean(l < r))))
                }
                (Value::Number(_), v) => self.err_invalid_operand(op, &["Number"], v.clone()),
                (v, _) => self.err_invalid_operand(op, &["Number"], v.clone()),
            },

            TokenValue::GreaterEqual => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    Ok(Rc::new(RefCell::new(Value::Boolean(l >= r))))
                }
                (Value::Number(_), v) => self.err_invalid_operand(op, &["Number"], v.clone()),
                (v, _) => self.err_invalid_operand(op, &["Number"], v.clone()),
            },

            TokenValue::LessEqual => match (left, right) {
                (Value::Number(l), Value::Number(r)) => {
                    Ok(Rc::new(RefCell::new(Value::Boolean(l <= r))))
                }
                (Value::Number(_), v) => self.err_invalid_operand(op, &["Number"], v.clone()),
                (v, _) => self.err_invalid_operand(op, &["Number"], v.clone()),
            },

            TokenValue::EqualEqual => Ok(Rc::new(RefCell::new(Value::Boolean(left == right)))),
            TokenValue::BangEqual => Ok(Rc::new(RefCell::new(Value::Boolean(left != right)))),

            x => unreachable!("Unrecognized binary operator `{}` at {}", x, op.location),
        }
    }

    fn visit_ternary(self, x: &crate::ast::Ternary, locals: Locals) -> Result<Rc<RefCell<Value>>> {
        let cond = self.evaluate(&x.left, OptRc::clone(&locals))?;
        if cond.borrow().is_truthy() {
            self.evaluate(&x.mid, OptRc::clone(&locals))
        } else {
            self.evaluate(&x.right, OptRc::clone(&locals))
        }
    }

    fn visit_grouping(
        self,
        x: &crate::ast::Grouping,
        locals: Locals,
    ) -> Result<Rc<RefCell<Value>>> {
        self.evaluate(&x.expr, locals)
    }

    fn visit_variable(
        self,
        x: &crate::ast::Variable,
        locals: Locals,
    ) -> Result<Rc<RefCell<Value>>> {
        let var = match self.binding(&x.name) {
            Some(binding) => locals.unwrap().borrow().get(binding),
            None => match self.globals.borrow().get(&x.name) {
                Some(v) => v,
                None => {
                    return Err(Error::UndefinedVariable {
                        name: x.name.lexeme.clone(),
                        location: x.name.location,
                    })
                }
            },
        };

        let var = var.borrow();
        match &*var {
            Variable::Value(v) => Ok(Rc::clone(v)),
            Variable::Uninitialized => Err(Error::UninitializedVariable {
                name: x.name.lexeme.clone(),
                location: x.name.location,
            }),
        }
    }

    fn visit_assign(self, x: &crate::ast::Assign, locals: Locals) -> Result<Rc<RefCell<Value>>> {
        let value = self.evaluate(&x.value, OptRc::clone(&locals))?;
        match self.binding(&x.name) {
            Some(binding) => {
                locals
                    .unwrap()
                    .borrow_mut()
                    .assign(binding, Some(Rc::clone(&value)));
            }
            None => {
                if !self
                    .globals
                    .borrow_mut()
                    .assign(&x.name, Some(Rc::clone(&value)))
                {
                    return Err(Error::UndefinedVariable {
                        name: x.name.lexeme.clone(),
                        location: x.name.location,
                    });
                }
            }
        };
        Ok(value)
    }

    fn visit_logical(self, x: &crate::ast::Logical, locals: Locals) -> Result<Rc<RefCell<Value>>> {
        let left = self.evaluate(&x.left, OptRc::clone(&locals))?;
        if x.operator.value == TokenValue::Or {
            if left.borrow().is_truthy() {
                return Ok(left);
            }
        } else if !left.borrow().is_truthy() {
            return Ok(left);
        }
        self.evaluate(&x.right, locals)
    }

    fn visit_call(self, call: &crate::ast::Call, locals: Locals) -> Result<Rc<RefCell<Value>>> {
        let value_rc = self.evaluate(&call.callee, OptRc::clone(&locals))?;
        let value = &*value_rc.borrow();
        let callee = match value {
            f @ Value::NativeFunction { .. }
            | f @ Value::Lambda { .. }
            | f @ Value::Function { .. }
            | f @ Value::Class(_) => Ok(f),
            what => Err(Error::NotCallable {
                what: what.clone(),
                location: call.closing_paren.location,
            }),
        }?;

        let arguments = call
            .arguments
            .iter()
            .map(|arg| self.evaluate(arg, OptRc::clone(&locals)))
            .collect::<Result<Vec<_>>>()?;

        if arguments.len() != callee.arity() {
            return Err(Error::WrongArity {
                expected: callee.arity(),
                actual: arguments.len(),
                location: call.closing_paren.location,
            });
        }

        match callee.call(self, arguments) {
            Err(Error::Return { value, .. }) => Ok(value),
            x => x,
        }
    }

    fn visit_lambda(self, x: &crate::ast::Lambda, locals: Locals) -> Result<Rc<RefCell<Value>>> {
        Ok(Rc::new(RefCell::new(Value::Lambda {
            declaration: x.clone(),
            closure: locals,
        })))
    }

    fn visit_get(self, expr: &crate::ast::Get, state: Locals) -> Result<Rc<RefCell<Value>>> {
        self.evaluate(&expr.object, state)?.borrow().get(&expr.name)
    }

    fn visit_set(self, expr: &crate::ast::Set, state: Locals) -> Result<Rc<RefCell<Value>>> {
        let object_rc = self.evaluate(&expr.object, OptRc::clone(&state))?;
        let mut object = object_rc.borrow_mut();

        if !matches!(&*object, Value::Instance { .. }) {
            return Err(Error::PropertyOnNonObject {
                non_object: object.clone(),
                property: expr.name.lexeme.clone(),
                location: expr.name.location,
            });
        }

        let value = self.evaluate(&expr.value, state)?;
        object.set(&expr.name, &value);
        Ok(value)
    }
}

impl StmtVisitor<Result<Option<Rc<RefCell<Value>>>>, Locals> for &mut Interpreter {
    fn visit_block(
        self,
        x: &crate::ast::Block,
        locals: Locals,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        LocalEnvironment::nested(OptRc::clone(&locals), |block_env| {
            self.execute_block(&x.statements, Some(block_env))
        })
    }

    fn visit_expression(
        self,
        x: &crate::ast::Expression,
        locals: Locals,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        self.evaluate(&x.expr, locals).map(Some)
    }

    fn visit_if(self, x: &crate::ast::If, locals: Locals) -> Result<Option<Rc<RefCell<Value>>>> {
        if self
            .evaluate(&x.condition, OptRc::clone(&locals))?
            .borrow()
            .is_truthy()
        {
            self.execute(&x.then_branch, locals)
        } else if let Some(branch) = &x.else_branch {
            self.execute(branch, locals)
        } else {
            Ok(None)
        }
    }

    fn visit_print(
        self,
        x: &crate::ast::Print,
        locals: Locals,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        println!("{}", self.evaluate(&x.expr, locals)?.borrow());
        Ok(None)
    }

    fn visit_var(self, x: &crate::ast::Var, locals: Locals) -> Result<Option<Rc<RefCell<Value>>>> {
        let value = match &x.initializer {
            Some(expr) => Some(self.evaluate(expr, OptRc::clone(&locals))?),
            None => None,
        };

        match self.binding(&x.name) {
            None => self.globals.borrow_mut().define(&x.name.lexeme, value),
            Some(binding) => locals.unwrap().borrow_mut().assign(binding, value),
        }
        Ok(None)
    }

    fn visit_while(
        self,
        x: &crate::ast::While,
        locals: Locals,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        while self
            .evaluate(&x.condition, OptRc::clone(&locals))?
            .borrow()
            .is_truthy()
        {
            match self.execute(&x.statement, OptRc::clone(&locals)) {
                Err(Error::Break { .. }) => return Ok(None),
                x => x?,
            };
        }
        Ok(None)
    }

    fn visit_break(
        self,
        x: &crate::ast::Break,
        _locals: Locals,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        Err(Error::Break {
            location: x.token.location,
        })
    }

    fn visit_function(self, x: &Function, locals: Locals) -> Result<Option<Rc<RefCell<Value>>>> {
        let fun = Value::Function {
            declaration: x.clone(),
            closure: OptRc::clone(&locals),
        };

        let value = Some(Rc::new(RefCell::new(fun)));
        match self.binding(&x.name) {
            None => self
                .globals
                .borrow_mut()
                .define(x.name.lexeme.clone(), value),
            Some(binding) => locals.unwrap().borrow_mut().assign(binding, value),
        };
        Ok(None)
    }

    fn visit_return(
        self,
        ret: &crate::ast::Return,
        locals: Locals,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        let value = if let Some(value_expr) = &ret.value {
            self.evaluate(value_expr, locals)?
        } else {
            Rc::new(RefCell::new(Value::Nil))
        };

        Err(Error::Return {
            location: ret.keyword.location,
            value,
        })
    }

    fn visit_class(
        self,
        stmt: &crate::ast::Class,
        state: Locals,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        let binding = self.binding(&stmt.name);
        if let Some(binding) = binding {
            state.as_ref().unwrap().borrow_mut().assign(binding, None);
        } else {
            self.globals.borrow_mut().define(&stmt.name, None);
        }

        let class = Rc::new(RefCell::new(Value::Class(Rc::new(RefCell::new(Class {
            name: stmt.name.lexeme.clone(),
        })))));

        if let Some(binding) = binding {
            state.unwrap().borrow_mut().assign(binding, Some(class));
        } else {
            let _ = self.globals.borrow_mut().assign(&stmt.name, Some(class));
        }

        Ok(None)
    }
}
