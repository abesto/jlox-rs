use std::{cell::RefCell, collections::HashMap, rc::Rc};

use derivative::Derivative;
use macros::ResolveErrorLocation;
use thiserror::Error;

use crate::{
    ast::{Expr, ExprVisitor, Literal, Stmt, StmtVisitor, Walkable},
    environment::{GlobalEnvironment, LocalEnvironment, Variable},
    resolver::{Binding, Bindings, CommandIndex},
    token::{Token, TokenValue},
    types::{Number, SourceLocation},
};

trait Callable {
    fn arity(&self) -> usize;
    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Rc<RefCell<Value>>>,
        scope: Locals,
    ) -> Result<Rc<RefCell<Value>>>;
}

fn call_common(
    interpreter: &mut Interpreter,
    args: Vec<Rc<RefCell<Value>>>,
    scope: Option<Rc<RefCell<LocalEnvironment>>>,
    params: &[Token],
    body: &[Stmt],
) -> Result<Rc<RefCell<Value>>, Error> {
    LocalEnvironment::nested(OptRc::clone(&scope), |function_scope| {
        for (param, arg) in params.iter().zip(args) {
            function_scope
                .borrow_mut()
                .assign(interpreter.binding(param).unwrap(), Some(arg))
        }
        interpreter
            .execute_block(body, Some(function_scope))
            .map(Option::unwrap_or_default)
    })
}

impl Callable for crate::ast::Function {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Rc<RefCell<Value>>>,
        scope: Locals,
    ) -> Result<Rc<RefCell<Value>>> {
        call_common(interpreter, args, scope, &self.params, &self.body)
    }
}

impl Callable for crate::ast::Lambda {
    fn arity(&self) -> usize {
        self.params.len()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Rc<RefCell<Value>>>,
        scope: Locals,
    ) -> Result<Rc<RefCell<Value>>> {
        call_common(interpreter, args, scope, &self.params, &self.body)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Class {
    name: String,
    superclass: Option<Rc<RefCell<Class>>>,
    methods: HashMap<String, Rc<RefCell<Function>>>,
    class_methods: HashMap<String, Rc<RefCell<Value>>>,
    getters: HashMap<String, Rc<RefCell<Function>>>,
    left_brace: Token,
}

impl std::fmt::Display for Class {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "<class '{}'>", self.name)
    }
}

impl Class {
    fn find_method(&self, name: &str) -> Option<Rc<RefCell<Function>>> {
        self.methods.get(name).map(Rc::clone).or_else(|| {
            self.superclass
                .as_ref()
                .and_then(|superclass| superclass.borrow().find_method(name))
        })
    }

    fn find_class_method(&self, name: &str) -> Option<Rc<RefCell<Value>>> {
        self.class_methods.get(name).map(Rc::clone).or_else(|| {
            self.superclass
                .as_ref()
                .and_then(|superclass| superclass.borrow().find_class_method(name))
        })
    }

    fn find_getter(&self, name: &str) -> Option<Rc<RefCell<Function>>> {
        self.getters.get(name).map(Rc::clone).or_else(|| {
            self.superclass
                .as_ref()
                .and_then(|superclass| superclass.borrow().find_getter(name))
        })
    }
}

impl Callable for Rc<RefCell<Class>> {
    fn arity(&self) -> usize {
        if let Some(init) = self.borrow().find_method("init") {
            init.borrow().arity()
        } else {
            0
        }
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Rc<RefCell<Value>>>,
        scope: Locals,
    ) -> Result<Rc<RefCell<Value>>> {
        let instance = Instance {
            class: Rc::clone(self),
            fields: Default::default(),
        };
        let instance = Rc::new(RefCell::new(Value::Instance(instance)));
        if let Some(init) = self.borrow().find_method("init") {
            interpreter
                .bind_this(&init.borrow(), Rc::clone(&instance))
                .call(interpreter, args, scope)?;
        }
        Ok(instance)
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Instance {
    class: Rc<RefCell<Class>>,
    fields: HashMap<String, Rc<RefCell<Value>>>,
}

impl Instance {
    fn set(&mut self, name: &Token, value: &Rc<RefCell<Value>>) {
        self.fields.insert(name.lexeme.clone(), Rc::clone(value));
    }
}

#[derive(Derivative)]
#[derivative(Debug, PartialEq, Clone)]
pub struct NativeFunction {
    pub name: String,
    pub arity: usize,
    #[derivative(
            Debug = "ignore",
            // Treat the implementation as always equal; we discriminate built-in functions by name
            PartialEq(compare_with = "always_equals"),
        )]
    #[allow(clippy::type_complexity)]
    pub fun: fn(&mut Interpreter, Vec<Rc<RefCell<Value>>>) -> Value,
}

impl Callable for NativeFunction {
    fn arity(&self) -> usize {
        self.arity
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Rc<RefCell<Value>>>,
        _scope: Locals,
    ) -> Result<Rc<RefCell<Value>>> {
        Ok(Rc::new(RefCell::new((self.fun)(interpreter, args))))
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Function {
    declaration: crate::ast::Function,
    closure: Locals,
    force_return: Option<Rc<RefCell<Value>>>, // Return value from constructor calls
}

impl Callable for Function {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Rc<RefCell<Value>>>,
        _scope: Locals,
    ) -> Result<Rc<RefCell<Value>>> {
        self.declaration
            .call(interpreter, args, OptRc::clone(&self.closure))
            .map_err(|e| match (&self.force_return, e) {
                (Some(value), Error::Return { location, .. }) => Error::Return {
                    location,
                    value: Rc::clone(value),
                },
                (_, e) => e,
            })
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Lambda {
    declaration: crate::ast::Lambda,
    closure: Locals,
}

impl Callable for Lambda {
    fn arity(&self) -> usize {
        self.declaration.params.len()
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        args: Vec<Rc<RefCell<Value>>>,
        _scope: Locals,
    ) -> Result<Rc<RefCell<Value>>> {
        self.declaration
            .call(interpreter, args, OptRc::clone(&self.closure))
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

    NativeFunction(NativeFunction),
    Function(Function),
    Lambda(Lambda),

    Class(Rc<RefCell<Class>>),
    Instance(Instance),
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
            Self::Instance(Instance { class, .. }) => format!("{}", class.borrow()),
        }
    }

    fn repr(&self) -> String {
        match self {
            Self::String(s) => format!("{:?}", s),
            v => format!("{}", v),
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
            Self::NativeFunction(NativeFunction { name, .. }) => write!(f, "<function {}>", name),
            Self::Function(Function { declaration, .. }) => {
                write!(f, "<function {}>", declaration.name.lexeme)
            }
            Self::Lambda { .. } => write!(f, "<anonymous function>"),
            Self::Class(class) => class.borrow().fmt(f),
            Self::Instance(Instance { class, .. }) => write!(f, "<{} object>", class.borrow().name),
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

    #[error("Class `{class}` has superclass `{superclass}` of type `{}` which is not a class at {location}", .superclass.type_of())]
    SuperclassNotClass {
        class: String,
        superclass: Value,
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
    fn clone<T>(x: &Option<Rc<RefCell<T>>>) -> Option<Rc<RefCell<T>>> {
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

    fn bind_this(&self, method: &Function, instance: Rc<RefCell<Value>>) -> Function {
        if let Value::Instance(Instance { class, .. }) = &*instance.borrow() {
            LocalEnvironment::nested(OptRc::clone(&method.closure), |environment| {
                environment.borrow_mut().assign(
                    self.binding(&class.borrow().left_brace).unwrap(),
                    Some(Rc::clone(&instance)),
                );
                Function {
                    declaration: method.declaration.clone(),
                    closure: Some(environment),
                    force_return: if method.declaration.name.lexeme == "init" {
                        Some(Rc::clone(&instance))
                    } else {
                        None
                    },
                }
            })
        } else {
            unreachable!()
        }
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
        let callee_rc = self.evaluate(&call.callee, OptRc::clone(&locals))?;
        let callee = &*callee_rc.borrow();

        let callable: Box<&dyn Callable> = match callee {
            Value::NativeFunction(f) => Box::new(f),
            Value::Function(f) => Box::new(f),
            Value::Lambda(f) => Box::new(f),
            Value::Class(c) => Box::new(c),
            _ => {
                return Err(Error::NotCallable {
                    what: callee.clone(),
                    location: call.closing_paren.location,
                });
            }
        };

        let arguments = call
            .arguments
            .iter()
            .map(|arg| self.evaluate(arg, OptRc::clone(&locals)))
            .collect::<Result<Vec<_>>>()?;

        if arguments.len() != callable.arity() {
            return Err(Error::WrongArity {
                expected: callable.arity(),
                actual: arguments.len(),
                location: call.closing_paren.location,
            });
        }

        match callable.call(self, arguments, locals) {
            Err(Error::Return { value, .. }) => Ok(value),
            x => x,
        }
    }

    fn visit_lambda(self, x: &crate::ast::Lambda, locals: Locals) -> Result<Rc<RefCell<Value>>> {
        Ok(Rc::new(RefCell::new(Value::Lambda(Lambda {
            declaration: x.clone(),
            closure: locals,
        }))))
    }

    fn visit_get(self, expr: &crate::ast::Get, state: Locals) -> Result<Rc<RefCell<Value>>> {
        let instance_rc = self.evaluate(&expr.object, OptRc::clone(&state))?;
        let instance = instance_rc.borrow();

        match &*instance {
            Value::Instance(Instance { fields, class }) => {
                // Field
                if let Some(field) = fields.get(&expr.name.lexeme) {
                    Ok(Rc::clone(field))

                // Method
                } else if let Some(method_def) = class.borrow().find_method(&expr.name.lexeme) {
                    Ok(Rc::new(RefCell::new(Value::Function(self.bind_this(
                        &method_def.borrow(),
                        Rc::clone(&instance_rc),
                    )))))

                // Getter
                } else if let Some(getter) = class.borrow().find_getter(&expr.name.lexeme) {
                    match self
                        .bind_this(&getter.borrow(), Rc::clone(&instance_rc))
                        .call(self, vec![], state)
                    {
                        Err(Error::Return { value, .. }) => Ok(value),
                        x => x,
                    }
                } else {
                    Err(Error::UndefinedProperty {
                        object: instance.clone(),
                        property: expr.name.lexeme.clone(),
                        location: expr.name.location,
                    })
                }
            }

            Value::Class(class) => {
                if let Some(method) = class.borrow().find_class_method(&expr.name.lexeme) {
                    Ok(method)
                } else {
                    Err(Error::UndefinedProperty {
                        object: instance.clone(),
                        property: expr.name.lexeme.clone(),
                        location: expr.name.location,
                    })
                }
            }

            _ => Err(Error::PropertyOnNonObject {
                non_object: instance.clone(),
                property: expr.name.lexeme.clone(),
                location: expr.name.location,
            }),
        }
    }

    fn visit_set(self, expr: &crate::ast::Set, state: Locals) -> Result<Rc<RefCell<Value>>> {
        let object_rc = self.evaluate(&expr.object, OptRc::clone(&state))?;
        let mut object = object_rc.borrow_mut();

        match &mut *object {
            Value::Instance(instance) => {
                let value = self.evaluate(&expr.value, state)?;
                instance.set(&expr.name, &value);
                Ok(value)
            }
            _ => Err(Error::PropertyOnNonObject {
                non_object: object.clone(),
                property: expr.name.lexeme.clone(),
                location: expr.name.location,
            }),
        }
    }

    fn visit_this(self, expr: &crate::ast::This, state: Locals) -> Result<Rc<RefCell<Value>>> {
        self.visit_variable(
            &crate::ast::Variable {
                name: expr.keyword.clone(),
            },
            state,
        )
    }

    fn visit_super(self, expr: &crate::ast::Super, state: Locals) -> Result<Rc<RefCell<Value>>> {
        let binding = self.binding(&expr.keyword).unwrap();
        let superclass = match &*OptRc::clone(&state).unwrap().borrow().get(binding).borrow() {
            Variable::Value(value_rc_ref) => match &*Rc::clone(value_rc_ref).borrow() {
                Value::Class(c) => Rc::clone(c),
                _ => unreachable!(),
            },
            _ => unreachable!(),
        };

        let object = match &*OptRc::clone(&state)
            .unwrap()
            .borrow()
            .get(&Binding {
                scopes_up: binding.scopes_up - 1,
                index_in_scope: 0, // `this` is always the first thing bound in this scope
            })
            .borrow()
        {
            Variable::Value(value) => Rc::clone(value),
            _ => unreachable!(),
        };

        let method = superclass.borrow().find_method(&expr.method.lexeme);
        match method {
            Some(method) => {
                return Ok(Rc::new(RefCell::new(Value::Function(
                    self.bind_this(&method.borrow(), object),
                ))))
            }
            None => {
                // TODO dedup this and `visit_get`
                let getter = superclass.borrow().find_getter(&expr.method.lexeme);
                match getter {
                    Some(getter) => {
                        match self.bind_this(&getter.borrow(), Rc::clone(&object)).call(
                            self,
                            vec![],
                            state,
                        ) {
                            Err(Error::Return { value, .. }) => Ok(value),
                            x => x,
                        }
                    }
                    None => Err(Error::UndefinedProperty {
                        object: object.borrow().clone(),
                        property: expr.method.lexeme.clone(),
                        location: expr.method.location,
                    }),
                }
            }
        }
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

    fn visit_function(
        self,
        x: &crate::ast::Function,
        locals: Locals,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        let fun = Value::Function(Function {
            declaration: x.clone(),
            closure: OptRc::clone(&locals),
            force_return: None,
        });

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
        let binding = self.binding(&stmt.name).cloned();
        if let Some(binding) = &binding {
            state.as_ref().unwrap().borrow_mut().assign(binding, None);
        } else {
            self.globals.borrow_mut().define(&stmt.name, None);
        }

        let (state, superclass) = match &stmt.superclass {
            Some(superclass_var) => {
                let superclass_value_rc =
                    self.visit_variable(&superclass_var.1, OptRc::clone(&state))?;
                let superclass_value = superclass_value_rc.borrow();

                let state = match &stmt.superclass {
                    Some(_) => {
                        let env = LocalEnvironment::new(OptRc::clone(&state));
                        Some(Rc::new(RefCell::new(env)))
                    }
                    None => state,
                };

                match &*superclass_value {
                    Value::Class(superclass) => {
                        let binding = self.binding(&superclass_var.0).unwrap();
                        state.as_ref().unwrap().borrow_mut().assign(
                            binding,
                            Some(Rc::new(RefCell::new(Value::Class(Rc::clone(superclass))))),
                        );

                        (state, Some(Rc::clone(superclass)))
                    }
                    _ => {
                        return Err(Error::SuperclassNotClass {
                            class: stmt.name.lexeme.clone(),
                            superclass: superclass_value.clone(),
                            location: superclass_var.1.name.location,
                        })
                    }
                }
            }
            None => (state, None),
        };

        let mut methods = HashMap::new();
        for method in &stmt.methods {
            methods.insert(
                method.name.lexeme.clone(),
                Rc::new(RefCell::new(Function {
                    declaration: method.clone(),
                    closure: OptRc::clone(&state),
                    force_return: None,
                })),
            );
        }
        let methods = methods;

        let mut getters = HashMap::new();
        for getter in &stmt.getters {
            getters.insert(
                getter.name.lexeme.clone(),
                Rc::new(RefCell::new(Function {
                    declaration: getter.clone(),
                    closure: OptRc::clone(&state),
                    force_return: None,
                })),
            );
        }
        let getters = getters;

        let mut class_methods = HashMap::new();
        for class_method in &stmt.class_methods {
            class_methods.insert(
                class_method.name.lexeme.clone(),
                Rc::new(RefCell::new(Value::Function(Function {
                    declaration: class_method.clone(),
                    closure: OptRc::clone(&state),
                    force_return: None,
                }))),
            );
        }
        let class_methods = class_methods;
        let has_superclass = superclass.is_some();

        let class = Class {
            name: stmt.name.lexeme.clone(),
            superclass,
            methods,
            class_methods,
            getters,
            left_brace: stmt.left_brace.clone(),
        };
        let class = Value::Class(Rc::new(RefCell::new(class)));
        let class = Rc::new(RefCell::new(class));

        let state = if has_superclass {
            state.unwrap().borrow().get_parent()
        } else {
            state
        };

        if let Some(binding) = &binding {
            state.unwrap().borrow_mut().assign(binding, Some(class));
        } else {
            let _ = self.globals.borrow_mut().assign(&stmt.name, Some(class));
        }

        Ok(None)
    }
}
