use std::{cell::RefCell, rc::Rc};

use derivative::Derivative;
use macros::ResolveErrorLocation;
use thiserror::Error;

use crate::{
    ast::{Expr, ExprVisitor, Function, Lambda, Literal, Stmt, StmtVisitor, Walkable},
    environment::{Environment, Variable},
    token::{Token, TokenValue},
    types::{Number, SourceLocation},
};

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
        fun: fn(&mut Interpreter, Rc<RefCell<Environment>>, Vec<Rc<RefCell<Value>>>) -> Value,
    },

    Function {
        declaration: Function,
        closure: Rc<RefCell<Environment>>,
    },

    Lambda {
        declaration: Lambda,
        closure: Rc<RefCell<Environment>>,
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
            Self::Nil => "Nil",
            Self::Boolean(_) => "Boolean",
            Self::Number(_) => "Number",
            Self::String(_) => "String",
            Self::NativeFunction { .. } => "Function",
            Self::Function { .. } => "Function",
            Self::Lambda { .. } => "Function",
        }
        .to_string()
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
            _ => panic!("Internal error: tried to check arity of non-callable; this should've been caught sooner")
        }
    }

    fn call(
        &self,
        interpreter: &mut Interpreter,
        env: Rc<RefCell<Environment>>,
        args: Vec<Rc<RefCell<Value>>>,
    ) -> Result<Rc<RefCell<Value>>> {
        match self {
            Value::NativeFunction { fun, .. } => {
                Ok(Rc::new(RefCell::new(fun(interpreter, env, args))))
            }
            Value::Function {
                declaration: Function { params, body, .. },
                closure,
            }
            | Value::Lambda {
                declaration: Lambda { params, body, .. },
                closure,
            } => Environment::nested(closure, |env| {
                for (param, arg) in params.iter().zip(args) {
                    env.borrow_mut().define(&param.lexeme, Some(arg))
                }
                interpreter
                    .execute_block(body, env)
                    .map(Option::unwrap_or_default)
            }),
            _ => panic!(
                "Internal error: tried to invoke non-callable; this should've been caught sooner"
            ),
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
}

pub type Result<V = Option<Rc<RefCell<Value>>>, E = Error> = std::result::Result<V, E>;

pub struct Interpreter {}

impl Interpreter {
    #[must_use]
    pub fn new() -> Self {
        Self {}
    }

    fn evaluate(
        &mut self,
        expr: &Expr,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Rc<RefCell<Value>>> {
        expr.walk(self, env)
    }

    pub fn interpret(&mut self, program: &[Stmt], env: Rc<RefCell<Environment>>) -> Result {
        let mut ret = None;
        for stmt in program {
            ret = self.execute(stmt, Rc::clone(&env))?;
        }
        Ok(ret)
    }

    fn execute(&mut self, stmt: &Stmt, env: Rc<RefCell<Environment>>) -> Result {
        stmt.walk(self, env)
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
            location: SourceLocation::new(op.offset),
        })
    }

    fn execute_block(&mut self, statements: &[Stmt], env: Rc<RefCell<Environment>>) -> Result {
        Environment::nested(&env, |block_env| {
            statements
                .iter()
                .map(|stmt| self.execute(stmt, Rc::clone(&block_env)))
                .try_fold(None, |_, x| x)
        })
    }
}

impl ExprVisitor<Result<Rc<RefCell<Value>>>, Rc<RefCell<Environment>>> for &mut Interpreter {
    fn visit_literal(
        self,
        x: &crate::ast::Literal,
        _: Rc<RefCell<Environment>>,
    ) -> Result<Rc<RefCell<Value>>> {
        Ok(Rc::new(RefCell::new(match x {
            Literal::Nil => Value::Nil,
            Literal::False => Value::Boolean(false),
            Literal::True => Value::Boolean(true),
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
        })))
    }

    fn visit_unary(
        self,
        x: &crate::ast::Unary,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Rc<RefCell<Value>>> {
        let right = self.evaluate(&x.right, env)?;

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

    fn visit_binary(
        self,
        x: &crate::ast::Binary,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Rc<RefCell<Value>>> {
        let left_rc = self.evaluate(&x.left, Rc::clone(&env))?;
        let left = &*left_rc.borrow();
        let right_rc = &*self.evaluate(&x.right, Rc::clone(&env))?;
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
                    location: SourceLocation::new(op.offset),
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

            x => unreachable!("Unrecognized binary operator `{}` at {}", x, op.offset),
        }
    }

    fn visit_ternary(
        self,
        x: &crate::ast::Ternary,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Rc<RefCell<Value>>> {
        let cond = self.evaluate(&x.left, Rc::clone(&env))?;
        if cond.borrow().is_truthy() {
            self.evaluate(&x.mid, Rc::clone(&env))
        } else {
            self.evaluate(&x.right, Rc::clone(&env))
        }
    }

    fn visit_grouping(
        self,
        x: &crate::ast::Grouping,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Rc<RefCell<Value>>> {
        self.evaluate(&x.expr, env)
    }

    fn visit_variable(
        self,
        x: &crate::ast::Variable,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Rc<RefCell<Value>>> {
        match env.borrow().get(&x.name) {
            Some(var) => match &*var.borrow() {
                Variable::Value(v) => Ok(Rc::clone(v)),
                Variable::Uninitialized => Err(Error::UninitializedVariable {
                    name: x.name.lexeme.clone(),
                    location: SourceLocation::new(x.name.offset),
                }),
            },
            None => Err(Error::UndefinedVariable {
                name: x.name.lexeme.clone(),
                location: SourceLocation::new(x.name.offset),
            }),
        }
    }

    fn visit_assign(
        self,
        x: &crate::ast::Assign,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Rc<RefCell<Value>>> {
        let value = self.evaluate(&x.value, Rc::clone(&env))?;
        if !env.borrow_mut().assign(&x.name, Rc::clone(&value)) {
            Err(Error::UndefinedVariable {
                name: x.name.lexeme.clone(),
                location: SourceLocation::new(x.name.offset),
            })
        } else {
            Ok(value)
        }
    }

    fn visit_logical(
        self,
        x: &crate::ast::Logical,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Rc<RefCell<Value>>> {
        let left = self.evaluate(&x.left, Rc::clone(&env))?;
        if x.operator.value == TokenValue::Or {
            if left.borrow().is_truthy() {
                return Ok(left);
            }
        } else if !left.borrow().is_truthy() {
            return Ok(left);
        }
        self.evaluate(&x.right, env)
    }

    fn visit_call(
        self,
        call: &crate::ast::Call,
        state: Rc<RefCell<Environment>>,
    ) -> Result<Rc<RefCell<Value>>> {
        let value_rc = self.evaluate(&call.callee, Rc::clone(&state))?;
        let value = &*value_rc.borrow();
        let callee = match value {
            f @ Value::NativeFunction { .. }
            | f @ Value::Lambda { .. }
            | f @ Value::Function { .. } => Ok(f),
            what => Err(Error::NotCallable {
                what: what.clone(),
                location: call.closing_paren.offset.into(),
            }),
        }?;

        let arguments = call
            .arguments
            .iter()
            .map(|arg| self.evaluate(arg, Rc::clone(&state)))
            .collect::<Result<Vec<_>>>()?;

        if arguments.len() != callee.arity() {
            return Err(Error::WrongArity {
                expected: callee.arity(),
                actual: arguments.len(),
                location: call.closing_paren.offset.into(),
            });
        }

        match callee.call(self, state, arguments) {
            Err(Error::Return { value, .. }) => Ok(value),
            x => x,
        }
    }

    fn visit_lambda(
        self,
        x: &crate::ast::Lambda,
        state: Rc<RefCell<Environment>>,
    ) -> Result<Rc<RefCell<Value>>> {
        Ok(Rc::new(RefCell::new(Value::Lambda {
            declaration: x.clone(),
            closure: Rc::clone(&state),
        })))
    }
}

impl StmtVisitor<Result<Option<Rc<RefCell<Value>>>>, Rc<RefCell<Environment>>>
    for &mut Interpreter
{
    fn visit_block(
        self,
        x: &crate::ast::Block,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        self.execute_block(&x.statements, env)
    }

    fn visit_expression(
        self,
        x: &crate::ast::Expression,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        self.evaluate(&x.expr, env).map(Some)
    }

    fn visit_if(
        self,
        x: &crate::ast::If,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        if self
            .evaluate(&x.condition, Rc::clone(&env))?
            .borrow()
            .is_truthy()
        {
            self.execute(&x.then_branch, env)
        } else if let Some(branch) = &x.else_branch {
            self.execute(branch, env)
        } else {
            Ok(None)
        }
    }

    fn visit_print(
        self,
        x: &crate::ast::Print,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        println!("{}", self.evaluate(&x.expr, env)?.borrow());
        Ok(None)
    }

    fn visit_var(
        self,
        x: &crate::ast::Var,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        let value = match &x.initializer {
            Some(expr) => Some(self.evaluate(expr, Rc::clone(&env))?),
            None => None,
        };

        env.borrow_mut().define(&x.name.lexeme, value);
        Ok(None)
    }

    fn visit_while(
        self,
        x: &crate::ast::While,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        while self
            .evaluate(&x.condition, Rc::clone(&env))?
            .borrow()
            .is_truthy()
        {
            match self.execute(&x.statement, Rc::clone(&env)) {
                Err(Error::Break { .. }) => return Ok(None),
                x => x?,
            };
        }
        Ok(None)
    }

    fn visit_break(
        self,
        x: &crate::ast::Break,
        _env: Rc<RefCell<Environment>>,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        Err(Error::Break {
            location: SourceLocation::new(x.token.offset),
        })
    }

    fn visit_function(
        self,
        x: &Function,
        state: Rc<RefCell<Environment>>,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        let fun = Value::Function {
            declaration: x.clone(),
            closure: Rc::clone(&state),
        };
        state
            .borrow_mut()
            .define(x.name.lexeme.clone(), Some(Rc::new(RefCell::new(fun))));
        Ok(None)
    }

    fn visit_return(
        self,
        ret: &crate::ast::Return,
        env: Rc<RefCell<Environment>>,
    ) -> Result<Option<Rc<RefCell<Value>>>> {
        let value = if let Some(value_expr) = &ret.value {
            self.evaluate(value_expr, env)?
        } else {
            Rc::new(RefCell::new(Value::Nil))
        };

        Err(Error::Return {
            location: ret.keyword.offset.into(),
            value,
        })
    }
}
