use macros::ResolveErrorLocation;
use thiserror::Error;

use crate::{
    ast::{walk_expr, walk_stmt, Expr, ExprVisitor, Literal, Stmt, StmtVisitor},
    environment::{Environment, Variable},
    token::{Token, TokenValue},
    types::{Number, SourceLocation},
};

#[derive(PartialEq, Clone, Debug)]
pub enum Value {
    Nil,
    Boolean(bool),
    Number(Number),
    String(String),
}

impl Value {
    fn is_truthy(&self) -> bool {
        !matches!(self, Self::Nil | Self::Boolean(false))
    }

    fn type_of(&self) -> String {
        match self {
            Self::Nil => "Nil",
            Self::Boolean(_) => "Boolean",
            Self::Number(_) => "Number",
            Self::String(_) => "String",
        }
        .to_string()
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
}

pub type Result<V = Option<Value>, E = Error> = std::result::Result<V, E>;

pub struct Interpreter {}

impl Interpreter {
    #[must_use]
    pub fn new() -> Self {
        Self {}
    }

    fn evaluate(&mut self, expr: &Expr, env: &mut Environment) -> Result<Value> {
        walk_expr(self, expr, env)
    }

    pub fn interpret(&mut self, program: &[Stmt], env: &mut Environment) -> Result {
        let mut ret = None;
        for stmt in program {
            ret = self.execute(stmt, env)?;
        }
        Ok(ret)
    }

    fn execute(&mut self, stmt: &Stmt, env: &mut Environment) -> Result {
        walk_stmt(&mut *self, stmt, env)
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

    fn execute_block(&mut self, statements: &[Stmt], env: &mut Environment) -> Result {
        env.nested(|block_env| {
            statements
                .iter()
                .map(|stmt| self.execute(stmt, block_env))
                .try_fold(None, |_, x| x)
        })
    }
}

impl ExprVisitor<Result<Value>, Environment> for &mut Interpreter {
    fn visit_literal(&mut self, x: &crate::ast::Literal, _: &mut Environment) -> Result<Value> {
        Ok(match x {
            Literal::Nil => Value::Nil,
            Literal::False => Value::Boolean(false),
            Literal::True => Value::Boolean(true),
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
        })
    }

    fn visit_unary(&mut self, x: &crate::ast::Unary, env: &mut Environment) -> Result<Value> {
        let right = self.evaluate(&x.right, env)?;

        match x.operator.value {
            TokenValue::Minus => match right {
                Value::Number(n) => Ok(Value::Number(-n)),
                v => self.err_invalid_operand(&x.operator, &["Number"], v),
            },
            TokenValue::Bang => Ok(Value::Boolean(!right.is_truthy())),
            _ => unreachable!(),
        }
    }

    fn visit_binary(&mut self, x: &crate::ast::Binary, env: &mut Environment) -> Result<Value> {
        let left = self.evaluate(&x.left, env)?;
        let right = self.evaluate(&x.right, env)?;
        let op = &x.operator;

        match op.value {
            TokenValue::Minus => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
                (Value::Number(_), v) => self.err_invalid_operand(op, &["Number"], v),
                (v, _) => self.err_invalid_operand(op, &["Number"], v),
            },

            TokenValue::Slash => match (left, right) {
                (Value::Number(_), Value::Number(r)) if r == 0.0 => Err(Error::DivisionByZero {
                    location: SourceLocation::new(op.offset),
                }),
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l / r)),
                (Value::Number(_), v) => self.err_invalid_operand(op, &["Number"], v),
                (v, _) => self.err_invalid_operand(op, &["Number"], v),
            },

            TokenValue::Star => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l * r)),
                (Value::Number(_), v) => self.err_invalid_operand(op, &["Number"], v),
                (v, _) => self.err_invalid_operand(op, &["Number"], v),
            },

            TokenValue::Plus => match (&left, &right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l + r)),
                (Value::String(_), _) | (_, Value::String(_)) => {
                    Ok(Value::String(format!("{}{}", left, right)))
                }
                (Value::Number(_), _) => self.err_invalid_operand(op, &["Number", "String"], right),
                _ => self.err_invalid_operand(op, &["Number", "String"], left),
            },

            TokenValue::Greater => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l > r)),
                (Value::Number(_), v) => self.err_invalid_operand(op, &["Number"], v),
                (v, _) => self.err_invalid_operand(op, &["Number"], v),
            },

            TokenValue::Less => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l < r)),
                (Value::Number(_), v) => self.err_invalid_operand(op, &["Number"], v),
                (v, _) => self.err_invalid_operand(op, &["Number"], v),
            },

            TokenValue::GreaterEqual => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l >= r)),
                (Value::Number(_), v) => self.err_invalid_operand(op, &["Number"], v),
                (v, _) => self.err_invalid_operand(op, &["Number"], v),
            },

            TokenValue::LessEqual => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Boolean(l <= r)),
                (Value::Number(_), v) => self.err_invalid_operand(op, &["Number"], v),
                (v, _) => self.err_invalid_operand(op, &["Number"], v),
            },

            TokenValue::EqualEqual => Ok(Value::Boolean(left == right)),
            TokenValue::BangEqual => Ok(Value::Boolean(left != right)),

            _ => unreachable!(),
        }
    }

    fn visit_ternary(&mut self, x: &crate::ast::Ternary, env: &mut Environment) -> Result<Value> {
        let cond = self.evaluate(&x.left, env)?;
        if cond.is_truthy() {
            self.evaluate(&x.mid, env)
        } else {
            self.evaluate(&x.right, env)
        }
    }

    fn visit_grouping(&mut self, x: &crate::ast::Grouping, env: &mut Environment) -> Result<Value> {
        self.evaluate(&x.expr, env)
    }

    fn visit_variable(&mut self, x: &crate::ast::Variable, env: &mut Environment) -> Result<Value> {
        match env.get(&x.name) {
            Some(Variable::Value(v)) => Ok(v.clone()),
            Some(Variable::Uninitialized) => Err(Error::UninitializedVariable {
                name: x.name.lexeme.clone(),
                location: SourceLocation::new(x.name.offset),
            }),
            None => Err(Error::UndefinedVariable {
                name: x.name.lexeme.clone(),
                location: SourceLocation::new(x.name.offset),
            }),
        }
    }

    fn visit_assign(&mut self, x: &crate::ast::Assign, env: &mut Environment) -> Result<Value> {
        let value = self.evaluate(&x.value, env)?;
        if !env.assign(&x.name, value.clone()) {
            Err(Error::UndefinedVariable {
                name: x.name.lexeme.clone(),
                location: SourceLocation::new(x.name.offset),
            })
        } else {
            Ok(value)
        }
    }
}

impl StmtVisitor<Result<Option<Value>>, Environment> for &mut Interpreter {
    fn visit_block(
        &mut self,
        x: &crate::ast::Block,
        env: &mut Environment,
    ) -> Result<Option<Value>> {
        self.execute_block(&x.statements, env)
    }

    fn visit_expression(
        &mut self,
        x: &crate::ast::Expression,
        env: &mut Environment,
    ) -> Result<Option<Value>> {
        self.evaluate(&x.expr, env).map(Some)
    }

    fn visit_if(&mut self, x: &crate::ast::If, env: &mut Environment) -> Result<Option<Value>> {
        if self.evaluate(&x.condition, env)?.is_truthy() {
            self.execute(&x.then_branch, env)
        } else if let Some(branch) = &x.else_branch {
            self.execute(branch, env)
        } else {
            Ok(None)
        }
    }

    fn visit_print(
        &mut self,
        x: &crate::ast::Print,
        env: &mut Environment,
    ) -> Result<Option<Value>> {
        println!("{}", self.evaluate(&x.expr, env)?);
        Ok(None)
    }

    fn visit_var(&mut self, x: &crate::ast::Var, env: &mut Environment) -> Result<Option<Value>> {
        let value = match &x.initializer {
            Some(expr) => Some(self.evaluate(expr, env)?),
            None => None,
        };

        env.define(&x.name.lexeme, value);
        Ok(None)
    }
}
