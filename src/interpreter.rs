use thiserror::Error;

use crate::{
    ast::{walk_expr, Expr, Literal, Visitor},
    token::{Token, TokenValue},
    types::{Number, SourceLocation},
};

#[derive(Debug, PartialEq, Clone)]
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
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Nil => f.write_str("nil"),
            Self::Boolean(b) => b.fmt(f),
            Self::Number(n) => n.fmt(f),
            Self::String(s) => write!(f, "{:?}", s),
        }
    }
}

#[derive(Error, Debug)]
pub enum Error {
    #[error(
        "`{operator}` expected one of: [{}], found {actual} of type {} at {location}",
        .expected.join(", "),
        .actual.type_of())
    ]
    InvalidOperand {
        operator: TokenValue,
        expected: Vec<String>,
        actual: Value,
        location: SourceLocation,
    },

    #[error(
        "`{operator}` arguments mismatched: {left} of type {} vs {right} of type {} at {location}",
        .left.type_of(), .right.type_of()
    )]
    OperandMismatch {
        operator: TokenValue,
        left: Value,
        right: Value,
        location: SourceLocation,
    },

    #[error("Division by zero at {location}")]
    DivisionByZero { location: SourceLocation },
}

pub type Result<V = Value, E = Error> = std::result::Result<V, E>;

pub struct Interpreter {
    source: Vec<u8>, // To resolve code locations for error reporting
}

impl Visitor<Result> for &mut Interpreter {
    fn visit_literal(&mut self, x: &crate::ast::Literal) -> Result {
        Ok(match x {
            Literal::Nil => Value::Nil,
            Literal::False => Value::Boolean(false),
            Literal::True => Value::Boolean(true),
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
        })
    }

    fn visit_unary(&mut self, x: &crate::ast::Unary) -> Result {
        let right = self._evaluate(&x.right)?;

        match x.operator.value {
            TokenValue::Minus => match right {
                Value::Number(n) => Ok(Value::Number(-n)),
                v => self.err_invalid_operand(&x.operator, &["Number"], v),
            },
            TokenValue::Bang => Ok(Value::Boolean(!right.is_truthy())),
            _ => unreachable!(),
        }
    }

    fn visit_binary(&mut self, x: &crate::ast::Binary) -> Result {
        let left = self._evaluate(&x.left)?;
        let right = self._evaluate(&x.right)?;
        let op = &x.operator;

        match op.value {
            TokenValue::Minus => match (left, right) {
                (Value::Number(l), Value::Number(r)) => Ok(Value::Number(l - r)),
                (Value::Number(_), v) => self.err_invalid_operand(op, &["Number"], v),
                (v, _) => self.err_invalid_operand(op, &["Number"], v),
            },
            TokenValue::Slash => match (left, right) {
                (Value::Number(_), Value::Number(r)) if r == 0.0 => Err(Error::DivisionByZero {
                    location: SourceLocation::new(&self.source, op.offset),
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
                (Value::String(l), Value::String(r)) => Ok(Value::String(format!("{}{}", l, r))),
                (Value::Number(_), Value::String(_)) | (Value::String(_), Value::Number(_)) => {
                    Err(Error::OperandMismatch {
                        operator: op.value.clone(),
                        left,
                        right,
                        location: SourceLocation::new(&self.source, op.offset),
                    })
                }
                (Value::String(_) | Value::Number(_), _) => {
                    self.err_invalid_operand(op, &["Number"], right)
                }
                _ => self.err_invalid_operand(op, &["Number"], left),
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

    fn visit_ternary(&mut self, x: &crate::ast::Ternary) -> Result {
        let cond = self._evaluate(&x.left)?;
        if cond.is_truthy() {
            self._evaluate(&x.mid)
        } else {
            self._evaluate(&x.right)
        }
    }

    fn visit_grouping(&mut self, x: &crate::ast::Grouping) -> Result {
        self._evaluate(&x.expr)
    }
}

impl Interpreter {
    #[must_use]
    pub fn new() -> Self {
        Self { source: vec![] }
    }

    fn _evaluate(&mut self, expr: &Expr) -> Result {
        walk_expr(self, expr)
    }

    pub fn evaluate(&mut self, source: Vec<u8>, expr: &Expr) -> Result {
        self.source = source;
        self._evaluate(expr)
    }

    fn err_invalid_operand<S: ToString>(
        &self,
        op: &Token,
        expected: &[S],
        actual: Value,
    ) -> Result {
        Err(Error::InvalidOperand {
            operator: op.value.clone(),
            expected: Vec::from_iter(expected.iter().map(ToString::to_string)),
            actual,
            location: SourceLocation::new(&self.source, op.offset),
        })
    }
}
