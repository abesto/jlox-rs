use replace_with::replace_with_or_abort;
use thiserror::Error;

use crate::{
    ast::{walk_expr, walk_stmt, Expr, ExprVisitor, Literal, Stmt, StmtVisitor},
    environment::Environment,
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

#[derive(Error, Debug)]
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
}

pub type Result<V = Option<Value>, E = Error> = std::result::Result<V, E>;

pub struct Interpreter {
    source: Vec<u8>, // To resolve code locations for error reporting
    environment: Environment,
}

impl Interpreter {
    #[must_use]
    pub fn new() -> Self {
        Self {
            source: vec![],
            environment: Environment::root(),
        }
    }

    fn _evaluate(&mut self, expr: &Expr) -> Result<Value> {
        walk_expr(self, expr)
    }

    fn _interpret(&mut self, program: &[Stmt]) -> Result {
        let mut ret = None;
        for stmt in program {
            ret = self._execute(stmt)?;
        }

        Ok(ret)
    }

    fn _execute(&mut self, stmt: &Stmt) -> Result {
        walk_stmt(&mut *self, stmt)
    }

    pub fn interpret(&mut self, source: Vec<u8>, program: &[Stmt]) -> Result {
        self.source = source;
        self._interpret(program)
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
            location: SourceLocation::new(&self.source, op.offset),
        })
    }

    fn execute_block(&mut self, statements: &[Stmt]) -> Result {
        replace_with_or_abort(&mut self.environment, |e| e.nested());
        let ret = statements
            .iter()
            .map(|stmt| self._execute(stmt))
            .try_fold(None, |_, x| x);
        replace_with_or_abort(&mut self.environment, |e| e.unwind().unwrap());
        ret
    }
}

impl ExprVisitor<Result<Value>> for &mut Interpreter {
    fn visit_literal(&mut self, x: &crate::ast::Literal) -> Result<Value> {
        Ok(match x {
            Literal::Nil => Value::Nil,
            Literal::False => Value::Boolean(false),
            Literal::True => Value::Boolean(true),
            Literal::Number(n) => Value::Number(*n),
            Literal::String(s) => Value::String(s.clone()),
        })
    }

    fn visit_unary(&mut self, x: &crate::ast::Unary) -> Result<Value> {
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

    fn visit_binary(&mut self, x: &crate::ast::Binary) -> Result<Value> {
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

    fn visit_ternary(&mut self, x: &crate::ast::Ternary) -> Result<Value> {
        let cond = self._evaluate(&x.left)?;
        if cond.is_truthy() {
            self._evaluate(&x.mid)
        } else {
            self._evaluate(&x.right)
        }
    }

    fn visit_grouping(&mut self, x: &crate::ast::Grouping) -> Result<Value> {
        self._evaluate(&x.expr)
    }

    fn visit_variable(&mut self, x: &crate::ast::Variable) -> Result<Value> {
        match self.environment.get(&x.name) {
            Some(v) => Ok(v.clone()),
            None => Err(Error::UndefinedVariable {
                name: x.name.lexeme.clone(),
                location: SourceLocation::new(&self.source, x.name.offset),
            }),
        }
    }

    fn visit_assign(&mut self, x: &crate::ast::Assign) -> Result<Value> {
        let value = self._evaluate(&x.value)?;
        if !self.environment.assign(&x.name, value.clone()) {
            Err(Error::UndefinedVariable {
                name: x.name.lexeme.clone(),
                location: SourceLocation::new(&self.source, x.name.offset),
            })
        } else {
            Ok(value)
        }
    }
}

impl StmtVisitor<Result<Option<Value>>> for &mut Interpreter {
    fn visit_expression(&mut self, x: &crate::ast::Expression) -> Result<Option<Value>> {
        self._evaluate(&x.expr).map(Some)
    }

    fn visit_print(&mut self, x: &crate::ast::Print) -> Result<Option<Value>> {
        println!("{}", self._evaluate(&x.expr)?);
        Ok(None)
    }

    fn visit_var(&mut self, x: &crate::ast::Var) -> Result<Option<Value>> {
        let value = match &x.initializer {
            Some(expr) => self._evaluate(expr)?,
            None => Value::Nil,
        };

        self.environment.define(&x.name.lexeme, value);
        Ok(None)
    }

    fn visit_block(&mut self, x: &crate::ast::Block) -> Result<Option<Value>> {
        self.execute_block(&x.statements)
    }
}
