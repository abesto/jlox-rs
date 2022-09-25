use std::collections::HashMap;

use macros::ResolveErrorLocation;
use thiserror::Error;

use crate::{
    ast::{ExprVisitor, Stmt, StmtVisitor, Walkable},
    token::Token,
    types::SourceLocation,
};

#[derive(Error, Debug, ResolveErrorLocation)]
pub enum Error {
    #[error("Can't read local variable `{name}` in its own initializer at {location}")]
    SelfReferencingInitializer {
        name: String,
        location: SourceLocation,
    },

    #[error("Variable `{name}` already exists in scope. This `var` statement: {location}")]
    DoubleDeclaration {
        name: String,
        location: SourceLocation,
    },
}

type Output = ();
pub type CommandIndex = usize;
pub type Bindings = HashMap<SourceLocation, usize>;
type State = Bindings;
type Result<T = Output, E = Vec<Error>> = std::result::Result<T, E>;

/// Concatenate list of errors if there are any, otherwise return the latest value
fn combine_results(l: Result, r: Result) -> Result {
    match (l, r) {
        (Ok(_), x @ Ok(_)) | (x @ Err(_), Ok(_)) | (Ok(_), x @ Err(_)) => x,
        (Err(mut l), Err(mut r)) => {
            l.append(&mut r);
            Err(l)
        }
    }
}

#[derive(Default)]
pub struct Resolver {
    scopes: Vec<HashMap<String, bool>>,
}

impl Resolver {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Default::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    pub fn resolve(&mut self, statements: &[Stmt]) -> Result<Bindings> {
        let mut state = State::new();
        self.resolve_statements(statements, &mut state)?;
        Ok(state)
    }

    fn resolve_statements(&mut self, statements: &[Stmt], state: &mut State) -> Result {
        statements
            .iter()
            .map(|s| s.walk(&mut *self, state))
            .reduce(combine_results)
            .unwrap_or(Ok(()))
    }

    fn resolve_local(&mut self, name: &Token, state: &mut State) {
        for (i, scope) in self.scopes.iter().enumerate().rev() {
            if scope.contains_key(&name.lexeme) {
                state.insert(name.location, self.scopes.len() - 1 - i);
            }
        }
    }

    fn declare(&mut self, name: &Token) -> Result {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.contains_key(&name.lexeme) {
                return Err(vec![Error::DoubleDeclaration {
                    name: name.lexeme.clone(),
                    location: name.location,
                }]);
            }
            scope.insert(name.lexeme.clone(), false);
        }
        Ok(())
    }

    fn define(&mut self, name: &Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme.clone(), true);
        }
    }

    fn resolve_function(&mut self, params: &[Token], body: &[Stmt], state: &mut State) -> Result {
        self.begin_scope();
        let mut result = Ok(());
        for param in params {
            result = combine_results(result, self.declare(param));
            self.define(param);
        }
        result = combine_results(result, self.resolve_statements(body, state));
        self.end_scope();
        result
    }
}

impl ExprVisitor<Result, &mut State> for &mut Resolver {
    fn visit_variable(self, expr: &crate::ast::Variable, state: &mut State) -> Result {
        if let Some(scope) = self.scopes.last() {
            if scope.get(&expr.name.lexeme) == Some(&false) {
                return Err(vec![Error::SelfReferencingInitializer {
                    name: expr.name.lexeme.clone(),
                    location: expr.name.location,
                }]);
            }
        }

        self.resolve_local(&expr.name, state);
        Ok(())
    }

    fn visit_assign(self, expr: &crate::ast::Assign, state: &mut State) -> Result {
        let r = expr.value.walk(&mut *self, state);
        self.resolve_local(&expr.name, state);
        r
    }

    fn visit_literal(self, _expr: &crate::ast::Literal, _state: &mut State) -> Result {
        Ok(())
    }

    fn visit_unary(self, expr: &crate::ast::Unary, state: &mut State) -> Result {
        expr.right.walk(self, state)
    }

    fn visit_binary(self, expr: &crate::ast::Binary, state: &mut State) -> Result {
        combine_results(
            expr.left.walk(&mut *self, state),
            expr.right.walk(self, state),
        )
    }

    fn visit_call(self, expr: &crate::ast::Call, state: &mut State) -> Result {
        let callee_result = expr.callee.walk(&mut *self, state);
        expr.arguments
            .iter()
            .map(|arg| arg.walk(&mut *self, state))
            .fold(callee_result, combine_results)
    }

    fn visit_logical(self, expr: &crate::ast::Logical, state: &mut State) -> Result {
        combine_results(
            expr.left.walk(&mut *self, state),
            expr.right.walk(self, state),
        )
    }

    fn visit_lambda(self, expr: &crate::ast::Lambda, state: &mut State) -> Result {
        self.resolve_function(&expr.params, &expr.body, state)
    }

    fn visit_ternary(self, expr: &crate::ast::Ternary, state: &mut State) -> Result {
        combine_results(
            expr.left.walk(&mut *self, state),
            combine_results(
                expr.mid.walk(&mut *self, state),
                expr.right.walk(&mut *self, state),
            ),
        )
    }

    fn visit_grouping(self, expr: &crate::ast::Grouping, state: &mut State) -> Result {
        expr.expr.walk(self, state)
    }
}

impl StmtVisitor<Result, &mut State> for &mut Resolver {
    fn visit_block(self, stmt: &crate::ast::Block, state: &mut State) -> Result {
        self.begin_scope();
        let r = self.resolve_statements(&stmt.statements, state);
        self.end_scope();
        r
    }

    fn visit_expression(self, stmt: &crate::ast::Expression, state: &mut State) -> Result {
        stmt.expr.walk(self, state)
    }

    fn visit_function(self, stmt: &crate::ast::Function, state: &mut State) -> Result {
        let result = self.declare(&stmt.name);
        self.define(&stmt.name);
        combine_results(
            result,
            self.resolve_function(&stmt.params, &stmt.body, state),
        )
    }

    fn visit_if(self, stmt: &crate::ast::If, state: &mut State) -> Result {
        let result = combine_results(
            stmt.condition.walk(&mut *self, state),
            stmt.then_branch.walk(&mut *self, state),
        );
        if let Some(else_branch) = &stmt.else_branch {
            combine_results(result, else_branch.walk(self, state))
        } else {
            result
        }
    }

    fn visit_print(self, stmt: &crate::ast::Print, state: &mut State) -> Result {
        stmt.expr.walk(self, state)
    }

    fn visit_return(self, stmt: &crate::ast::Return, state: &mut State) -> Result {
        if let Some(expr) = &stmt.value {
            expr.walk(self, state)
        } else {
            Ok(())
        }
    }

    fn visit_var(self, stmt: &crate::ast::Var, state: &mut State) -> Result {
        let r = combine_results(
            self.declare(&stmt.name),
            if let Some(initializer) = &stmt.initializer {
                initializer.walk(&mut *self, state)
            } else {
                Ok(())
            },
        );
        self.define(&stmt.name);
        r
    }

    fn visit_while(self, stmt: &crate::ast::While, state: &mut State) -> Result {
        combine_results(
            stmt.condition.walk(&mut *self, state),
            stmt.statement.walk(self, state),
        )
    }

    fn visit_break(self, _stmt: &crate::ast::Break, _state: &mut State) -> Result {
        Ok(())
    }
}
