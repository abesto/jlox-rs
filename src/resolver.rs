use std::collections::HashMap;

use macros::ResolveErrorLocation;
use thiserror::Error;

use crate::{
    ast::{Expr, ExprVisitor, Stmt, StmtVisitor, Walkable},
    token::Token,
    types::SourceLocation,
};

#[derive(Error, Debug, ResolveErrorLocation)]
enum Error {
    #[error("Can't read local variable `{name}` in its own initializer at {location}")]
    SelfReferencingInitializer {
        name: String,
        location: SourceLocation,
    },
}

type Output = ();
type State = HashMap<SourceLocation, usize>;
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
    fn new() -> Self {
        Self::default()
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Default::default());
    }

    fn end_scope(&mut self) {
        self.scopes.pop();
    }

    fn resolve_statements(&mut self, statements: &[Stmt], state: &mut State) -> Result {
        statements
            .iter()
            .map(|s| s.walk(&mut *self, state))
            .reduce(combine_results)
            .unwrap()
    }

    fn resolve_local(&mut self, name: &Token, state: &mut State) {
        for (i, scope) in self.scopes.iter().rev().enumerate() {
            if scope.contains_key(&name.lexeme) {
                state.insert(name.offset.into(), self.scopes.len() - 1 - i);
            }
        }
    }

    fn declare(&mut self, name: Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme, false);
        }
    }

    fn define(&mut self, name: Token) {
        if let Some(scope) = self.scopes.last_mut() {
            scope.insert(name.lexeme, true);
        }
    }
}

impl ExprVisitor<Result, &mut State> for &mut Resolver {
    fn visit_variable(self, expr: &crate::ast::Variable, state: &mut State) -> Result {
        if let Some(scope) = self.scopes.last() {
            if scope.get(&expr.name.lexeme) == Some(&false) {
                return Err(vec![Error::SelfReferencingInitializer {
                    name: expr.name.lexeme.clone(),
                    location: expr.name.offset.into(),
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

    fn visit_literal(self, expr: &crate::ast::Literal, state: &mut State) -> Result {
        todo!()
    }

    fn visit_unary(self, expr: &crate::ast::Unary, state: &mut State) -> Result {
        todo!()
    }

    fn visit_binary(self, expr: &crate::ast::Binary, state: &mut State) -> Result {
        todo!()
    }

    fn visit_call(self, expr: &crate::ast::Call, state: &mut State) -> Result {
        todo!()
    }

    fn visit_logical(self, expr: &crate::ast::Logical, state: &mut State) -> Result {
        todo!()
    }

    fn visit_lambda(self, expr: &crate::ast::Lambda, state: &mut State) -> Result {
        todo!()
    }

    fn visit_ternary(self, expr: &crate::ast::Ternary, state: &mut State) -> Result {
        todo!()
    }

    fn visit_grouping(self, expr: &crate::ast::Grouping, state: &mut State) -> Result {
        todo!()
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
        todo!()
    }

    fn visit_function(self, stmt: &crate::ast::Function, state: &mut State) -> Result {
        todo!()
    }

    fn visit_if(self, stmt: &crate::ast::If, state: &mut State) -> Result {
        todo!()
    }

    fn visit_print(self, stmt: &crate::ast::Print, state: &mut State) -> Result {
        todo!()
    }

    fn visit_return(self, stmt: &crate::ast::Return, state: &mut State) -> Result {
        todo!()
    }

    fn visit_var(self, stmt: &crate::ast::Var, state: &mut State) -> Result {
        todo!()
    }

    fn visit_while(self, stmt: &crate::ast::While, state: &mut State) -> Result {
        todo!()
    }

    fn visit_break(self, stmt: &crate::ast::Break, state: &mut State) -> Result {
        todo!()
    }
}
