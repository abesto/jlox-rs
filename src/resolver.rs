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

    #[error("`return` outside function at {location}")]
    ReturnOutsideFunction { location: SourceLocation },

    #[error("Tried to set undeclared variable `{name}` at {location}")]
    DefineUndeclared {
        name: String,
        location: SourceLocation,
    },

    #[error("Variable `{name}` used before initialization at {location}")]
    UseBeforeInit {
        name: String,
        location: SourceLocation,
    },

    #[error("Unused local variable `{name}`, declared at {location}")]
    UnusedLocal {
        name: String,
        location: SourceLocation,
    },

    #[error("`this` outside of a class at {location}")]
    ThisOutsideClass { location: SourceLocation },

    #[error("`this` in static method at {location}")]
    ThisInStaticMethod { location: SourceLocation },

    #[error("Return from initializer of `{class}` at {location}")]
    ReturnFromInitializer {
        class: String,
        location: SourceLocation,
    },

    #[error("Class `{class}` inherits from itself at {location}")]
    SelfInheritance {
        class: String,
        location: SourceLocation,
    },

    #[error("`super` outside of a class at {location}")]
    SuperOutsideClass { location: SourceLocation },

    #[error("`super` in class with no superclass at {location}")]
    SuperWithoutSuperclass { location: SourceLocation },
}

type Output = ();
pub type CommandIndex = usize;

#[derive(Debug, Clone)]
pub struct Binding {
    pub scopes_up: usize,
    pub index_in_scope: usize,
}

impl Binding {
    pub fn less_one_scope(&self) -> Self {
        Self {
            scopes_up: self.scopes_up - 1,
            index_in_scope: self.index_in_scope,
        }
    }
}

pub type Bindings = HashMap<SourceLocation, Binding>;

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

fn combine_many_results<I>(results: I) -> Result
where
    I: IntoIterator<Item = Result>,
{
    results.into_iter().reduce(combine_results).unwrap()
}

/// Are we inside a function-like thing?
/// Used to detect `return`s outside functions.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum FunctionType {
    None,
    Function,
    Lambda,
    Method,
    ClassMethod,
    Initializer(String),
}

impl Default for FunctionType {
    fn default() -> Self {
        Self::None
    }
}

/// Are we inside a class?
/// Used to detect `this`s outside classes.
#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum ClassType {
    None,
    Class,
    Subclass,
}

impl Default for ClassType {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
enum VariableState {
    Declared,
    Defined,
    Used,
}

#[derive(Debug, PartialEq, Eq, PartialOrd, Ord)]
struct VariableData {
    state: VariableState,
    declared_at: SourceLocation,
    index_in_scope: usize,
}

impl VariableData {
    #[must_use]
    fn new(declared_at: SourceLocation, index_in_scope: usize) -> Self {
        Self {
            state: VariableState::Declared,
            declared_at,
            index_in_scope,
        }
    }
}

#[derive(Default)]
struct Scope {
    vars: HashMap<String, VariableData>,
    next_index: usize,
}

impl Scope {
    fn reserve(&mut self) -> usize {
        let n = self.next_index;
        self.next_index += 1;
        n
    }
}

#[derive(Default)]
pub struct ResolverConfig {
    pub error_on_unused_locals: bool,
}

#[derive(Default)]
pub struct Resolver {
    config: ResolverConfig,
    scopes: Vec<Scope>,
    current_function: FunctionType,
    current_class: ClassType,
}

impl Resolver {
    #[must_use]
    pub fn new(config: ResolverConfig) -> Self {
        Self {
            config,
            ..Default::default()
        }
    }

    fn begin_scope(&mut self) {
        self.scopes.push(Default::default());
    }

    fn end_scope(&mut self) -> Result {
        if let Some(scope) = self.scopes.pop() {
            if self.config.error_on_unused_locals {
                let errors: Vec<Error> = scope
                    .vars
                    .iter()
                    .filter(|(_, v)| v.state != VariableState::Used)
                    .map(|(k, v)| Error::UnusedLocal {
                        name: k.clone(),
                        location: v.declared_at,
                    })
                    .collect();
                if !errors.is_empty() {
                    return Err(errors);
                }
            }
        }
        Ok(())
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

    fn resolve_local(&mut self, name: &Token, state: &mut State, is_use: bool) -> Result {
        let scopes_len = self.scopes.len();
        for (i, scope) in self.scopes.iter_mut().enumerate().rev() {
            if let Some(var) = scope.vars.get_mut(&name.lexeme) {
                if var.state == VariableState::Declared {
                    return Err(vec![Error::UseBeforeInit {
                        name: name.lexeme.clone(),
                        location: name.location,
                    }]);
                }

                if is_use && var.state >= VariableState::Defined && var.state < VariableState::Used
                {
                    var.state = VariableState::Used;
                }
                state.insert(
                    name.location,
                    Binding {
                        scopes_up: scopes_len - 1 - i,
                        index_in_scope: var.index_in_scope,
                    },
                );
                return Ok(());
            }
        }
        Ok(())
    }

    fn declare(&mut self, name: &Token) -> Result {
        if let Some(scope) = self.scopes.last_mut() {
            if scope.vars.contains_key(&name.lexeme) {
                return Err(vec![Error::DoubleDeclaration {
                    name: name.lexeme.clone(),
                    location: name.location,
                }]);
            }
            let index_in_scope = scope.reserve();
            scope.vars.insert(
                name.lexeme.clone(),
                VariableData::new(name.location, index_in_scope),
            );
        }
        Ok(())
    }

    fn define(&mut self, name: &Token) -> Result {
        if let Some(scope) = self.scopes.last_mut() {
            if let Some(var) = scope.vars.get_mut(&name.lexeme) {
                if var.state < VariableState::Defined {
                    var.state = VariableState::Defined;
                }
            } else {
                return Err(vec![Error::DefineUndeclared {
                    name: name.lexeme.clone(),
                    location: name.location,
                }]);
            }
        }
        Ok(())
    }

    fn resolve_function(
        &mut self,
        params: &[Token],
        body: &[Stmt],
        function_type: FunctionType,
        state: &mut State,
    ) -> Result {
        let enclosing_function = std::mem::replace(&mut self.current_function, function_type);
        self.begin_scope();

        let mut result = Ok(());
        for param in params {
            result = combine_many_results([
                result,
                self.declare(param),
                self.define(param),
                self.resolve_local(param, state, true),
            ]);
        }
        result = combine_many_results([
            result,
            self.resolve_statements(body, state),
            self.end_scope(),
        ]);

        self.current_function = enclosing_function;

        result
    }
}

impl ExprVisitor<Result, &mut State> for &mut Resolver {
    fn visit_variable(self, expr: &crate::ast::Variable, state: &mut State) -> Result {
        if let Some(scope) = self.scopes.last() {
            if scope.vars.get(&expr.name.lexeme).map(|d| &d.state) == Some(&VariableState::Declared)
            {
                return Err(vec![Error::SelfReferencingInitializer {
                    name: expr.name.lexeme.clone(),
                    location: expr.name.location,
                }]);
            }
        }

        self.resolve_local(&expr.name, state, true)
    }

    fn visit_assign(self, expr: &crate::ast::Assign, state: &mut State) -> Result {
        combine_results(
            expr.value.walk(&mut *self, state),
            self.resolve_local(&expr.name, state, false),
        )
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
        self.resolve_function(&expr.params, &expr.body, FunctionType::Lambda, state)
    }

    fn visit_ternary(self, expr: &crate::ast::Ternary, state: &mut State) -> Result {
        combine_many_results(
            [&expr.left, &expr.mid, &expr.right]
                .into_iter()
                .map(|expr| expr.walk(&mut *self, state)),
        )
    }

    fn visit_grouping(self, expr: &crate::ast::Grouping, state: &mut State) -> Result {
        expr.expr.walk(self, state)
    }

    fn visit_get(self, expr: &crate::ast::Get, state: &mut State) -> Result {
        expr.object.walk(self, state)
    }

    fn visit_set(self, expr: &crate::ast::Set, state: &mut State) -> Result {
        combine_results(
            expr.value.walk(&mut *self, state),
            expr.object.walk(self, state),
        )
    }

    fn visit_this(self, expr: &crate::ast::This, state: &mut State) -> Result {
        if self.current_class == ClassType::None {
            Err(vec![Error::ThisOutsideClass {
                location: expr.keyword.location,
            }])
        } else if self.current_function == FunctionType::ClassMethod {
            Err(vec![Error::ThisInStaticMethod {
                location: expr.keyword.location,
            }])
        } else {
            self.resolve_local(&expr.keyword, state, true)
        }
    }

    fn visit_super(self, expr: &crate::ast::Super, state: &mut State) -> Result {
        let mut result = Ok(());
        if self.current_class == ClassType::None {
            result = combine_results(
                result,
                Err(vec![Error::SuperOutsideClass {
                    location: expr.keyword.location,
                }]),
            );
        } else if self.current_class == ClassType::Class {
            result = combine_results(
                result,
                Err(vec![Error::SuperWithoutSuperclass {
                    location: expr.keyword.location,
                }]),
            );
        }
        combine_many_results([
            result,
            self.resolve_local(&expr.keyword, state, true),
            self.resolve_local(&expr.method, state, true),
        ])
    }
}

impl StmtVisitor<Result, &mut State> for &mut Resolver {
    fn visit_block(self, stmt: &crate::ast::Block, state: &mut State) -> Result {
        self.begin_scope();
        combine_results(
            self.resolve_statements(&stmt.statements, state),
            self.end_scope(),
        )
    }

    fn visit_expression(self, stmt: &crate::ast::Expression, state: &mut State) -> Result {
        stmt.expr.walk(self, state)
    }

    fn visit_function(self, stmt: &crate::ast::Function, state: &mut State) -> Result {
        combine_many_results([
            self.declare(&stmt.name),
            self.define(&stmt.name),
            self.resolve_function(&stmt.params, &stmt.body, FunctionType::Function, state),
            self.resolve_local(&stmt.name, state, false),
        ])
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
        if self.current_function == FunctionType::None {
            return Err(vec![Error::ReturnOutsideFunction {
                location: stmt.keyword.location,
            }]);
        }

        if let Some(expr) = &stmt.value {
            if let FunctionType::Initializer(class) = &self.current_function {
                return Err(vec![Error::ReturnFromInitializer {
                    class: class.clone(),
                    location: stmt.keyword.location,
                }]);
            }
            expr.walk(self, state)
        } else {
            Ok(())
        }
    }

    fn visit_var(self, stmt: &crate::ast::Var, state: &mut State) -> Result {
        combine_many_results([
            self.declare(&stmt.name),
            if let Some(initializer) = &stmt.initializer {
                initializer.walk(&mut *self, state)
            } else {
                Ok(())
            },
            self.define(&stmt.name),
            self.resolve_local(&stmt.name, state, false),
        ])
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

    fn visit_class(self, stmt: &crate::ast::Class, state: &mut State) -> Result {
        let enclosing_class = std::mem::replace(&mut self.current_class, ClassType::Class);

        let mut result = combine_many_results([
            self.declare(&stmt.name),
            self.define(&stmt.name),
            self.resolve_local(&stmt.name, state, false),
        ]);

        if let Some(superclass) = &stmt.superclass {
            self.current_class = ClassType::Subclass;
            if superclass.1.name.lexeme == stmt.name.lexeme {
                result = combine_results(
                    result,
                    Err(vec![Error::SelfInheritance {
                        class: stmt.name.lexeme.clone(),
                        location: superclass.1.name.location,
                    }]),
                );
            }

            result = combine_results(
                result,
                crate::ast::Expr::Variable(superclass.1.clone()).walk(&mut *self, state),
            );

            self.begin_scope();
            let scope = self.scopes.last_mut().unwrap();
            let index_in_scope = scope.reserve();
            scope.vars.insert(
                "super".to_string(),
                VariableData {
                    state: VariableState::Used,
                    declared_at: superclass.0.location,
                    index_in_scope,
                },
            );
            state.insert(
                superclass.0.location,
                Binding {
                    scopes_up: 0,
                    index_in_scope,
                },
            );
        }

        self.begin_scope();
        {
            let scope = self.scopes.last_mut().unwrap();
            let index_in_scope = scope.reserve();
            scope.vars.insert(
                "this".to_string(),
                VariableData {
                    state: VariableState::Used,
                    declared_at: stmt.left_brace.location,
                    index_in_scope,
                },
            );
            state.insert(
                stmt.left_brace.location,
                Binding {
                    scopes_up: 0,
                    index_in_scope,
                },
            );
        }

        for method in &stmt.methods {
            let declaration = if method.name.lexeme == "init" {
                FunctionType::Initializer(stmt.name.lexeme.clone())
            } else {
                FunctionType::Method
            };
            result = combine_results(
                result,
                self.resolve_function(&method.params, &method.body, declaration, state),
            );
        }

        for class_method in &stmt.class_methods {
            result = combine_results(
                result,
                self.resolve_function(
                    &class_method.params,
                    &class_method.body,
                    FunctionType::ClassMethod,
                    state,
                ),
            );
        }

        for getter in &stmt.getters {
            result = combine_results(
                result,
                self.resolve_function(&getter.params, &getter.body, FunctionType::Function, state),
            );
        }

        if stmt.superclass.is_some() {
            result = combine_results(result, self.end_scope());
        }

        result = combine_results(result, self.end_scope());
        self.current_class = enclosing_class;
        result
    }
}
