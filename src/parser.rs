use macros::ResolveErrorLocation;
use thiserror::Error;

use crate::ast::*;
use crate::token::Token;
use crate::token::TokenValue as TV;
use crate::types::SourceLocation;

#[derive(Error, Debug, ResolveErrorLocation)]
pub enum Error {
    #[error("{msg} at {location}")]
    Bad {
        msg: String,
        location: SourceLocation,
    },

    #[error("LHS missing for `{operator}` at {location}")]
    MissingLhs {
        operator: String,
        location: SourceLocation,
    },

    #[error("Invalid assignment target at {location}: `{target}`")]
    InvalidAssignmentTarget {
        target: Expr,
        location: SourceLocation,
    },

    #[error("`break` outside loop at {location}")]
    BreakOutsideLoop { location: SourceLocation },

    #[error("Can't have more than 255 arguments. Call site: {location}")]
    TooManyArguments { location: SourceLocation },
}

type Result<T, E = Error> = std::result::Result<T, E>;

/// program      = declaration* EOF ;
///
/// declaration  = classDecl
///              | funDecl
///              | varDecl
///              | statement ;
/// classDecl    = "class" IDENTIFIER "{" function* "}" ;
/// funDecl      = "fun" function ;
/// function     = IDENTIFIER "(" parameters? ")" block ;
/// parameters   = IDENTIFIER ( "," IDENTIFIER )* ;
/// varDecl      = "var" IDENTIFIER ( "=" expression )? ";" ;
///
/// statement    = exprStmt
///              | ifStmt
///              | returnStmt
///              | printStmt
///              | whileStmt
///              | forStmt
///              | breakStmt
///              | block ;
/// exprStmt     = expression ";" ;
/// ifStmt       = "if" "(" expression ")" statement
///                ( "else" statement )? ;
/// printStmt    = "print" expression ";" ;
/// whileStmt    = "while" "(" expression ")" statement ;
/// forStmt      = "for" "("
///                    (varDecl | exprStmt | ";")
///                    expression? ";"
///                    expression?
///                ")" statement ;
/// breakStmt    = "break" ;
/// block        = "{" declaration "}" ;
///
/// expression   = comma ;
/// comma        = assignment ( ( "," ) assignment )* ;
/// assignment   = "fun" lambda
///              | ( call "." )? IDENTIFIER "=" assignment
///              | ternary ;
/// lambda       = "(" parameters? ")" block ;
/// ternary      = logic_or ( "?" expression ":" expression )* ;
/// logic_or     = logic_and ( "or" logic_and )* ;
/// logic_and    = equality ( "and" equality )* ;
/// equality     = comparison ( ( "!=" | "==" ) comparison )*
/// comparison   = term ( ( ">" | "<" | "<=" | ">=" ) term )* ;
/// term         = factor ( ( "-" | "+" ) factor )* ;
/// factor       = unary ( ( "/" | "*" ) unary )* ;
/// unary        = ( "!" | "-" ) unary
///              | call ;
/// call         = primary ( "(" arguments ")" | "." IDENTIFIER )* ;
/// arguments    = expression ( "," expression )* ;
/// primary      = NUMBER | STRING | "true" | "false" | "nil"
///              | "(" expression ")"
///              | IDENTIFIER ;
pub struct Parser {
    tokens: Vec<Token>,
    errors: Vec<Error>,
    current: usize,
    loop_depth: usize, // Used to raise a syntax error on `break` outside a loop
}

trait TokenPred {
    fn matches(&self, token: &Token) -> bool;
}

impl TokenPred for TV {
    fn matches(&self, token: &Token) -> bool {
        &token.value == self
    }
}

impl<F> TokenPred for F
where
    F: Fn(&Token) -> bool,
{
    fn matches(&self, token: &Token) -> bool {
        self(token)
    }
}

impl Parser {
    #[must_use]
    pub fn new(tokens: Vec<Token>) -> Self {
        Self {
            tokens,
            errors: vec![],
            current: 0,
            loop_depth: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Vec<Stmt>, Vec<Error>> {
        let mut statements = vec![];

        while !self.is_at_end() {
            match self.declaration() {
                Ok(s) => {
                    statements.push(s);
                }
                Err(e) => {
                    self.errors.push(e);
                    return Err(std::mem::take(&mut self.errors));
                }
            }
        }

        if self.errors.is_empty() {
            Ok(statements)
        } else {
            Err(std::mem::take(&mut self.errors))
        }
    }

    fn is_at_end(&self) -> bool {
        self.tokens[self.current].value == TV::Eof
    }

    fn match_(&mut self, preds: &[impl TokenPred]) -> bool {
        for pred in preds {
            if self.check(pred) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume<S: ToString>(&mut self, pred: &impl TokenPred, msg: S) -> Result<&Token> {
        if self.check(pred) {
            Ok(self.advance())
        } else {
            Err(Error::Bad {
                msg: msg.to_string(),
                location: self.peek().location,
            })
        }
    }

    fn consume_identifier<S: ToString>(&mut self, msg: S) -> Result<&Token> {
        self.consume(&|t: &Token| matches!(t.value, TV::Identifier(_)), msg)
    }

    fn check(&self, pred: &impl TokenPred) -> bool {
        self.tokens
            .get(self.current)
            .map_or(false, |t| pred.matches(t))
    }

    fn advance(&mut self) -> &Token {
        if !self.is_at_end() {
            self.current += 1;
        }
        self.previous()
    }

    fn previous(&self) -> &Token {
        &self.tokens[self.current - 1]
    }

    fn peek(&self) -> &Token {
        &self.tokens[self.current]
    }

    fn declaration(&mut self) -> Result<Stmt> {
        let res = if self.match_(&[TV::Fun]) {
            self.function_statement()
        } else if self.match_(&[TV::Class]) {
            self.class_declaration()
        } else if self.match_(&[TV::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        };
        if res.is_err() {
            self.synchronize();
        }
        res
    }

    fn function_statement(&mut self) -> Result<Stmt> {
        if !self.check(&|t: &Token| matches!(t.value, TV::Identifier(_))) {
            let lambda = self.lambda()?;
            self.consume(
                &TV::Semicolon,
                "Expected `;` after anonymous function expression statement",
            )?;
            return Ok(Stmt::Expression(Expression { expr: lambda }));
        }

        Ok(Stmt::Function(self.function("function")?))
    }

    fn function(&mut self, kind: &str) -> Result<Function> {
        let name = self
            .consume_identifier(format!("Expected {} name", kind))?
            .clone();
        self.consume(&TV::LeftParen, format!("Expected `(` after {} name", kind))?;

        let params = self.parameters(name.location)?;
        let body = self.body(kind)?;
        Ok(Function { name, params, body })
    }

    fn parameters(&mut self, location: SourceLocation) -> Result<Vec<Token>> {
        let mut params = vec![];
        if !self.check(&TV::RightParen) {
            loop {
                if params.len() >= 255 {
                    return Err(Error::TooManyArguments { location });
                }
                params.push(
                    self.consume(
                        &|t: &Token| matches!(t.value, TV::Identifier(_)),
                        "Expected parameter name",
                    )?
                    .clone(),
                );

                if !self.match_(&[TV::Comma]) {
                    break;
                }
            }
        }
        self.consume(&TV::RightParen, "Expected `)` after parameters")?;
        Ok(params)
    }

    fn body(&mut self, kind: &str) -> Result<Vec<Stmt>> {
        self.consume(
            &TV::LeftBrace,
            format!("Expected `{{` before {} body", kind),
        )?;
        match self.block()? {
            Stmt::Block(Block { statements }) => Ok(statements),
            _ => {
                unreachable!("Internal error: `Parser::block` somehow returned NOT a `Stmt::Block`")
            }
        }
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        let name = self.consume_identifier("Expected variable name")?.clone();
        let initializer = if self.match_(&[TV::Equal]) {
            Some(Box::new(self.expression()?))
        } else {
            None
        };
        self.consume(&TV::Semicolon, "Expected `;` after variable declaration")?;

        Ok(Stmt::Var(Var { name, initializer }))
    }

    fn class_declaration(&mut self) -> Result<Stmt> {
        let name = self.consume_identifier("Expected class name")?.clone();
        self.consume(&TV::LeftBrace, "Expected `{` before class body")?;

        let mut methods = vec![];
        while !self.check(&TV::RightBrace) && !self.is_at_end() {
            methods.push(self.function("method")?);
        }

        self.consume(&TV::RightBrace, "Expected `}` after class body")?;
        Ok(Stmt::Class(Class { name, methods }))
    }

    fn statement(&mut self) -> Result<Stmt> {
        if self.match_(&[TV::Print]) {
            self.print_statement()
        } else if self.match_(&[TV::LeftBrace]) {
            self.block()
        } else if self.match_(&[TV::If]) {
            self.if_statement()
        } else if self.match_(&[TV::Return]) {
            self.return_statement()
        } else if self.match_(&[TV::While]) {
            self.while_statement()
        } else if self.match_(&[TV::For]) {
            self.for_statement()
        } else if self.match_(&[TV::Break]) {
            self.break_statement()
        } else {
            self.expression_statement()
        }
    }

    fn print_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume(&TV::Semicolon, "Expected `;` after value")?;
        Ok(Stmt::Print(Print { expr }))
    }

    fn block(&mut self) -> Result<Stmt> {
        let mut statements = vec![];
        while !self.check(&TV::RightBrace) && !self.is_at_end() {
            statements.push(self.declaration()?);
        }
        self.consume(&TV::RightBrace, "Expected `}` after block")?;

        Ok(Stmt::Block(Block { statements }))
    }

    fn if_statement(&mut self) -> Result<Stmt> {
        self.consume(&TV::LeftParen, "Expected `(` after `if`")?;
        let condition = self.expression()?;
        self.consume(&TV::RightParen, "Expected `)` after `if` condition")?;
        let then_branch = self.statement()?;
        let else_branch = if self.match_(&[TV::Else]) {
            Some(self.statement()?)
        } else {
            None
        };
        Ok(Stmt::If(If {
            condition: Box::new(condition),
            then_branch: Box::new(then_branch),
            else_branch: else_branch.map(Box::new),
        }))
    }

    fn return_statement(&mut self) -> Result<Stmt> {
        let keyword = self.previous().clone();
        let value = if self.check(&TV::Semicolon) {
            None
        } else {
            Some(self.expression()?)
        };

        self.consume(&TV::Semicolon, "Expected `;` after return value")?;
        Ok(Stmt::Return(Return {
            keyword,
            value: value.map(Box::new),
        }))
    }

    fn while_statement(&mut self) -> Result<Stmt> {
        self.consume(&TV::LeftParen, "Expected `(` after `while`")?;
        let condition = self.expression()?;
        self.consume(&TV::RightParen, "Expected `)` after `while` condition")?;

        self.loop_depth += 1;
        let statement = self.statement()?;
        self.loop_depth -= 1;

        Ok(Stmt::While(While {
            condition: Box::new(condition),
            statement: Box::new(statement),
        }))
    }

    /// Desugar to `while` loop
    fn for_statement(&mut self) -> Result<Stmt> {
        self.consume(&TV::LeftParen, "Expected `(` after `for`")?;

        let initializer = if self.match_(&[TV::Semicolon]) {
            None
        } else if self.match_(&[TV::Var]) {
            Some(self.var_declaration()?)
        } else {
            Some(self.expression_statement()?)
        };

        let condition = if !self.check(&TV::Semicolon) {
            self.expression()?
        } else {
            Expr::Literal(Literal::True)
        };
        self.consume(&TV::Semicolon, "Expected `;` after `for` condition")?;

        let increment = if !self.check(&TV::RightParen) {
            Some(self.expression()?)
        } else {
            None
        };
        self.consume(&TV::RightParen, "Expected `)` after `for` clauses")?;

        self.loop_depth += 1;
        let mut body = self.statement()?;
        self.loop_depth -= 1;

        if let Some(increment) = increment {
            body = Stmt::Block(Block {
                statements: vec![body, Stmt::Expression(Expression { expr: increment })],
            });
        }
        body = Stmt::While(While {
            condition: Box::new(condition),
            statement: Box::new(body),
        });
        if let Some(initializer) = initializer {
            body = Stmt::Block(Block {
                statements: vec![initializer, body],
            });
        }

        Ok(body)
    }

    fn break_statement(&mut self) -> Result<Stmt> {
        if self.loop_depth == 0 {
            Err(Error::BreakOutsideLoop {
                location: SourceLocation::new(self.previous().location.command(), self.current),
            })
        } else {
            self.consume(&TV::Semicolon, "Expected `;` after `break`")?;
            Ok(Stmt::Break(Break {
                token: self.previous().clone(),
            }))
        }
    }

    fn expression_statement(&mut self) -> Result<Stmt> {
        let expr = self.expression()?;
        self.consume(&TV::Semicolon, "Expected `;` after expression")?;
        Ok(Stmt::Expression(Expression { expr }))
    }

    fn expression(&mut self) -> Result<Expr> {
        self.comma()
    }

    /// Error production: missing LHS for binary operator
    fn _missing_lhs(
        &mut self,
        operators: &[TV],
        operand: fn(&mut Self) -> Result<Expr>,
    ) -> Result<()> {
        if self.match_(operators) {
            self.errors.push(Error::MissingLhs {
                operator: self.previous().lexeme.clone(),
                location: self.previous().location,
            });
            // Skip RHS
            operand(self)?;
        }
        Ok(())
    }

    fn _left_assoc_binary(
        &mut self,
        operators: &[TV],
        operand: fn(&mut Self) -> Result<Expr>,
    ) -> Result<Expr> {
        self._missing_lhs(operators, operand)?;
        let mut expr = operand(self)?;

        while self.match_(operators) {
            let operator = self.previous().clone();
            let right = operand(self)?;
            expr = Expr::Binary(Binary {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn comma(&mut self) -> Result<Expr> {
        self._left_assoc_binary(&[TV::Comma], Self::assignment)
    }

    fn assignment(&mut self) -> Result<Expr> {
        if self.match_(&[TV::Fun]) {
            return self.lambda();
        }

        let start = self.current;
        let expr = self.ternary()?;

        if self.match_(&[TV::Equal]) {
            let value = self.assignment()?;

            if let Expr::Variable(v) = expr {
                let name = v.name;
                Ok(Expr::Assign(Assign {
                    name,
                    value: Box::new(value),
                }))
            } else if let Expr::Get(Get { object, name }) = expr {
                Ok(Expr::Set(Set {
                    object,
                    name,
                    value: Box::new(value),
                }))
            } else {
                Err(Error::InvalidAssignmentTarget {
                    target: expr,
                    location: SourceLocation::new(self.previous().location.command(), start),
                })
            }
        } else {
            Ok(expr)
        }
    }

    fn lambda(&mut self) -> Result<Expr> {
        let token = self.previous().clone();
        self.consume(&TV::LeftParen, "Expected `(` after anonymous `fun`")?;
        let params =
            self.parameters(SourceLocation::new(token.location.command(), self.current))?;
        let body = self.body("lambda")?;
        Ok(Expr::Lambda(Lambda {
            token,
            params,
            body,
        }))
    }

    fn ternary(&mut self) -> Result<Expr> {
        let mut children = vec![self.or()?];

        while self.match_(&[TV::Question]) {
            children.push(self.expression()?);
            self.consume(&TV::Colon, ": expected")?;
            children.push(self.expression()?);
        }

        if children.len() == 1 {
            return Ok(children.pop().unwrap());
        }

        let mut expr = Expr::Ternary(Ternary {
            right: Box::new(children.pop().unwrap()),
            mid: Box::new(children.pop().unwrap()),
            left: Box::new(children.pop().unwrap()),
        });

        while !children.is_empty() {
            expr = Expr::Ternary(Ternary {
                mid: Box::new(children.pop().unwrap()),
                left: Box::new(children.pop().unwrap()),
                right: Box::new(expr),
            });
        }

        Ok(expr)
    }

    fn or(&mut self) -> Result<Expr> {
        let mut expr = self.and()?;

        while self.match_(&[TV::Or]) {
            let operator = self.previous().clone();
            let right = self.and()?;
            expr = Expr::Logical(Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn and(&mut self) -> Result<Expr> {
        let mut expr = self.equality()?;

        while self.match_(&[TV::And]) {
            let operator = self.previous().clone();
            let right = self.equality()?;
            expr = Expr::Logical(Logical {
                left: Box::new(expr),
                operator,
                right: Box::new(right),
            });
        }

        Ok(expr)
    }

    fn equality(&mut self) -> Result<Expr> {
        self._left_assoc_binary(&[TV::BangEqual, TV::EqualEqual], Self::comparison)
    }

    fn comparison(&mut self) -> Result<Expr> {
        self._left_assoc_binary(
            &[TV::Greater, TV::GreaterEqual, TV::Less, TV::LessEqual],
            Self::term,
        )
    }

    fn term(&mut self) -> Result<Expr> {
        self._left_assoc_binary(&[TV::Minus, TV::Plus], Self::factor)
    }

    fn factor(&mut self) -> Result<Expr> {
        self._left_assoc_binary(&[TV::Slash, TV::Star], Self::unary)
    }

    fn unary(&mut self) -> Result<Expr> {
        if self.match_(&[TV::Bang, TV::Minus]) {
            let operator = self.previous().clone();
            let right = self.unary()?;
            Ok(Expr::Unary(Unary {
                operator,
                right: Box::new(right),
            }))
        } else {
            self.call()
        }
    }

    fn call(&mut self) -> Result<Expr> {
        let mut expr = self.primary()?;

        loop {
            if self.match_(&[TV::LeftParen]) {
                expr = self.finish_call(expr)?;
            } else if self.match_(&[TV::Dot]) {
                let name = self
                    .consume_identifier("Expected property name after `.`")?
                    .clone();
                expr = Expr::Get(Get {
                    name,
                    object: Box::new(expr),
                })
            } else {
                break;
            }
        }

        Ok(expr)
    }

    fn finish_call(&mut self, callee: Expr) -> Result<Expr> {
        let mut arguments = vec![];
        let start = self.current;
        if !self.check(&TV::RightParen) {
            loop {
                if arguments.len() >= 255 {
                    return Err(Error::TooManyArguments {
                        location: SourceLocation::new(self.previous().location.command(), start),
                    });
                }
                arguments.push(self.assignment()?);
                if !self.match_(&[TV::Comma]) {
                    break;
                }
            }
        }
        let paren = self.consume(&TV::RightParen, "Expected `)` after arguments.")?;
        Ok(Expr::Call(Call {
            callee: Box::new(callee),
            arguments,
            closing_paren: paren.clone(),
        }))
    }

    fn primary(&mut self) -> Result<Expr> {
        self.advance();
        match &self.previous().value {
            TV::False => Ok(Expr::Literal(Literal::False)),
            TV::True => Ok(Expr::Literal(Literal::True)),
            TV::Nil => Ok(Expr::Literal(Literal::Nil)),
            TV::Number(n) => Ok(Expr::Literal(Literal::Number(*n))),
            TV::String(s) => Ok(Expr::Literal(Literal::String(s.clone()))),
            TV::LeftParen => {
                let expr = self.expression()?;
                self.consume(&TV::RightParen, "Expected `)` after expression.")?;
                Ok(Expr::Grouping(Grouping {
                    expr: Box::new(expr),
                }))
            }
            TV::Identifier(_) => Ok(Expr::Variable(Variable {
                name: self.previous().clone(),
            })),
            t => Err(Error::Bad {
                msg: format!("Expected expression, found: `{}`", t),
                location: self.previous().location,
            }),
        }
    }

    fn synchronize(&mut self) {
        self.advance();

        while !self.is_at_end() {
            if self.previous().value == TV::Semicolon {
                return;
            }

            if matches!(
                self.peek().value,
                TV::Class
                    | TV::Fun
                    | TV::Var
                    | TV::For
                    | TV::If
                    | TV::While
                    | TV::Print
                    | TV::Return
            ) {
                return;
            }

            self.advance();
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::scanner::Scanner;
    use crate::types::ResolveErrorLocation;

    fn expr_parses_to(input: &str, expected: &str) {
        let tokens = Scanner::new(format!("{};", input).as_bytes(), 0)
            .scan_tokens()
            .unwrap();
        let statements = Parser::new(tokens).parse().unwrap();
        let expr = match &statements[0] {
            Stmt::Expression(Expression { expr }) => expr,
            _ => unreachable!(),
        };
        assert_eq!(expected, format!("{}", expr));
    }

    #[test]
    fn test_expression() {
        expr_parses_to(
            "1 + 2, (3 + 4) * 5 / 6 == 7",
            "((1 + 2) , (((((3 + 4)) * 5) / 6) == 7))",
        );
    }

    #[test]
    fn test_math_left_assoc() {
        expr_parses_to("1 + 2 - 3 + 4", "(((1 + 2) - 3) + 4)");
        expr_parses_to("1 * 2 / 3 * 4", "(((1 * 2) / 3) * 4)");
    }

    #[test]
    fn test_equality_right_assoc() {
        expr_parses_to("1 == 2 != 3 == 4", "(((1 == 2) != 3) == 4)");
    }

    #[test]
    fn test_comparison_right_assoc() {
        expr_parses_to("1 < 2 <= 3 > 4 >= 5", "((((1 < 2) <= 3) > 4) >= 5)");
    }

    #[test]
    fn test_ternary() {
        expr_parses_to("1 < 2 ? 3 + 4 : 5 + 6", "((1 < 2) ? ((3 + 4)) : (5 + 6))");
    }

    #[test]
    fn test_ternary_right_assoc() {
        expr_parses_to("0 ? 1 + 2 : 2 ? 3 : 4", "(0 ? ((1 + 2)) : (2 ? (3) : 4))");
    }

    #[test]
    fn test_missing_lhs() {
        let input = "+ 3;";
        let tokens = Scanner::new(input.as_bytes(), 0).scan_tokens().unwrap();
        let mut errors = Parser::new(tokens).parse().err().unwrap();
        assert_eq!(
            vec![
                "LHS missing for `+` at 0:0".to_string(),
                "Expected expression, found: `;` at 0:3".to_string()
            ],
            errors
                .iter_mut()
                .map(|e| {
                    e.resolve(input.as_bytes());
                    e.to_string()
                })
                .collect::<Vec<String>>()
        );
    }
}
