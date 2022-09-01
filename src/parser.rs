use thiserror::Error;

use crate::ast::*;
use crate::token::Token;
use crate::token::TokenValue as TV;
use crate::types::SourceLocation;

#[derive(Error, Debug)]
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
}

type Result<T, E = Error> = std::result::Result<T, E>;

/// program      = declaration* EOF ;
///
/// declaration  = varDecl
///              | statement ;
/// varDecl      = "var" IDENTIFIER ( "=" expression )? ";" ;
///
/// statement    = exprStmt
///              | ifStmt
///              | printStmt
///              | block ;
/// exprStmt     = expression ";" ;
/// ifStmt       = "if" "(" expression ")" statement
///                ( "else" statement )? ;
/// printStmt    = "print" expression ";" ;
/// block        = "{" declaration "}" ;
///
/// expression   = comma ;
/// comma        = assignment ( ( "," ) assignment )* ;
/// assignment   = IDENTIFIER "=" assignment
///              | ternary ;
/// ternary      = equality ( "?" expression ":" expression )*;
/// equality     = comparison ( ( "!=" | "==" ) comparison )*
/// comparison   = term ( ( ">" | "<" | "<=" | ">=" ) term )* ;
/// term         = factor ( ( "-" | "+" ) factor )* ;
/// factor       = unary ( ( "/" | "*" ) unary )* ;
/// unary        = ( "!" | "-" ) unary
///              | primary ;
/// primary      = NUMBER | STRING | "true" | "false" | "nil"
///              | "(" expression ")"
///              | IDENTIFIER ;
pub struct Parser<'a> {
    source: &'a [u8], // To resolve code locations for error reporting
    tokens: Vec<Token>,
    errors: Vec<Error>,
    current: usize,
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

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new(source: &'a [u8], tokens: Vec<Token>) -> Self {
        Self {
            source,
            tokens,
            errors: vec![],
            current: 0,
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
                location: SourceLocation::new(self.source, self.peek().offset),
            })
        }
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
        let res = if self.match_(&[TV::Var]) {
            self.var_declaration()
        } else {
            self.statement()
        };
        if res.is_err() {
            self.synchronize();
        }
        res
    }

    fn var_declaration(&mut self) -> Result<Stmt> {
        let name = self
            .consume(
                &|t: &Token| matches!(t.value, TV::Identifier(_)),
                "Expected variable name",
            )?
            .clone();
        let initializer = if self.match_(&[TV::Equal]) {
            Some(Box::new(self.expression()?))
        } else {
            None
        };
        self.consume(&TV::Semicolon, "Expected `;` after variable declaration")?;

        Ok(Stmt::Var(Var { name, initializer }))
    }

    fn statement(&mut self) -> Result<Stmt> {
        if self.match_(&[TV::Print]) {
            self.print_statement()
        } else if self.match_(&[TV::LeftBrace]) {
            self.block()
        } else if self.match_(&[TV::If]) {
            self.if_statement()
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
                location: SourceLocation::new(self.source, self.previous().offset),
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
            } else {
                Err(Error::InvalidAssignmentTarget {
                    target: expr,
                    location: SourceLocation::new(self.source, start),
                })
            }
        } else {
            Ok(expr)
        }
    }

    fn ternary(&mut self) -> Result<Expr> {
        let mut children = vec![self.equality()?];

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
            self.primary()
        }
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
                self.consume(&TV::RightParen, "Expect ')' after expression.")?;
                Ok(Expr::Grouping(Grouping {
                    expr: Box::new(expr),
                }))
            }
            TV::Identifier(_) => Ok(Expr::Variable(Variable {
                name: self.previous().clone(),
            })),
            t => Err(Error::Bad {
                msg: format!("Expected expression, found: `{}`", t),
                location: SourceLocation::new(self.source, self.previous().offset),
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

    fn expr_parses_to(input: &str, expected: &str) {
        let tokens = Scanner::new(format!("{};", input).as_bytes())
            .scan_tokens()
            .unwrap();
        let statements = Parser::new(input.as_bytes(), tokens).parse().unwrap();
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
        let input = "+ 3 (1 + 2) > /4 (< 1)";
        let tokens = Scanner::new(input.as_bytes()).scan_tokens().unwrap();
        let errors = Parser::new(input.as_bytes(), tokens).parse().err().unwrap();
        assert_eq!(
            vec![
                "LHS missing for `+` at 0:0".to_string(),
                "LHS missing for `/` at 0:14".to_string(),
                "LHS missing for `<` at 0:18".to_string(),
                "Expected expression, found: `)` at 0:21".to_string()
            ],
            errors
                .iter()
                .map(|e| e.to_string())
                .collect::<Vec<String>>()
        );
    }
}
