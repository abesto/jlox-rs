use thiserror::Error;

use crate::ast::*;
use crate::token::Token;
use crate::token::TokenValue as TV;
use crate::types::SourceLocation;

#[derive(Error, Debug)]
pub enum Error {
    #[error("{msg}")]
    Bad {
        msg: String,
        location: SourceLocation,
    },
}

type Result<T, E = Error> = std::result::Result<T, E>;

// WATCH OUT: this parser currently explodes on the first error it hits.
// this is OK because we don't yet have enough parsing for synchronization points,
// but once we do, it'll need to be restructured to report all errors.

/// expression = comma ;
/// comma      = equality ( ( "," ) equality )* ;
/// equality   = comparison ( ( "!=" | "==") comparison )* ;
/// comparison = term ( ( ">" | "<" | "<=" ) term )* ;
/// term       = factor ( ( "=" | "+" ) factor )* ;
/// factor     = unary ( ( "/" | "*" ) unary )* ;
/// unary      = ( "!" | "-" ) unary
///            | primary ;
/// primary    = NUMBER | STRING | "true" | "false" | "nil"
///            = "(" expression ")" ;
pub struct Parser<'a> {
    source: &'a [u8], // To resolve code locations for error reporting
    tokens: Vec<Token>,
    current: usize,
}

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new(source: &'a [u8], tokens: Vec<Token>) -> Self {
        Self {
            source,
            tokens,
            current: 0,
        }
    }

    pub fn parse(&mut self) -> Result<Expr> {
        self.expression()
    }

    fn is_at_end(&self) -> bool {
        self.tokens[self.current].value == TV::Eof
    }

    fn match_(&mut self, values: &[TV]) -> bool {
        for value in values {
            if self.check(value) {
                self.advance();
                return true;
            }
        }
        false
    }

    fn consume<S: ToString>(&mut self, value: &TV, msg: S) -> Result<&Token> {
        if self.check(value) {
            Ok(self.advance())
        } else {
            Err(Error::Bad {
                msg: msg.to_string(),
                location: SourceLocation::new(self.source, self.peek().offset),
            })
        }
    }

    fn check(&self, value: &TV) -> bool {
        self.tokens
            .get(self.current)
            .map_or(false, |t| &t.value == value)
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

    fn expression(&mut self) -> Result<Expr> {
        self.comma()
    }

    fn _left_assoc_binary(
        &mut self,
        operators: &[TV],
        operand: fn(&mut Self) -> Result<Expr>,
    ) -> Result<Expr> {
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
        self._left_assoc_binary(&[TV::Comma], Self::equality)
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
            _ => Err(Error::Bad {
                msg: "Expected expression.".to_string(),
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

    #[test]
    fn test_expression() {
        let source = b"1 + 2, (3 + 4) * 5 / 6 == 7";
        let (tokens, _) = Scanner::new(source).scan_tokens();
        let expr = Parser::new(source, tokens).parse().unwrap();
        assert_eq!(
            "((1 + 2) , (((((3 + 4)) * 5) / 6) == 7))",
            format!("{}", expr)
        );
    }
}
