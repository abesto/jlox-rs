use thiserror::Error;

use crate::token::Token;
use crate::token::TokenValue;
use crate::types::Number;
use crate::types::SourceIndex;
use crate::types::SourceLocation;

#[derive(Error, Debug)]
pub enum Error {
    #[error("Invalid UTF-8 character at {location}")]
    InvalidUtf8Char { location: SourceLocation },

    #[error("Unexpected character `{c}` at {location}")]
    UnexpectedCharacter { c: char, location: SourceLocation },

    #[error("Unterminated string starting at {start}")]
    UnterminatedString { start: SourceLocation },

    #[error("Unterminated /* block comment */ starting at {start}")]
    UnterminatedComment { start: SourceLocation },
}

pub struct Scanner<'a> {
    source: &'a [u8],
    start: SourceIndex,
    current: SourceIndex,
}

impl<'a> Scanner<'a> {
    pub fn new(source: &'a [u8]) -> Self {
        Scanner {
            source,
            start: 0,
            current: 0,
        }
    }

    pub fn scan_tokens(&mut self) -> Result<Vec<Token>, Vec<Error>> {
        let mut tokens = vec![];
        let mut errors = vec![];

        while !self.is_at_end() {
            match self
                .scan_token()
                .and_then(|ov| ov.map(|v| self.make_token(v)).transpose())
            {
                Ok(Some(token)) => tokens.push(token),
                Ok(None) => {}
                Err(e) => errors.push(e),
            };
        }

        tokens.push(Token {
            value: TokenValue::Eof,
            lexeme: String::new(),
            offset: self.current,
        });

        if errors.is_empty() {
            Ok(tokens)
        } else {
            Err(errors)
        }
    }

    fn is_at_end(&self) -> bool {
        self.current >= self.source.len()
    }

    fn advance(&mut self) -> u8 {
        let c = self.source[self.current];
        self.current += 1;
        c
    }

    fn match_(&mut self, c: u8) -> bool {
        if self.is_at_end() {
            return false;
        }
        if self.peek() != c {
            return false;
        }
        self.current += 1;
        true
    }

    fn peek_offset(&self, offset: SourceIndex) -> u8 {
        *self.source.get(self.current + offset).unwrap_or(&b'\0')
    }

    fn peek(&self) -> u8 {
        self.peek_offset(0)
    }

    fn peek_next(&self) -> u8 {
        self.peek_offset(1)
    }

    fn substring(&self, start: SourceIndex, end: SourceIndex) -> Result<String, Error> {
        String::from_utf8(self.source[start..end].to_vec()).map_err(|source| {
            Error::InvalidUtf8Char {
                location: SourceLocation::new(
                    self.source,
                    start + source.utf8_error().valid_up_to(),
                ),
            }
        })
    }

    fn make_token(&mut self, value: TokenValue) -> Result<Token, Error> {
        Ok(Token {
            value,
            lexeme: self.substring(self.start, self.current)?,
            offset: self.start,
        })
    }

    fn string(&mut self) -> Result<TokenValue, Error> {
        while self.peek() != b'"' && !self.is_at_end() {
            self.advance();
        }

        if self.is_at_end() {
            return Err(Error::UnterminatedString {
                start: SourceLocation::new(self.source, self.start),
            });
        }

        // Closing "
        self.advance();

        Ok(TokenValue::String(
            self.substring(self.start + 1, self.current - 1)?,
        ))
    }

    fn number(&mut self) -> Result<TokenValue, Error> {
        while self.peek().is_ascii_digit() {
            self.advance();
        }

        // Optional fractional part
        if self.peek() == b'.' && self.peek_next().is_ascii_digit() {
            self.advance();
            while self.peek().is_ascii_digit() {
                self.advance();
            }
        }

        Ok(TokenValue::Number(
            self.substring(self.start, self.current)?
                .parse::<Number>()
                .expect("Weird, I'm super sure this ought to be a valid f64"),
        ))
    }

    fn identifier(&mut self) -> Result<String, Error> {
        while self.peek().is_ascii_alphanumeric() || self.peek() == b'_' {
            self.advance();
        }
        self.substring(self.start, self.current)
    }

    fn scan_token(&mut self) -> Result<Option<TokenValue>, Error> {
        self.start = self.current;
        match self.advance() {
            b'(' => Ok(Some(TokenValue::LeftParen)),
            b')' => Ok(Some(TokenValue::RightParen)),
            b'{' => Ok(Some(TokenValue::LeftBrace)),
            b'}' => Ok(Some(TokenValue::RightBrace)),
            b',' => Ok(Some(TokenValue::Comma)),
            b'.' => Ok(Some(TokenValue::Dot)),
            b'-' => Ok(Some(TokenValue::Minus)),
            b'+' => Ok(Some(TokenValue::Plus)),
            b';' => Ok(Some(TokenValue::Semicolon)),
            b'*' => Ok(Some(TokenValue::Star)),
            b'?' => Ok(Some(TokenValue::Question)),
            b':' => Ok(Some(TokenValue::Colon)),

            b'!' => Ok(Some(if self.match_(b'=') {
                TokenValue::BangEqual
            } else {
                TokenValue::Bang
            })),

            b'=' => Ok(Some(if self.match_(b'=') {
                TokenValue::EqualEqual
            } else {
                TokenValue::Equal
            })),

            b'<' => Ok(Some(if self.match_(b'=') {
                TokenValue::LessEqual
            } else {
                TokenValue::Less
            })),

            b'>' => Ok(Some(if self.match_(b'=') {
                TokenValue::GreaterEqual
            } else {
                TokenValue::Greater
            })),

            b'/' => {
                if self.match_(b'/') {
                    // Comment to the end of the line
                    while self.peek() != b'\n' && !self.is_at_end() {
                        self.advance();
                    }
                    Ok(None)
                } else if self.match_(b'*') {
                    while !(self.is_at_end() || (self.peek() == b'*' && self.peek_next() == b'/')) {
                        self.advance();
                    }
                    if self.is_at_end() {
                        Err(Error::UnterminatedComment {
                            start: SourceLocation::new(self.source, self.start),
                        })
                    } else {
                        self.advance();
                        self.advance();
                        Ok(None)
                    }
                } else {
                    Ok(Some(TokenValue::Slash))
                }
            }

            b' ' | b'\r' | b'\t' | b'\n' => Ok(None),

            b'"' => self.string().map(Some),
            c if c.is_ascii_digit() => self.number().map(Some),
            c @ b'_' | c if c.is_ascii_alphabetic() => {
                self.identifier().map(|x| match x.as_str() {
                    "and" => Some(TokenValue::And),
                    "class" => Some(TokenValue::Class),
                    "else" => Some(TokenValue::Else),
                    "false" => Some(TokenValue::False),
                    "fun" => Some(TokenValue::Fun),
                    "for" => Some(TokenValue::For),
                    "if" => Some(TokenValue::If),
                    "nil" => Some(TokenValue::Nil),
                    "or" => Some(TokenValue::Or),
                    "print" => Some(TokenValue::Print),
                    "return" => Some(TokenValue::Return),
                    "super" => Some(TokenValue::Super),
                    "this" => Some(TokenValue::This),
                    "true" => Some(TokenValue::True),
                    "var" => Some(TokenValue::Var),
                    "while" => Some(TokenValue::While),
                    _ => Some(TokenValue::Identifier(x)),
                })
            }

            c => Err(Error::UnexpectedCharacter {
                c: c.into(),
                location: SourceLocation::new(self.source, self.current),
            }),
        }
    }
}

#[cfg(test)]
mod test {
    use crate::token::TokenValue;

    #[test]
    fn test_tokens() {
        let source = b"(){},.-+;*!23!=42.42/* block \n comment */==<<==>/>=\"foo \nbar\"// this is a comment now".to_vec();
        let mut scanner = super::Scanner::new(&source);
        let tokens = scanner.scan_tokens().unwrap();

        for (i, v) in [
            TokenValue::LeftParen,
            TokenValue::RightParen,
            TokenValue::LeftBrace,
            TokenValue::RightBrace,
            TokenValue::Comma,
            TokenValue::Dot,
            TokenValue::Minus,
            TokenValue::Plus,
            TokenValue::Semicolon,
            TokenValue::Star,
            TokenValue::Bang,
            TokenValue::Number(23.0),
            TokenValue::BangEqual,
            TokenValue::Number(42.42),
            TokenValue::EqualEqual,
            TokenValue::Less,
            TokenValue::LessEqual,
            TokenValue::Equal,
            TokenValue::Greater,
            TokenValue::Slash,
            TokenValue::GreaterEqual,
            TokenValue::String("foo \nbar".to_string()),
        ]
        .iter()
        .enumerate()
        {
            assert_eq!(&tokens[i].value, v);
        }

        assert_eq!(tokens[12].lexeme, "!=");
    }

    #[test]
    fn test_unexpected_character() {
        let errs = super::Scanner::new(b"^").scan_tokens().err().unwrap();
        assert_eq!(errs.len(), 1);
    }

    // TODO test identifiers and reserved keywords
    // TODO Test each error case
    // TODO Test whitespace / multi line input
}
