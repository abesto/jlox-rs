use crate::types::{Number, SourceIndex};

#[derive(Debug, PartialEq, Clone)]
pub enum TokenValue {
    // Single-character tokens.
    LeftParen,
    RightParen,
    LeftBrace,
    RightBrace,
    Colon,
    Comma,
    Dot,
    Minus,
    Plus,
    Question,
    Semicolon,
    Slash,
    Star,

    // One or two character tokens.
    Bang,
    BangEqual,
    Equal,
    EqualEqual,
    Greater,
    GreaterEqual,
    Less,
    LessEqual,

    // Literals.
    Identifier(String),
    String(String),
    Number(Number),

    // Keywords.
    And,
    Class,
    Else,
    False,
    Fun,
    For,
    If,
    Nil,
    Or,
    Print,
    Return,
    Super,
    This,
    True,
    Var,
    While,

    Eof,
}

#[derive(Debug, Clone)]
pub struct Token {
    pub value: TokenValue,
    pub lexeme: String,
    pub offset: SourceIndex,
}

impl std::fmt::Display for TokenValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match &self {
            TokenValue::LeftParen => f.write_str("("),
            TokenValue::RightParen => f.write_str(")"),
            TokenValue::LeftBrace => f.write_str("{"),
            TokenValue::RightBrace => f.write_str("}"),
            TokenValue::Colon => f.write_str(":"),
            TokenValue::Comma => f.write_str(","),
            TokenValue::Dot => f.write_str("."),
            TokenValue::Minus => f.write_str("-"),
            TokenValue::Plus => f.write_str("+"),
            TokenValue::Question => f.write_str("?"),
            TokenValue::Semicolon => f.write_str(";"),
            TokenValue::Slash => f.write_str("/"),
            TokenValue::Star => f.write_str("*"),
            TokenValue::Bang => f.write_str("!"),
            TokenValue::BangEqual => f.write_str("!="),
            TokenValue::Equal => f.write_str("="),
            TokenValue::EqualEqual => f.write_str("=="),
            TokenValue::Greater => f.write_str(">"),
            TokenValue::GreaterEqual => f.write_str(">="),
            TokenValue::Less => f.write_str("<"),
            TokenValue::LessEqual => f.write_str("<="),
            TokenValue::Identifier(s) => f.write_str(s),
            TokenValue::String(s) => s.fmt(f),
            TokenValue::Number(n) => n.fmt(f),
            TokenValue::And => f.write_str("and"),
            TokenValue::Class => f.write_str("class"),
            TokenValue::Else => f.write_str("else"),
            TokenValue::False => f.write_str("false"),
            TokenValue::Fun => f.write_str("fun"),
            TokenValue::For => f.write_str("for"),
            TokenValue::If => f.write_str("if"),
            TokenValue::Nil => f.write_str("nil"),
            TokenValue::Or => f.write_str("or"),
            TokenValue::Print => f.write_str("print"),
            TokenValue::Return => f.write_str("return"),
            TokenValue::Super => f.write_str("super"),
            TokenValue::This => f.write_str("this"),
            TokenValue::True => f.write_str("true"),
            TokenValue::Var => f.write_str("var"),
            TokenValue::While => f.write_str("while"),
            TokenValue::Eof => f.write_str("\\d"),
        }
    }
}

impl std::fmt::Display for Token {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        self.value.fmt(f)
    }
}
