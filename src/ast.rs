use paste::paste;

use crate::{token::Token, types::Number};

macro_rules! ast_root {
    (pub enum $r:ident { $($n:ident: $t:ident $b:tt),* $(,)? }) => {
        #[derive(Debug)]
        pub enum $r {
            $($n($n)),*
        }

        $(
            #[derive(Debug)]
            pub $t $n $b
        )*
    }
}

macro_rules! ast {
    ($(pub enum $r:ident { $($n:ident: $t:ident $b:tt),* $(,)? })*) => { paste! {
        $(
            ast_root!(
                pub enum $r { $($n: $t $b),* }
            );

            pub trait [<$r Visitor>]<T> {
                $(
                    fn [<visit_ $n:lower>](&mut self, x: &$n) -> T;
                )*
            }

            pub fn [<walk_ $r:lower>]<T>(mut visitor: impl [<$r Visitor>]<T>, x: &$r) -> T {
                match x {
                    $(
                        $r::$n(y) => visitor.[<visit_ $n:lower>](y)
                    ),*
                }
            }
        )*
    }}
}

ast! {
    pub enum Expr {
        Literal: enum {
            Number(Number),
            String(String),
            True,
            False,
            Nil,
        },
        Unary: struct {
            pub operator: Token,
            pub right: Box<Expr>,
        },
        Binary: struct {
            pub left: Box<Expr>,
            pub operator: Token,
            pub right: Box<Expr>,
        },
        Ternary: struct {
            pub left: Box<Expr>,
            pub mid: Box<Expr>,
            pub right: Box<Expr>,
        },
        Grouping: struct {
            pub expr: Box<Expr>
        },
        Variable: struct {
            pub name: Token
        },
        Assign: struct {
            pub name: Token,
            pub value: Box<Expr>
        }
    }

    pub enum Stmt {
        Block: struct {
            pub statements: Vec<Stmt>
        },
        Expression: struct {
            pub expr: Expr
        },
        If: struct {
            pub condition: Box<Expr>,
            pub then_branch: Box<Stmt>,
            pub else_branch: Option<Box<Stmt>>
        },
        Print: struct {
            pub expr: Expr
        },
        Var: struct {
            pub name: Token,
            pub initializer: Option<Box<Expr>>
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Literal(Literal::Number(n)) => n.fmt(f),
            Expr::Literal(Literal::String(s)) => s.fmt(f),
            Expr::Literal(Literal::False) => false.fmt(f),
            Expr::Literal(Literal::True) => true.fmt(f),
            Expr::Literal(Literal::Nil) => f.write_str("nil"),
            Expr::Unary(Unary { operator, right }) => {
                f.write_fmt(format_args!("{}{}", operator, right))
            }
            Expr::Binary(Binary {
                left,
                operator,
                right,
            }) => f.write_fmt(format_args!("({} {} {})", left, operator, right)),
            Expr::Ternary(Ternary { left, mid, right }) => {
                f.write_fmt(format_args!("({} ? ({}) : {})", left, mid, right))
            }
            Expr::Grouping(Grouping { expr }) => f.write_fmt(format_args!("({})", expr)),
            Expr::Variable(Variable { name }) => name.lexeme.fmt(f),
            Expr::Assign(Assign { name, value }) => write!(f, "{} = {}", name, value),
        }
    }
}
