use paste::paste;

use crate::{token::Token, types::Number};

pub trait Walkable<V, T, S> {
    fn walk(&self, visitor: V, state: S) -> T;
}

macro_rules! ast_root {
    (pub enum $r:ident { $($n:ident: $t:ident $b:tt),* $(,)? }) => {
        #[derive(Debug, Clone, PartialEq)]
        pub enum $r {
            $($n($n)),*
        }

        $(
            #[derive(Debug, Clone, PartialEq)]
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

            pub trait [<$r Visitor>]<T, S> {
                $(
                    fn [<visit_ $n:lower>](self, [<$r:lower>]: &$n, state: S) -> T;
                )*
            }

            impl<T, S, V: [<$r Visitor>]<T, S>> Walkable<V, T, S> for $r {
                fn walk(&self, visitor: V, state: S) -> T {
                    match self {
                        $(
                            $r::$n(y) => visitor.[<visit_ $n:lower>](y, state)
                        ),*
                    }
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
        Call: struct {
            pub callee: Box<Expr>,
            pub closing_paren: Token,
            pub arguments: Vec<Expr>,
        },
        Get: struct {
            pub object: Box<Expr>,
            pub name: Token
        },
        Logical: struct {
            pub left: Box<Expr>,
            pub operator: Token,
            pub right: Box<Expr>,
        },
        Lambda: struct {
            pub token: Token,
            pub params: Vec<Token>,
            pub body: Vec<Stmt>,
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
        Class: struct {
            pub name: Token,
            pub methods: Vec<Function>,
        },
        Function: struct {
            pub name: Token,
            pub params: Vec<Token>,
            pub body: Vec<Stmt>,
        },
        If: struct {
            pub condition: Box<Expr>,
            pub then_branch: Box<Stmt>,
            pub else_branch: Option<Box<Stmt>>
        },
        Print: struct {
            pub expr: Expr
        },
        Return: struct {
            pub keyword: Token,
            pub value: Option<Box<Expr>>,
        },
        Var: struct {
            pub name: Token,
            pub initializer: Option<Box<Expr>>
        },
        While: struct {
            pub condition: Box<Expr>,
            pub statement: Box<Stmt>
        },
        Break: struct {
            pub token: Token
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
            Expr::Logical(Logical {
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
            Expr::Call(Call {
                callee, arguments, ..
            }) => {
                write!(f, "{}(", callee)?;
                let mut iter = arguments.iter().peekable();
                while let Some(x) = iter.next() {
                    write!(f, "{}", x)?;
                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Expr::Lambda(Lambda { params, .. }) => {
                write!(f, "fun (")?;
                let mut iter = params.iter().peekable();
                while let Some(x) = iter.next() {
                    write!(f, "{}", x)?;
                    if iter.peek().is_some() {
                        write!(f, ", ")?;
                    }
                }
                write!(f, ")")
            }
            Expr::Get(Get { object, name }) => write!(f, "{}.{}", object, name),
        }
    }
}
