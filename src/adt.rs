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
            )
        ),*;

        pub trait Visitor {
            $($(
                fn [<visit_ $n:lower>](&mut self, x: &$n);
            )*)*
        }

        $(
            pub fn [<walk_ $r:lower>](mut visitor: impl Visitor, x: &$r) {
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
        Grouping: struct {
            pub extr: Box<Expr>
        },
    }
}
