#[macro_export]
macro_rules! let_stmt {
    ($name:expr => $value:expr) => {
        $crate::ast::Statement::Let($crate::ast::Let {
            name: $crate::ident!($name),
            value: $value,
        })
    }
}

#[macro_export]
macro_rules! return_stmt {
    ($value:expr) => {
        $crate::ast::Statement::Return($crate::ast::Return {
            value: $value,
        })
    };
}

#[macro_export]
macro_rules! block_stmt {
    ($($expression:expr;)*) => {
        $crate::ast::Statement::Block($crate::ast::Block {
            statements: vec![
                $($expression,)*
            ],
        })
    };
}

#[macro_export]
macro_rules! expr_stmt {
    ($kind:tt => $value:expr) => {
        $crate::ast::Statement::Expression($crate::ast::Expression::$kind($value))
    };
    ($value:expr) => {
        $crate::ast::Statement::Expression($value)
    }
}

#[macro_export]
macro_rules! expr {
    ($kind:tt => $value:expr) => {
        $crate::ast::Expression::$kind($value)
    };
}

#[macro_export]
macro_rules! ident {
    ($name:expr) => {
        $crate::ast::Identifier {
            value: $name.to_owned(),
        }
    };
}

#[macro_export]
macro_rules! int {
    ($value:expr) => {
        $crate::primitive!(Int => $value)
    };
}

#[macro_export]
macro_rules! float {
    ($value:expr) => {
        $crate::primitive!(Float => $value)
    };
}

#[macro_export]
macro_rules! hex {
    ($value:expr) => {
        $crate::primitive!(Hex => $value)
    };
}

#[macro_export]
macro_rules! string {
    ($value:expr) => {
        $crate::primitive!(String => String::from($value))
    };
}

#[macro_export]
macro_rules! bool {
    ($value:expr) => {
        $crate::primitive!(Bool => $value)
    };
}

#[macro_export]
macro_rules! primitive {
    ($kind:tt => $value:expr) => {
        $crate::ast::Expression::Primitive($crate::ast::Primitive::$kind($value))
    };
}

#[macro_export]
macro_rules! prefix {
    ($token:tt, $value:expr) => {
        $crate::ast::Expression::Prefix($crate::ast::Prefix::from($crate::token::Token::$token, $value))
    }
}

#[macro_export]
macro_rules! infix {
    ($left:expr, $operator:tt, $right:expr) => {
        $crate::ast::Expression::Infix($crate::ast::Infix::from($left, $crate::token::Token::$operator, $right))
    };
}