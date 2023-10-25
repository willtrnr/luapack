use crate::token::{IntoToken, TokenReferenceExt};
use full_moon::{
    ast::{
        self,
        punctuated::{Pair, Punctuated},
    },
    tokenizer::{Symbol, TokenReference},
    visitors::VisitorMut,
    ShortString,
};

pub use ast::Ast;

pub trait IntoValue {
    fn into_value(self) -> ast::Value;
}

impl IntoValue for ast::Value {
    #[inline]
    fn into_value(self) -> ast::Value {
        self
    }
}

impl IntoValue for ast::FunctionCall {
    #[inline]
    fn into_value(self) -> ast::Value {
        ast::Value::FunctionCall(self)
    }
}

impl IntoValue for ast::types::IfExpression {
    #[inline]
    fn into_value(self) -> ast::Value {
        ast::Value::IfExpression(self)
    }
}

impl IntoValue for ast::types::InterpolatedString {
    #[inline]
    fn into_value(self) -> ast::Value {
        ast::Value::InterpolatedString(self)
    }
}

impl IntoValue for ast::TableConstructor {
    #[inline]
    fn into_value(self) -> ast::Value {
        ast::Value::TableConstructor(self)
    }
}

impl IntoValue for ast::Expression {
    #[inline]
    fn into_value(self) -> ast::Value {
        ast::Value::ParenthesesExpression(self)
    }
}

impl IntoValue for ast::Var {
    #[inline]
    fn into_value(self) -> ast::Value {
        ast::Value::Var(self)
    }
}

impl IntoValue for Symbol {
    #[inline]
    fn into_value(self) -> ast::Value {
        ast::Value::Symbol(self.into_token_ref())
    }
}

impl IntoValue for bool {
    fn into_value(self) -> ast::Value {
        ast::Value::Symbol(self.into_token_ref())
    }
}

macro_rules! impl_into_value_num {
    ($($t:ty)+) => {
        $(
            impl IntoValue for $t {
                #[inline]
                fn into_value(self) -> ast::Value {
                    ast::Value::Number(self.into_token_ref())
                }
            }
        )*
    }
}

impl_into_value_num!(u8 u16 u32 u64 i8 i16 i32 i64 f32 f64);

impl IntoValue for String {
    fn into_value(self) -> ast::Value {
        ast::Value::String(self.into_token_ref())
    }
}

impl<T: IntoValue> IntoValue for Box<T> {
    #[inline]
    fn into_value(self) -> ast::Value {
        (*self).into_value()
    }
}

impl<T: IntoValue> IntoValue for Option<T> {
    fn into_value(self) -> ast::Value {
        match self {
            Some(v) => v.into_value(),
            None => ast::Value::Symbol(Symbol::Nil.into_token_ref()),
        }
    }
}

pub trait PunctuatedExt<T> {
    fn single(item: T) -> Punctuated<T> {
        let mut slf = Punctuated::new();
        slf.push(Pair::new(item, None));
        slf
    }
}

impl<T> PunctuatedExt<T> for Punctuated<T> {}

pub trait AstExt {
    fn take_nodes(self) -> ast::Block;
}

impl AstExt for ast::Ast {
    fn take_nodes(self) -> ast::Block {
        struct V(Option<ast::Block>);
        impl VisitorMut for V {
            fn visit_block(&mut self, node: ast::Block) -> ast::Block {
                self.0.get_or_insert(node);
                ast::Block::new()
            }
        }

        let mut v = V(None);
        v.visit_ast(self);
        v.0.unwrap_or_default()
    }
}

pub trait ExpressionExt {
    fn value(value: ast::Value) -> ast::Expression {
        ast::Expression::Value {
            value: Box::new(value),
            type_assertion: None,
        }
    }
}

impl ExpressionExt for ast::Expression {}

pub trait VarExpressionExt {
    fn name<S: Into<ShortString>>(name: S) -> ast::VarExpression {
        ast::VarExpression::new(ast::Prefix::Name(TokenReference::identifier(name)))
    }
}

impl VarExpressionExt for ast::VarExpression {}
