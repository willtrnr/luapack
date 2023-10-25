use full_moon::{
    tokenizer::{Symbol, Token, TokenReference, TokenType},
    ShortString,
};

pub trait IntoToken: Sized {
    fn into_token(self) -> Token;

    fn into_token_ref(self) -> TokenReference {
        TokenReference::new(Vec::new(), self.into_token(), Vec::new())
    }
}

impl IntoToken for TokenType {
    #[inline]
    fn into_token(self) -> Token {
        Token::new(self)
    }
}

impl IntoToken for Token {
    #[inline]
    fn into_token(self) -> Token {
        self
    }
}

impl IntoToken for Symbol {
    #[inline]
    fn into_token(self) -> Token {
        Token::new(TokenType::Symbol { symbol: self })
    }
}

impl IntoToken for bool {
    #[inline]
    fn into_token(self) -> Token {
        Token::new(TokenType::Symbol {
            symbol: if self { Symbol::True } else { Symbol::False },
        })
    }
}

macro_rules! impl_into_token_num {
    ($($t:ty)+) => {
        $(
            impl IntoToken for $t {
                #[inline]
                fn into_token(self) -> Token {
                    Token::new(TokenType::Number { text: self.to_string().into() })
                }
            }
        )*
    }
}

impl_into_token_num!(u8 u16 u32 u64 i8 i16 i32 i64 f32 f64);

impl IntoToken for String {
    fn into_token(self) -> Token {
        let multiline = self.contains('\n') || self.contains('\r');
        Token::new(TokenType::StringLiteral {
            multi_line: if multiline {
                Some(if self.contains("]]") {
                    if self.contains("=]") {
                        2
                    } else {
                        1
                    }
                } else {
                    0
                })
            } else {
                None
            },
            quote_type: if multiline {
                full_moon::tokenizer::StringLiteralQuoteType::Brackets
            } else if !self.contains('"') {
                full_moon::tokenizer::StringLiteralQuoteType::Double
            } else if !self.contains('\'') {
                full_moon::tokenizer::StringLiteralQuoteType::Single
            } else {
                full_moon::tokenizer::StringLiteralQuoteType::Brackets
            },
            literal: self.into(),
        })
    }
}

impl<T: IntoToken> IntoToken for Box<T> {
    #[inline]
    fn into_token(self) -> Token {
        (*self).into_token()
    }

    #[inline]
    fn into_token_ref(self) -> TokenReference {
        (*self).into_token_ref()
    }
}

impl<T: IntoToken> IntoToken for Option<T> {
    fn into_token(self) -> Token {
        match self {
            Some(v) => v.into_token(),
            None => Symbol::Nil.into_token(),
        }
    }

    fn into_token_ref(self) -> TokenReference {
        match self {
            Some(v) => v.into_token_ref(),
            None => Symbol::Nil.into_token_ref(),
        }
    }
}

pub trait TokenReferenceExt {
    fn identifier<S: Into<ShortString>>(ident: S) -> TokenReference {
        TokenReference::new(
            Vec::new(),
            Token::new(TokenType::Identifier {
                identifier: ident.into(),
            }),
            Vec::new(),
        )
    }

    fn as_number_value(&self) -> Option<f64>;
    fn as_string_value(&self) -> Option<&str>;
    fn as_identifier(&self) -> Option<&str>;
}

impl TokenReferenceExt for TokenReference {
    fn as_number_value(&self) -> Option<f64> {
        if let TokenType::Number { text } = self.token_type() {
            text.parse().ok()
        } else {
            None
        }
    }

    fn as_string_value(&self) -> Option<&str> {
        if let TokenType::StringLiteral { literal, .. } = self.token_type() {
            Some(literal.as_str())
        } else {
            None
        }
    }

    fn as_identifier(&self) -> Option<&str> {
        if let TokenType::Identifier { identifier } = self.token_type() {
            Some(identifier.as_str())
        } else {
            None
        }
    }
}
