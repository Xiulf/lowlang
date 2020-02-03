use crate::parse::{Parse, ParseStream, ToTokens};
use crate::buffer::{Cursor, TokenBuffer, Entry};
use crate::token::Token;
use crate::error::Result;
use diagnostics::{Span, Spanned};

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Punct {
    pub span: Span,
    pub ch: char,
    pub spacing: Spacing
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Spacing {
    Alone,
    Joint
}

impl Parse for Punct {
    fn parse(input: ParseStream) -> Result<Punct> {
        input.step(|cursor| match cursor.punct() {
            Some((p, rest)) => Ok((p.clone(), rest)),
            None => Err(cursor.error("expected a punctuation character"))
        })
    }
}

impl Token for Punct {
    fn peek(cursor: Cursor) -> bool {
        match cursor.punct() {
            Some(_) => true,
            None => false,
        }
    }
    
    fn display() -> &'static str {
        "punctuation"
    }
}

impl Spanned for Punct {
    fn span(&self) -> Span {
        self.span.clone()
    }
}

impl ToTokens for Punct {
    fn to_tokens(&self) -> TokenBuffer {
        TokenBuffer::new(vec![
            Entry::Punct(self.clone())
        ])
    }
}