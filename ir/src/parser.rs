use logos::{Lexer, Logos};

#[derive(Debug, PartialEq, Eq, Logos)]
pub enum Token<'a> {
    #[error]
    #[regex(r"[ \t\n\f]+", logos::skip)]
    Error,

    #[regex(r"-?[0-9]+", |lex| lex.slice().parse())]
    Int(i128),

    #[regex(r#""[^"]+""#, |lex| &lex.slice()[1..lex.slice().len() - 1])]
    String(&'a str),

    #[regex(r"[a-zA-Z_][a-zA-Z0-9_]*")]
    Ident(&'a str),

    #[token(":")]
    Colon,

    #[token(",")]
    Comma,

    #[token(".")]
    Dot,

    #[token("=")]
    Equals,

    #[token("*")]
    Star,

    #[token("$")]
    Dollar,

    #[token("->")]
    Arrow,

    #[token("(")]
    LParen,

    #[token(")")]
    RParen,

    #[token("{")]
    LBrace,

    #[token("}")]
    RBrace,

    #[token("[")]
    LBracket,

    #[token("]")]
    RBracket,
}

pub struct Parser<'a> {
    lexer: Lexer<'a, Token<'a>>,
}
