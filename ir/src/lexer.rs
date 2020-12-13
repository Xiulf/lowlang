use std::iter::Peekable;

#[derive(Debug, PartialEq)]
pub enum Token {
    LParen,
    RParen,
    LBrace,
    RBrace,
    LBracket,
    RBracket,
    Equals,
    Comma,
    Dot,
    Star,
    Pipe,
    Colon,
    Arrow,
    DblColon,
    Scalar(u128),
    Identifier(String),
    Decl(String),
    Intrinsic(String),
    Local(String),
    Block(usize),
}

pub fn lex(src: &str) -> Vec<Token> {
    let mut tokens = Vec::new();
    let mut it = src.chars().peekable();

    while let Some(&c) = it.peek() {
        match c {
            '(' => {
                it.next();
                tokens.push(Token::LParen);
            }
            ')' => {
                it.next();
                tokens.push(Token::RParen);
            }
            '{' => {
                it.next();
                tokens.push(Token::LBrace);
            }
            '}' => {
                it.next();
                tokens.push(Token::RBrace);
            }
            '[' => {
                it.next();
                tokens.push(Token::LBracket);
            }
            ']' => {
                it.next();
                tokens.push(Token::RBracket);
            }
            '=' => {
                it.next();
                tokens.push(Token::Equals);
            }
            ',' => {
                it.next();
                tokens.push(Token::Comma);
            }
            '.' => {
                it.next();
                tokens.push(Token::Dot);
            }
            '*' => {
                it.next();
                tokens.push(Token::Star);
            }
            '|' => {
                it.next();
                tokens.push(Token::Pipe);
            }
            ':' => {
                it.next();

                if it.peek() == Some(&':') {
                    it.next();
                    tokens.push(Token::DblColon);
                } else {
                    tokens.push(Token::Colon);
                }
            }
            '-' => {
                it.next();

                if it.peek() == Some(&'>') {
                    it.next();
                    tokens.push(Token::Arrow);
                } else {
                    panic!("'-' must be followed by '>'");
                }
            }
            '0'..='9' => {
                it.next();

                let s = lex_scalar(c, &mut it);

                tokens.push(Token::Scalar(s));
            }
            '_' => {
                it.next();

                let s = lex_ident(&mut it);

                tokens.push(Token::Local(s));
            }
            '%' => {
                it.next();

                let s = lex_scalar('0', &mut it);

                tokens.push(Token::Block(s as usize));
            }
            '@' => {
                it.next();

                let i = lex_ident(&mut it);

                tokens.push(Token::Decl(i));
            }
            '#' => {
                it.next();

                let i = lex_ident(&mut it);

                tokens.push(Token::Intrinsic(i));
            }
            c if c.is_alphabetic() => {
                let i = lex_ident(&mut it);

                tokens.push(Token::Identifier(i));
            }
            c if c.is_whitespace() => {
                it.next();
            }
            ';' => {
                while !matches!(it.peek(), Some('\n')) {
                    it.next();
                }
            }
            _ => panic!("Unknown character {:?}", c),
        }
    }

    tokens
}

fn lex_scalar(c: char, it: &mut Peekable<impl Iterator<Item = char>>) -> u128 {
    let mut res = (c as u32 - '0' as u32) as u128;

    while let Some(&c @ '0'..='9') = it.peek() {
        let digit = (c as u32 - '0' as u32) as u128;

        res = res * 10 + digit;
        it.next();
    }

    res
}

fn lex_ident(it: &mut Peekable<impl Iterator<Item = char>>) -> String {
    let mut res = String::with_capacity(1);

    while let Some(&c) = it.peek() {
        if c.is_alphanumeric() || c == '_' {
            it.next();
            res.push(c);
        } else {
            break;
        }
    }

    res
}
