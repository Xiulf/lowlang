use crate::buffer::{Entry, TokenBuffer};
use crate::error::Result;
use crate::ident::Ident;
use crate::literal::*;
use crate::punct::{Punct, Spacing};
use diagnostics::{Reporter, Diagnostic, Severity, FileId, Position, Span};

pub struct Lexer<'a> {
    reporter: &'a Reporter,
    file: FileId,
    source: &'a str,
    chars: std::iter::Peekable<std::str::CharIndices<'a>>,
    start: Position,
    pos: Position,
}

impl<'a> Lexer<'a> {
    pub fn new(source: &'a str, file: FileId, reporter: &'a Reporter) -> Lexer<'a> {
        Lexer {
            reporter,
            file,
            source,
            chars: source.char_indices().peekable(),
            start: Position::default(),
            pos: Position::default(),
        }
    }

    pub fn run(&mut self) -> TokenBuffer {
        let mut tokens = Vec::new();
        
        while !self.eof() {
            match self.next() {
                Ok(t) => tokens.push(t),
                Err(e) => self.reporter.add(e),
            }
        }

        if let Some(Entry::Empty) = tokens.last() {} else {
            tokens.push(Entry::Empty);
        }

        TokenBuffer::new(tokens)
    }

    fn next(&mut self) -> Result<Entry> {
        self.skip();
        self.start = self.pos;
        
        let ch = self.peek();
        
        self.advance();
        
        match ch {
            'r' if self.peek() == '"' => {
                self.advance();
                self.string(true)
            }
            '"' => self.string(false),
            '\'' => self.char(),
            '0'..='9' => self.number(),
            'a'..='z' | 'A'..='Z' | '_' => self.ident(),
            '\0' => Ok(Entry::Empty),
            ch => {
                let spacing = if match self.peek() {
                    'r' if self.peek_n(1) == '"' => false,
                    '"' => false,
                    '\'' => false,
                    '0'..='9' => false,
                    'a'..='z' | 'A'..='Z' | '_' => false,
                    ' ' | '\t' | '\r' | '\n' => false,
                    _ => true,
                } {
                    Spacing::Joint
                } else {
                    Spacing::Alone
                };
                
                Ok(Entry::Punct(Punct {
                    span: self.span(),
                    ch,
                    spacing,
                }))
            }
        }
    }

    fn skip(&mut self) {
        let mut blocks = 0;
        
        while !self.eof() {
            match self.peek() {
                ' ' | '\t' | '\r' | '\n' => self.advance(),
                '/' => {
                    if self.peek_n(1) == '/' {
                        self.advance();
                        self.advance();
                        
                        while !self.eof() && self.peek() != '\n' {
                            self.advance();
                        }
                    } else if self.peek_n(1) == '*' {
                        self.advance();
                        blocks += 1;
                        
                        while !self.eof() && blocks > 0 {
                            self.advance();
                            
                            if self.peek() == '/' && self.peek_n(1) == '*' {
                                blocks += 1;
                                self.advance();
                            }
                            
                            if self.peek() == '*' && self.peek_n(1) == '/' {
                                blocks -= 1;
                                self.advance();
                                self.advance();
                            }
                        }
                    } else {
                        break;
                    }
                }
                _ => break,
            }
        }
    }

    fn ident(&mut self) -> Result<Entry> {
        while !self.eof() {
            match self.peek() {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' => self.advance(),
                _ => break,
            }
        }
        
        Ok(Entry::Ident(Ident {
            span: self.span(),
            name: self.text().to_string(),
        }))
    }

    fn string(&mut self, raw: bool) -> Result<Entry> {
        let mut text = String::new();
        
        while !self.eof() {
            match self.peek() {
                '"' => {
                    self.advance();
                    break;
                }
                '\\' if !raw => {
                    self.advance();
                    text.push(self.escape()?);
                }
                ch => {
                    self.advance();
                    text.push(ch);
                }
            }
        }
        
        Ok(Entry::Literal(Literal::String(StringLiteral {
            span: self.span(),
            text,
        })))
    }

    fn char(&mut self) -> Result<Entry> {
        let ch = match self.peek() {
            '\\' => {
                self.advance();
                self.escape()?
            }
            ch => {
                self.advance();
                ch
            }
        };
        
        if self.peek() == '\'' {
            self.advance();
        } else {
            return Err(Diagnostic::new(
                Severity::Error,
                None,
                "invalid character literal",
            ).label(Severity::Error, self.span(), "test"));
        }
        
        Ok(Entry::Literal(Literal::Char(CharLiteral {
            span: self.span(),
            ch,
        })))
    }
    
    fn number(&mut self) -> Result<Entry> {
        let mut count = 0;
        
        while !self.eof() {
            match self.peek() {
                '_' if self.peek_n(1) >= '0' && self.peek_n(1) <= '9' => self.advance(),
                '0'..='9' => self.advance(),
                _ => break,
            }
            
            count += 1;
        }
        
        let mut float = false;
        
        if self.peek() == '.' && self.peek_n(1) != '.' {
            self.advance();
            float = true;
            
            while !self.eof() {
                match self.peek() {
                    '_' if self.peek_n(1) >= '0' && self.peek_n(1) <= '9' => self.advance(),
                    '0'..='9' => self.advance(),
                    _ => break,
                }
            }
            
            if self.peek() == 'e' && (self.peek_n(1) == '+' || self.peek_n(1) == '-') {
                self.advance();
                self.advance();
                
                while !self.eof() {
                    match self.peek() {
                        '0'..='9' => self.advance(),
                        _ => break,
                    }
                }
            }
        } else if self.peek() == 'x' && count == 0 {
            self.advance();
            
            while !self.eof() {
                match self.peek() {
                    '0'..='9' | 'a'..='z' | 'A'..='Z' => self.advance(),
                    _ => break,
                }
            }
        } else if self.peek() == 'b' && count == 0 {
            self.advance();

            while !self.eof() {
                match self.peek() {
                    '0' | '1' => self.advance(),
                    _ => break,
                }
            }
        }
        
        let mut ty = String::new();
        
        while !self.eof() {
            match self.peek() {
                c @ 'a'..='z' | c @ '0'..='9' => {
                    ty.push(c);
                    self.advance();
                }
                _ => break,
            }
        }

        if float || ty == "f32" || ty == "f64" {
            let val: f64 = self.source[self.start.offset..self.pos.offset - ty.len()]
                .parse()
                .map_err(|_| {
                    Diagnostic::new(
                        Severity::Error,
                        None,
                        "Invalid floating point literal",
                    ).label(Severity::Error, self.span(), None::<String>)
                })?;
            Ok(Entry::Literal(Literal::Float(FloatLiteral {
                span: self.span(),
                float: val.to_bits(),
                ty: match ty.as_str() {
                    "f32" => FloatType::F32,
                    "f64" => FloatType::F64,
                    "" => FloatType::Unknown,
                    _ => {
                        let span = self.span();
                        let span = Span {
                            start: Position {
                                line: span.end.line,
                                col: span.end.col - ty.len(),
                                offset: span.end.offset - ty.len(),
                            },
                            .. span
                        };
                        
                        return Err(Diagnostic::new(
                            Severity::Error,
                            None,
                            "Invalid number type",
                        ).label(Severity::Error, span, None::<String>))
                    }
                },
            })))
        } else {
            let text = &self.source[self.start.offset..self.pos.offset - ty.len()];
            let val = if text.contains('x') && text.find('x') == Some(1) {
                u128::from_str_radix(&text[2..], 16)
            } else if text.contains('b') && text.find('b') == Some(1) {
                u128::from_str_radix(&text[2..], 2)
            } else {
                u128::from_str_radix(&text, 10)
            }.map_err(|_| {
                Diagnostic::new(Severity::Error, None, "Invalid integer literal")
                    .label(Severity::Error, self.span(), None::<String>)
            })?;
            
            Ok(Entry::Literal(Literal::Int(IntLiteral {
                span: self.span(),
                int: val,
                ty: match ty.as_str() {
                    "u8" => IntType::U8,
                    "u16" => IntType::U16,
                    "u32" => IntType::U32,
                    "u64" => IntType::U64,
                    "i8" => IntType::I8,
                    "i16" => IntType::I16,
                    "i32" => IntType::I32,
                    "i64" => IntType::I64,
                    "" => IntType::Unknown,
                    _ => {
                        let span = self.span();
                        let span = Span {
                            start: Position {
                                line: span.end.line,
                                col: span.end.col - ty.len(),
                                offset: span.end.offset - ty.len(),
                            },
                            .. span
                        };
                        
                        return Err(Diagnostic::new(
                            Severity::Error,
                            None,
                            "Invalid number type",
                        ).label(Severity::Error, span, None::<String>));
                    }
                },
            })))
        }
    }
    
    fn escape(&mut self) -> Result<char> {
        let ch = self.peek();
        
        self.advance();
        
        match ch {
            '"' => Ok('"'),
            '\'' => Ok('\''),
            'n' => Ok('\n'),
            'r' => Ok('\r'),
            't' => Ok('\t'),
            '0' => Ok('\0'),
            'u' => {
                let mut num = String::new();
                
                while !self.eof() {
                    match self.peek() {
                        c @ '0'..='9' |
                        c @ 'a'..='z' |
                        c @ 'A'..='Z' => {
                            self.advance();
                            num.push(c);
                        },
                        _ => break,
                    }
                }

                u32::from_str_radix(&num, 16).map(|n| match std::char::from_u32(n) {
                    Some(c) => Ok(c),
                    None => Err(Diagnostic::new(Severity::Error, None, "invalid character escape")
                        .label(Severity::Error, self.span(), None::<String>))
                }).map_err(|_| {
                    Diagnostic::new(Severity::Error, None, "invalid character escape")
                        .label(Severity::Error, self.span(), None::<String>)
                })?
            },
            _ => Err(Diagnostic::new(
                Severity::Error,
                None,
                "invalid character escape",
            ).label(Severity::Error, self.span(), None::<String>)),
        }
    }

    fn span(&self) -> Span {
        Span {
            start: self.start,
            end: self.pos,
            file: self.file,
        }
    }

    fn text(&self) -> &str {
        &self.source[self.start.offset..self.pos.offset]
    }

    fn eof(&mut self) -> bool {
        self.peek() == '\0'
    }

    fn peek(&mut self) -> char {
        self.chars.peek().map(|c| c.1).unwrap_or('\0')
    }

    fn peek_n(&self, n: usize) -> char {
        let mut chars = self.chars.clone();
        let mut ch = chars.next();

        for _ in 0..n {
            ch = chars.next();
        }

        ch.map(|(_, c)| c).unwrap_or('\0')
    }

    fn advance(&mut self) {
        if let Some((idx, ch)) = self.chars.next() {
            if ch == '\n' {
                self.pos.line += 1;
                self.pos.col = 0;
            } else {
                self.pos.col += 1;
            }

            self.pos.offset = idx + 1;
        }
    }
}
