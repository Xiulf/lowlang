use crate::error::Result;
use crate::buffer::{Entry, TokenBuffer};
use crate::ident::Ident;
use crate::punct::{Punct, Spacing};
use crate::literal::*;
use diagnostics::{Position, Span, Diagnostic, Severity, SourceFile, FileId, Reporter};

pub struct Lexer<'a> {
    reporter: &'a Reporter,
    file: FileId,
    chars: Vec<char>,
    start: Position,
    pos: Position,
}

impl<'a> Lexer<'a> {
    pub fn new(file: &SourceFile, reporter: &'a Reporter) -> Lexer<'a> {
        Lexer {
            reporter,
            file: file.id,
            chars: file.text.chars().collect(),
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
            'r' if self.peek() == '"' => { self.advance(); self.string(true) },
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
                    _ => true
                } {
                    Spacing::Joint
                } else {
                    Spacing::Alone
                };
                
                Ok(Entry::Punct(Punct {
                    span: self.span(),
                    ch,
                    spacing
                }))
            }
        }
    }
    
    fn skip(&mut self) {
        let mut blocks = 0;
        
        while !self.eof() {
            match self.peek() {
                ' ' | '\t' | '\r' | '\n'  => self.advance(),
                '/' => {
                    if self.peek_n(1) == '/' {
                        self.advance();
                        self.advance();
                        
                        while !self.eof() && self.peek() != '\n' {
                            self.advance();
                        }
                    } else if self.peek_n(1) == '*' {
                        blocks += 1;
                        
                        while !self.eof() && blocks > 0 {
                            self.advance();
                            
                            if self.peek() == '/' && self.peek_n(1) == '*' {
                                blocks += 1;
                            }
                            
                            if self.peek() == '*' && self.peek_n(1) == '/' {
                                blocks -= 1;
                                self.advance();
                            }
                        }
                    } else {
                        break;
                    }
                },
                _ => break
            }
        }
    }
    
    fn ident(&mut self) -> Result<Entry> {
        while !self.eof() {
            match self.peek() {
                'a'..='z' | 'A'..='Z' | '0'..='9' | '_' | '-' => self.advance(),
                _ => break
            }
        }
        
        Ok(Entry::Ident(Ident {
            span: self.span(),
            text: self.text()
        }))
    }
    
    fn string(&mut self, raw: bool) -> Result<Entry> {
        let mut text = String::new();
        
        while !self.eof() {
            match self.peek() {
                '"' => {
                    self.advance();
                    break;
                },
                '\\' if !raw => {
                    self.advance();
                    text.push(self.escape()?);
                },
                ch => {
                    self.advance();
                    text.push(ch);
                }
            }
        }
        
        Ok(Entry::Literal(Literal::String(StringLiteral {
            span: self.span(),
            text
        })))
    }
    
    fn char(&mut self) -> Result<Entry> {
        let ch = match self.peek() {
            '\\' => {
                self.advance();
                self.escape()?
            },
            ch => {
                self.advance();
                ch
            }
        };
        
        if self.peek() == '\'' {
            self.advance();
        } else {
            return Err(Diagnostic::new(Severity::Error, self.span(), "invalid character literal"));
        }
        
        Ok(Entry::Literal(Literal::Char(CharLiteral {
            span: self.span(),
            ch
        })))
    }
    
    fn number(&mut self) -> Result<Entry> {
        while !self.eof() {
            match self.peek() {
                '0'..='9' => self.advance(),
                _ => break
            }
        }
        
        let mut float = false;
        
        if self.peek() == '.' {
            self.advance();
            float = true;
            
            while !self.eof() {
                match self.peek() {
                    '0'..='9' => self.advance(),
                    _ => break
                }
            }
        }
        
        if float {
            let val: f64 = self.text().parse().map_err(|_| Diagnostic::new(Severity::Error, self.span(), "invalid floating point literal"))?;
            
            Ok(Entry::Literal(Literal::Float(FloatLiteral {
                span: self.span(),
                float: val,
                ty: FloatType::Unknown
            })))
        } else {
            let val: f64 = self.text().parse().map_err(|_| Diagnostic::new(Severity::Error, self.span(), "invalid integer literal"))?;
            
            Ok(Entry::Literal(Literal::Int(IntLiteral {
                span: self.span(),
                int: val as u64,
                ty: IntType::Unknown
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
            'u' if self.peek() == '{' => {
                self.advance();
                
                let mut num = String::new();
                
                while !self.eof() {
                    match self.peek() {
                        c @ '0'..='9' => { self.advance(); num.push(c); },
                        _ => break
                    }
                }
                
                if self.peek() != '}' {
                    self.advance();
                    
                    return Err(Diagnostic::new(Severity::Error, self.span(), "invalid character escape"));
                } else {
                    self.advance();
                }
                
                format!("\\u{{{}}}", num).parse().map_err(|_| Diagnostic::new(Severity::Error, self.span(), "invalid character escape"))
            },
            _ => Err(Diagnostic::new(Severity::Error, self.span(), "invalid character escape"))
        }
    }
    
    fn span(&self) -> Span {
        Span {
            start: self.start,
            end: self.pos,
            file: self.file
        }
    }
    
    fn text(&self) -> String {
        self.chars[self.start.offset..self.pos.offset].iter().collect()
    }
    
    fn eof(&self) -> bool {
        self.pos.offset >= self.chars.len()
    }
    
    fn peek(&self) -> char {
        if self.eof() {
            '\0'
        } else {
            self.chars[self.pos.offset]
        }
    }
    
    fn peek_n(&self, n: usize) -> char {
        if self.pos.offset + n >= self.chars.len() {
            '\0'
        } else {
            self.chars[self.pos.offset + n]
        }
    }
    
    fn advance(&mut self) {
        if !self.eof() {
            if self.peek() == '\n' {
                self.pos.line += 1;
                self.pos.col = 0;
            } else {
                self.pos.col += 1;
            }
            
            self.pos.offset += 1;
        }
    }
}