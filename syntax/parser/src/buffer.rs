use crate::ident::Ident;
use crate::punct::Punct;
use crate::literal::Literal;
use crate::Spanned;
use diagnostics::Span;
use std::marker::PhantomData;

#[derive(Clone, Debug)]
pub enum Entry {
    Ident(Ident),
    Punct(Punct),
    Literal(Literal),
    Empty,
}

#[derive(Debug, Copy, Clone, Eq, PartialEq)]
pub struct Cursor<'a> {
    ptr: *const Entry,
    end: *const Entry,
    marker: PhantomData<&'a Entry>
}

#[derive(Debug, Clone)]
pub struct TokenBuffer {
    pub tokens: Vec<Entry>
}

impl<'a> Cursor<'a> {
    pub fn new(ptr: *const Entry, end: *const Entry) -> Cursor<'a> {
        Cursor {
            ptr,
            end,
            marker: PhantomData
        }
    }
    
    pub fn span(&self) -> Span {
        self.entry().span()
    }
    
    pub fn entry(self) -> &'a Entry {
        unsafe { &*self.ptr }
    }
    
    pub fn bump(self) -> Cursor<'a> {
        unsafe { Cursor::new(self.ptr.offset(1), self.end) }
    }
    
    pub fn eof(&self) -> bool {
        self.ptr == self.end
    }
    
    pub fn any(self) -> Option<(Entry, Cursor<'a>)> {
        Some((self.entry().clone(), self.bump()))
    }
    
    pub fn ident(self) -> Option<(Ident, Cursor<'a>)> {
        match self.entry() {
            Entry::Ident(ident) => Some((ident.clone(), self.bump())),
            _ => None
        }
    }
    
    pub fn punct(self) -> Option<(Punct, Cursor<'a>)> {
        match self.entry() {
            Entry::Punct(punct) => Some((punct.clone(), self.bump())),
            _ => None
        }
    }
    
    pub fn literal(self) -> Option<(Literal, Cursor<'a>)> {
        match self.entry() {
            Entry::Literal(literal) => Some((literal.clone(), self.bump())),
            _ => None
        }
    }
}

impl TokenBuffer {
    pub fn new(tokens: Vec<Entry>) -> TokenBuffer {
        TokenBuffer {
            tokens: tokens.into()
        }
    }
    
    pub fn begin(&self) -> Cursor {
        if self.tokens.is_empty() {
            struct UnsafeSyncEntry(Entry);
            unsafe impl Sync for UnsafeSyncEntry {}
            static EMPTY_ENTRY: UnsafeSyncEntry = UnsafeSyncEntry(Entry::Empty);
            
            Cursor {
                ptr: &EMPTY_ENTRY.0,
                end: &EMPTY_ENTRY.0,
                marker: PhantomData
            }
        } else {
            Cursor::new(&self.tokens[0], &self.tokens[self.tokens.len() - 1])
        }
    }
    
    pub fn span(&self) -> Span {
        if self.tokens.is_empty() {
            Span::default()
        } else {
            let first = self.tokens[0].span();
            let last = self.tokens[self.tokens.len() - 2].span();
            
            Span {
                start: first.start,
                end: last.end,
                file: first.file
            }
        }
    }
    
    pub fn extend(self, other: TokenBuffer) -> TokenBuffer {
        TokenBuffer {
            tokens: self.tokens.into_iter().chain(other.tokens.into_iter()).collect()
        }
    }
}

impl Spanned for Entry {
    fn span(&self) -> Span {
        match self {
            Entry::Ident(ident) => ident.span(),
            Entry::Punct(punct) => punct.span(),
            Entry::Literal(literal) => literal.span(),
            Entry::Empty => unreachable!()
        }
    }
}

impl std::fmt::Display for TokenBuffer {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        for entry in &self.tokens {
            match entry {
                Entry::Empty => (),
                Entry::Ident(id) => write!(f, "{} ", id.text)?,
                Entry::Punct(p) => if let crate::punct::Spacing::Joint = &p.spacing {
                    p.ch.fmt(f)?;
                } else {
                    write!(f, "{} ", p.ch)?;
                },
                Entry::Literal(l) => match l {
                    Literal::Int(i) => write!(f, "{} ", i.int)?,
                    Literal::Float(v) => write!(f, "{} ", v.float)?,
                    Literal::Char(c) => write!(f, "{:?} ", c.ch)?,
                    Literal::String(s) => write!(f, "{:?} ", s.text)?
                }
            }
        }
        
        Ok(())
    }
}