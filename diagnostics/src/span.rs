use crate::file::{FileId, FileInfo};
use intern::Intern;

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Span {
    pub file: FileId,
    pub start: Position,
    pub end: Position,
}

#[derive(Default, Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub struct Position {
    pub offset: usize,
    pub line: usize,
    pub col: usize,
}

impl Span {
    pub fn empty(file: FileId) -> Span {
        Span { file, .. Default::default() }
    }
    
    pub fn to(self, other: Span) -> Span {
        assert_eq!(self.file, other.file);
        Span { start: self.start, .. other }
    }
    
    pub fn line_start(&self, end: bool) -> Position {
        if end {
            Position {
                offset: self.end.offset - self.end.col,
                line: self.end.line,
                col: 0,
            }
        } else {
            Position {
                offset: self.start.offset - self.start.col,
                line: self.start.line,
                col: 0,
            }
        }
    }

    pub fn line_end(&self, start: bool) -> Position {
        if start {
            let len = FileInfo::untern(self.file).source.lines().nth(self.start.line).unwrap().len();

            Position {
                offset: self.start.offset + (len - self.start.col),
                line: self.start.line,
                col: len,
            }
        } else {
            let len = FileInfo::untern(self.file).source.lines().nth(self.end.line).unwrap().len();

            Position {
                offset: self.end.offset + (len - self.end.col),
                line: self.end.line,
                col: len,
            }
        }
    }
}

pub trait Spanned {
    fn span(&self) -> Span;
}
