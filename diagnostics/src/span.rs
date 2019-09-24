use crate::file::FileId;
use fluix_encode::{Encodable, Decodable};

#[derive(Debug, Copy, Clone, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Encodable, Decodable)]
pub struct Position {
    pub offset: usize,
    pub col: usize,
    pub line: usize
}

#[derive(Debug, Clone, Copy, Default, PartialEq, Eq, PartialOrd, Ord, Hash, Encodable, Decodable)]
pub struct Span {
    pub start: Position,
    pub end: Position,
    pub file: FileId,
}

impl Span {
    pub fn to(self, other: Span) -> Span {
        Span {
            start: self.start,
            end: other.end,
            file: self.file
        }
    }
}

impl std::fmt::Display for Position {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}:{}", self.line + 1, self.col + 1)
    }
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}