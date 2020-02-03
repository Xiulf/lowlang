use super::*;
use crate::Position;

pub struct FileName<'a> {
    pub name: &'a std::path::Path,
    pub pos: Position,
}

impl<'a> Emit for FileName<'a> {
    fn emit(&self, e: &mut impl WriteColor) -> io::Result<()> {
        write!(e, "{}:{}:{}", self.name.display(), self.pos.line + 1, self.pos.col + 1)
    }
}
