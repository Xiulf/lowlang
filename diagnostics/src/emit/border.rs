use super::*;

pub struct BorderTop(pub usize);
pub struct BorderTopLeft(pub bool);
pub struct BorderLeft;
pub struct BorderSplit;
pub struct BorderBottomLeft;

impl Emit for BorderTop {
    fn emit(&self, e: &mut impl WriteColor) -> io::Result<()> {
        write!(e, "{}", "─".repeat(self.0))
    }
}

impl Emit for BorderTopLeft {
    fn emit(&self, e: &mut impl WriteColor) -> io::Result<()> {
        write!(e, "{}", if self.0 { '┌' } else { '┌' })
    }
}

impl Emit for BorderLeft {
    fn emit(&self, e: &mut impl WriteColor) -> io::Result<()> {
        write!(e, "│ ")
    }
}

impl Emit for BorderSplit {
    fn emit(&self, e: &mut impl WriteColor) -> io::Result<()> {
        write!(e, "├")
    }
}

impl Emit for BorderBottomLeft {
    fn emit(&self, e: &mut impl WriteColor) -> io::Result<()> {
        write!(e, "└")
    }
}
