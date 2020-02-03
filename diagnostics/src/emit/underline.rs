use super::*;
use crate::Severity;

pub struct Underline<'a> {
    pub indent: usize,
    pub width: usize,
    pub severity: Severity,
    pub message: Option<&'a String>,
}

impl<'a> Emit for Underline<'a> {
    fn emit(&self, e: &mut impl WriteColor) -> io::Result<()> {
        write!(e, "{}", " ".repeat(self.indent))?;

        e.set_color(&self.severity.color())?;

        write!(e, "{}", match self.severity {
            Severity::Bug | Severity::Error => "^",
            Severity::Warning => "~",
            _ => "-",
        }.repeat(self.width))?;

        if let Some(msg) = &self.message {
            write!(e, " {}", msg)?;
        }

        e.reset()
    }
}
