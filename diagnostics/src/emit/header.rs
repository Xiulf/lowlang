use super::*;
use crate::Severity;

pub struct Header<'a> {
    pub(crate) severity: Severity,
    pub(crate) code: Option<u16>,
    pub(crate) message: &'a str,
}

impl<'a> Emit for Header<'a> {
    fn emit(&self, e: &mut impl WriteColor) -> io::Result<()> {
       
        e.set_color(self.severity.color().set_bold(true))?;
        write!(e, "{}", self.severity.to_string())?;

        if let Some(code) = self.code {
            write!(e, "[{:0>4}]", code)?;
        }

        e.set_color(ColorSpec::new().set_bold(true).set_intense(true))?;
        write!(e, ": {}.\n", self.message)?;
        e.reset()
    }
}
