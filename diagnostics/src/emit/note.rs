use super::*;

pub struct Note<'a> {
    pub gutter_width: usize,
    pub label: &'a crate::Label,
}

impl<'a> Emit for Note<'a> {
    fn emit(&self, e: &mut impl WriteColor) -> io::Result<()> {
        if let Some(msg) = &self.label.message {
            write!(e, "{}= ", " ".repeat(self.gutter_width))?;
            e.set_color(ColorSpec::new().set_bold(true).set_intense(true))?;
            
            write!(e, "{}: ", match &self.label.severity {
                crate::Severity::Info => "note",
                s => s.to_string(),
            })?;
            
            e.reset()?;
            write!(e, "{}\n", msg)?;
        }
        
        Ok(())
    }
}