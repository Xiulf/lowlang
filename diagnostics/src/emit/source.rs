use super::*;

pub struct SourceLine<'a> {
    pub slice: String,
    pub line: usize,
    pub gutter_width: usize,
    pub annotations: Vec<Annotation<'a>>,
}

pub struct Annotation<'a> {
    pub color: ColorSpec,
    pub kind: AnnotationKind,
    pub msg: Option<&'a String>,
    pub meta: (usize, usize),
}

pub enum AnnotationKind {
    /// 3 │   test :: fn () -> i32 {
    ///   │ ┌──────────────────────^
    MultilineStart,
    /// 3 │ ┌ test :: fn () -> i32 {
    MultilineStartAlt,
    /// 4 │ │     true
    MultilinePart,
    /// 5 │   }
    ///   │ └─^ ...
    MultilineEnd,
    /// 3 │ test :: fn () -> i32 {
    ///   │                  ^^^ ...
    Inline,
}

impl<'a> SourceLine<'a> {
    pub fn is_connected(&self) -> bool {
        self.annotations.iter().any(|a| match &a.kind {
            AnnotationKind::MultilineEnd |
            AnnotationKind::MultilinePart => true,
            _ => false,
        })
    }
    
    pub fn finish(&mut self) {
        self.annotations.sort_by_key(|a| std::cmp::Reverse(a.meta.0));
    }
}

impl<'a> Emit for SourceLine<'a> {
    fn emit(&self, e: &mut impl WriteColor) -> io::Result<()> {
        let mut bars = Vec::new();
        let mut starts = 0;
        let mut ends = Vec::new();
        let mut ends_mov = 1;
        
        fn emit_gutter(e: &mut impl WriteColor, line: Option<usize>, width: usize) -> io::Result<()> {
            if let Some(line) = line {
                write!(e, " {line:>width$} ", line = line, width = width - 2)?;
            } else {
                write!(e, "{}", " ".repeat(width))?;
            }

            border::BorderLeft.emit(e)
        }
        
        emit_gutter(e, Some(self.line), self.gutter_width)?;
        
        for ann in &self.annotations {
            match ann.kind {
                AnnotationKind::MultilineStart => {
                    write!(e, "  ")?;
                    starts += 1;
                },
                AnnotationKind::MultilineStartAlt => {
                    e.set_color(&ann.color)?;
                    border::BorderTopLeft(true).emit(e)?;
                    write!(e, " ")?;
                    e.reset()?;
                    bars.push(&ann.color);
                },
                AnnotationKind::MultilinePart => {
                    e.set_color(&ann.color)?;
                    border::BorderLeft.emit(e)?;
                    e.reset()?;
                    bars.push(&ann.color);
                },
                _ => ()
            }
        }
        
        for ann in self.annotations.iter().rev() {
            match ann.kind {
                AnnotationKind::MultilineEnd => {
                    e.set_color(&ann.color)?;
                    border::BorderLeft.emit(e)?;
                    e.reset()?;
                    ends.push(&ann.color);
                },
                _ => ()
            }
        }
        
        write!(e, "{}\n", self.slice)?;
        
        for ann in &self.annotations {
            match ann.kind {
                AnnotationKind::MultilineStart => {
                    emit_gutter(e, None, self.gutter_width)?;
                    
                    for bar in &bars { e.set_color(bar)?; border::BorderLeft.emit(e)?; }
                    
                    e.set_color(&ann.color)?;

                    border::BorderTopLeft(true).emit(e)?; 
                    write!(e, "{}{}\n", "─".repeat(ann.meta.0 + starts * 2 - 1), "^".repeat(ann.meta.1 - ann.meta.0))?;
                    
                    e.reset()?;
                    bars.push(&ann.color);
                    starts -= 1;
                },
                AnnotationKind::MultilineEnd => {
                    let msg = if let Some(msg) = &ann.msg {
                        format!(" {}", msg)
                    } else {
                        String::new()
                    };
                    
                    emit_gutter(e, None, self.gutter_width)?;
                    
                    for bar in &bars { e.set_color(bar)?; border::BorderLeft.emit(e)?; }
                    for end in 0..ends.len() - 1 { e.set_color(&ends[end])?; border::BorderLeft.emit(e)?; }
                    
                    ends.pop().unwrap();
                    e.set_color(&ann.color)?;
                    
                    border::BorderBottomLeft.emit(e)?;

                    write!(e, "{}{}{}\n",
                        "─".repeat(ann.meta.0 + ends_mov),
                        "^".repeat(ann.meta.1 - ann.meta.0),
                        msg
                    )?;
                    
                    e.reset()?;
                    
                    ends_mov += 2;
                },
                AnnotationKind::Inline => {
                    let msg = if let Some(msg) = &ann.msg {
                        format!(" {}", msg)
                    } else {
                        String::new()
                    };
                    
                    emit_gutter(e, None, self.gutter_width)?;
                    
                    for bar in &bars { e.set_color(bar)?; border::BorderLeft.emit(e)?; }
                    
                    e.set_color(&ann.color)?;
                    
                    write!(e, "{}{}{}\n", " ".repeat(ann.meta.0), "^".repeat(ann.meta.1 - ann.meta.0), msg)?;
                    
                    e.reset()?;
                },
                _ => (),
            }
        }
        
        Ok(())
    }
}
