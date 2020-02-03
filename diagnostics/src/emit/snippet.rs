use super::*;
use crate::{Span, FileInfo};
use intern::Intern;

pub struct Snippet<'a> {
    pub first: bool,
    pub labels: &'a [&'a crate::Label],
    pub gutter_width: usize,
}

impl<'a> Snippet<'a> {
    fn source_slice(&self, span: Span) -> String {
        FileInfo::untern(span.file).source[span.start.offset..span.end.offset].to_string()
    }
}

impl<'a> Emit for Snippet<'a> {
    fn emit(&self, e: &mut impl WriteColor) -> io::Result<()> {
        let first_file = super::file::FileName {
            name: &FileInfo::untern(self.labels[0].span.unwrap().file).name,
            pos: self.labels[0].span.unwrap().start,
        };

        write!(e, "{}", " ".repeat(self.gutter_width))?;
        
        if self.first {
            border::BorderTopLeft(false).emit(e)?;
        } else {
            border::BorderSplit.emit(e)?;
        }
        
        border::BorderTop(2).emit(e)?;
        write!(e, " ")?;
        first_file.emit(e)?;
        write!(e, " ")?;
        border::BorderTop(3).emit(e)?;
        write!(e, "\n")?;
        
        use std::collections::BTreeMap;
        
        let mut lines = BTreeMap::new();

        for label in self.labels {
            let span = label.span.unwrap();
            let label_style = label.severity.color();
            
            if span.start.line == span.end.line {
                let line = lines.entry(span.start.line).or_insert(source::SourceLine {
                    slice: self.source_slice(Span { file: span.file, start: span.line_start(false), end: span.line_end(true) }),
                    line: span.start.line + 1,
                    gutter_width: self.gutter_width,
                    annotations: Vec::new(),
                });
                
                line.annotations.push(source::Annotation {
                    color: label_style,
                    kind: source::AnnotationKind::Inline,
                    msg: label.message.as_ref(),
                    meta: (span.start.col, span.end.col),
                });
            } else {
                {
                    let line = lines.entry(span.start.line).or_insert(source::SourceLine {
                        slice: self.source_slice(Span { file: span.file, start: span.line_start(false), end: span.line_end(true) }),
                        line: span.start.line + 1,
                        gutter_width: self.gutter_width,
                        annotations: Vec::new(),
                    });
                    
                    let kind = if span.start.col <= line.slice.len() - line.slice.trim_start().len() {
                        source::AnnotationKind::MultilineStartAlt
                    } else {
                        source::AnnotationKind::MultilineStart
                    };
                    
                    line.annotations.push(source::Annotation {
                        color: label_style.clone(),
                        kind,
                        msg: None,
                        meta: (span.start.col, FileInfo::untern(span.file).source.lines().nth(span.start.line).unwrap().len()),
                    });
                }
                
                {
                    let line = lines.entry(span.end.line).or_insert(source::SourceLine {
                        slice: self.source_slice(Span { file: span.file, start: span.line_start(true), end: span.line_end(false) }),
                        line: span.end.line + 1,
                        gutter_width: self.gutter_width,
                        annotations: Vec::new(),
                    });
                    
                    line.annotations.push(source::Annotation {
                        color: label_style.clone(),
                        kind: source::AnnotationKind::MultilineEnd,
                        msg: label.message.as_ref(),
                        meta: (0, span.end.col),
                    });
                }
            }
        }
        
        write!(e, "{}", " ".repeat(self.gutter_width))?;
        border::BorderLeft.emit(e)?;
        write!(e, "\n")?;
        
        let lines = lines.into_iter().map(|mut v| {
            v.1.finish();
            v.1
        }).collect::<Vec<_>>();
        
        for (i, line) in lines.iter().enumerate() {
            if i > 0 && line.line - lines[i - 1].line > 1 && !line.is_connected() {
                write!(e, "{}", " ".repeat(self.gutter_width))?;
                border::BorderLeft.emit(e)?;
                write!(e, "\n")?;
            }
            
            line.emit(e)?;
        }

        write!(e, "{}", " ".repeat(self.gutter_width))?;
        border::BorderLeft.emit(e)?;
        write!(e, "\n")?;

        e.reset()
    }
}
