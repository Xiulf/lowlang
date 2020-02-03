pub mod border;
pub mod header;
pub mod file;
pub mod source;
pub mod snippet;
pub mod underline;
pub mod note;

use termcolor::{WriteColor, ColorSpec, StandardStream};
use std::io;

pub fn emit(diagnostic: &crate::Diagnostic) -> io::Result<()> {
    use std::collections::BTreeMap;

    let mut e = match &diagnostic.severity {
        crate::Severity::Bug | crate::Severity::Error => StandardStream::stderr(termcolor::ColorChoice::Always),
        _ => StandardStream::stdout(termcolor::ColorChoice::Always),
    };

    let last_line = diagnostic.labels.iter().fold(1, |acc, label| {
        usize::max(acc, label.span.unwrap_or_default().end.line + 1)
    });

    let gutter_width = format!(" {} ", last_line).len();

    let header = header::Header {
        severity: diagnostic.severity,
        code: diagnostic.code,
        message: &diagnostic.message,
    };

    let mut label_groups = BTreeMap::new();
    
    for label in &diagnostic.labels {
        if let Some(span) = &label.span {
            label_groups.entry(span.file).or_insert(Vec::new()).push(label.clone());
        }
    }
    
    let primary_file = diagnostic.labels[0].span.unwrap().file;
    let mut primary_labels = label_groups.remove(&primary_file).expect("no labels");

    primary_labels.sort_by(|a, b| a.span.cmp(&b.span));
    primary_labels.sort_by(|a, b| (a.span.unwrap().start.line == a.span.unwrap().end.line).cmp(&(b.span.unwrap().start.line == b.span.unwrap().end.line)));

    let snippet = snippet::Snippet {
        first: true,
        labels: &primary_labels,
        gutter_width,
    };

    header.emit(&mut e)?;
    snippet.emit(&mut e)?;

    for (_, mut labels) in label_groups {
        labels.sort_by(|a, b| a.span.cmp(&b.span));
        labels.sort_by(|a, b| (a.span.unwrap().start.line == a.span.unwrap().end.line).cmp(&(b.span.unwrap().start.line == b.span.unwrap().end.line)));

        let snippet = snippet::Snippet {
            first: false,
            labels: &labels,
            gutter_width,
        };

        snippet.emit(&mut e)?;
    }
    
    let notes = diagnostic.labels.iter().filter(|l| l.span.is_none()).collect::<Vec<_>>();

    for note in notes {
        note::Note {
            gutter_width,
            label: note,
        }.emit(&mut e)?;
    }
    
    e.reset()?;
    
    use io::Write;
    
    writeln!(&mut e)
}

pub trait Emit {
    fn emit(&self, e: &mut impl WriteColor) -> io::Result<()>;
}
