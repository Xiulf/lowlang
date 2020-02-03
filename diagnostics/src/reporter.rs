use crate::{Diagnostic, Severity, Span};
use std::cell::RefCell;

#[derive(Default)]
pub struct Reporter {
    diagnostics: RefCell<Vec<Diagnostic>>,
}

impl Reporter {
    pub fn add(&self, diagnostic: Diagnostic) {
        let is_bug = diagnostic.severity == Severity::Bug;

        self.diagnostics.borrow_mut().push(diagnostic);

        if is_bug {
            self.report(true);
        }
    }

    pub fn remove(&self, span: Span, code: u16) {
        self.diagnostics.borrow_mut().retain(|diag| {
            !(diag.labels[0].span == Some(span) && diag.code == Some(code))
        });
    }
    
    pub fn has_errors(&self) -> bool {
        self.diagnostics.borrow().iter().any(|d| d.severity == Severity::Error || d.severity == Severity::Bug)
    }
    
    pub fn report(&self, exit: bool) {
        self.diagnostics.borrow_mut().sort_by_key(|d| d.severity);
        
        for d in self.diagnostics.borrow().iter() {
            let _ = crate::emit::emit(d);
        }
        
        if self.has_errors() && exit {
            std::process::exit(0);
        }
    }
}
