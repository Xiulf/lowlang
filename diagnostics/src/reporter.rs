use crate::diagnostic::{Diagnostic, ToDiagnostic, Severity};
use crate::file::Filemap;
use std::rc::Rc;
use std::cell::RefCell;

pub struct Reporter {
    diagnostics: Rc<RefCell<Vec<Diagnostic>>>,
}

impl Reporter {
    pub fn new() -> Reporter {
        Reporter {
            diagnostics: Default::default()
        }
    }
    
    pub fn add(&self, d: Diagnostic) {
        self.diagnostics.borrow_mut().push(d);
    }
    
    pub fn add_into<D: ToDiagnostic>(&self, d: D) {
        self.diagnostics.borrow_mut().push(d.to_diagnostic());
    }
    
    pub fn has_errors(&self) -> bool {
        self.diagnostics.borrow().iter().any(|d| d.severity == Severity::Error || d.severity == Severity::Bug)
    }
    
    pub fn report(&self, files: &Filemap, exit: bool) {
        self.diagnostics.borrow_mut().sort_by_key(|d| d.severity);
        
        for d in self.diagnostics.borrow().iter() {
            d.print(&files.get(d.span.file).name, &files.get(d.span.file).text);
        }
        
        if self.has_errors() && exit {
            std::process::exit(0);
        }
        
        self.diagnostics.borrow_mut().clear();
    }
}