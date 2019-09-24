pub mod code;
pub mod diagnostic;
pub mod span;
pub mod file;
pub mod reporter;

pub use diagnostic::*;
pub use span::*;
pub use file::*;
pub use reporter::*;
pub use diagnostics_derive::ToDiagnostic;

pub fn warn(span: span::Span, msg: String) {
    let d = diagnostic::Diagnostic::new(diagnostic::Severity::Warning, span, msg);
    
    d.print("", "");
}