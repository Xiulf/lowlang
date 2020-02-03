#![feature(decl_macro)]

mod span;
mod file;
mod diagnostic;
mod reporter;
mod emit;

pub use file::*;
pub use span::*;
pub use diagnostic::*;
pub use reporter::*;

pub macro unimpl($span:expr, $msg:literal $(, $arg:expr)*) {
    $crate::Diagnostic::new($crate::Severity::Bug, None, format!("Unimplemented feature: {}", format!($msg $(, $arg)*)))
        .label($crate::Severity::Bug, $span, None::<String>)
}

pub macro unreach($span:expr, $msg:literal $(, $arg:expr)*) {
    $crate::Diagnostic::new($crate::Severity::Bug, None, format!("Unreachable code reached: {}", format!($msg $(, $arg)*)))
        .label($crate::Severity::Bug, $span, None::<String>)
}
