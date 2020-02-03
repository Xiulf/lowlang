use crate::Span;

#[derive(Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub code: Option<u16>,
    pub message: String,
    pub labels: Vec<Label>,
}

#[derive(Debug)]
pub struct Label {
    pub span: Option<Span>,
    pub message: Option<String>,
    pub severity: Severity,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Bug,
    Error,
    Warning,
    Info,
    Help,
}

impl Diagnostic {
    pub fn new(severity: Severity, code: impl Into<Option<u16>>, message: impl Into<String>) -> Diagnostic {
        Diagnostic {
            severity,
            code: code.into(),
            message: message.into(),
            labels: Vec::new(),
        }
    }
    
    pub fn label<M, S>(mut self, severity: Severity, span: S, message: M) -> Diagnostic
    where
        M: IntoOption<String>,
        S: Into<Option<Span>>,
    {
        self.labels.push(Label {
            severity,
            span: span.into(),
            message: message.into_option(),
        });
        
        self
    }
    
    pub fn note(self, message: impl Into<String>) -> Diagnostic {
        self.label(Severity::Info, None, Some(message.into()))
    }
    
    pub fn help(self, message: impl Into<String>) -> Diagnostic {
        self.label(Severity::Help, None, Some(message.into()))
    }
}

impl Severity {
    pub fn color(&self) -> termcolor::ColorSpec {
        let mut spec = termcolor::ColorSpec::new();
        
        match self {
            Severity::Bug => spec.set_fg(Some(termcolor::Color::Red)).set_intense(true),
            Severity::Error => spec.set_fg(Some(termcolor::Color::Red)),
            Severity::Warning => spec.set_fg(Some(termcolor::Color::Yellow)),
            Severity::Info => spec.set_fg(Some(termcolor::Color::Cyan)),
            Severity::Help => spec.set_fg(Some(termcolor::Color::Green)).set_intense(true),
        };
        
        spec
    }

    pub fn to_string(&self) -> &'static str {
        match self {
            Severity::Bug => "bug",
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Info => "info",
            Severity::Help => "help",
        }
    }
}

pub trait IntoOption<T = String> {
    fn into_option(self) -> Option<T>;
}

impl<A, B: Into<A>> IntoOption<A> for Option<B> {
    #[inline]
    fn into_option(self) -> Option<A> {
        self.map(|t| t.into())
    }
}

impl IntoOption for String {
    #[inline]
    fn into_option(self) -> Option<String> {
        Some(self)
    }
}

impl IntoOption for &str {
    #[inline]
    fn into_option(self) -> Option<String> {
        Some(String::from(self))
    }
}
