use super::span::Span;
use colored::*;

pub trait ToDiagnostic {
    fn to_diagnostic(&self) -> Diagnostic;
}

#[derive(Debug, Clone)]
pub struct Diagnostic {
    pub span: Span,
    pub severity: Severity,
    pub message: String,
    pub labels: Vec<Label>,
}

#[derive(Debug, Clone)]
pub struct Label {
    pub severity: Severity,
    pub span: Span,
    pub message: Option<String>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Severity {
    Bug,
    Error,
    Warning,
    Info,
    Help,
}

struct LineInfo {
    num: usize,
    text: String,
    labels: Vec<Label>,
}

impl Severity {
    pub fn color(&self) -> &'static str {
        match self {
            Severity::Bug => "bright red",
            Severity::Error => "red",
            Severity::Warning => "yellow",
            Severity::Info => "bright cyan",
            Severity::Help => "bright green",
        }
    }
}

impl Diagnostic {
    pub fn new(
        severity: Severity,
        span: Span,
        message: impl Into<String>,
    ) -> Diagnostic {
        Diagnostic {
            severity,
            span,
            message: message.into(),
            labels: Vec::new(),
        }
    }
    
    pub fn new_derived(
        severity: Severity,
        span: Span,
        message: String,
        labels: Vec<Label>,
    ) -> Diagnostic {
        Diagnostic {
            severity,
            span,
            message,
            labels,
        }
    }
    
    pub fn width_label(mut self, severity: Severity, span: Span, message: Option<impl Into<String>>) -> Diagnostic {
        self.labels.push(Label {
            severity,
            span,
            message: if let Some(m) = message { Some(m.into()) } else { None }
        });
        
        self
    }
    
    pub fn with_info(mut self, span: Option<Span>, message: Option<impl Into<String>>) -> Diagnostic {
        self.labels.push(Label {
            severity: Severity::Info,
            span: if let Some(span) = span { span } else { Span::default() },
            message: if let Some(m) = message { Some(m.into()) } else { None }
        });
        
        self
    }
    
    pub fn with_help(mut self, span: Option<Span>, message: Option<impl Into<String>>) -> Diagnostic {
        self.labels.push(Label {
            severity: Severity::Help,
            span: if let Some(span) = span { span } else { Span::default() },
            message: if let Some(m) = message { Some(m.into()) } else { None }
        });
        
        self
    }
    
    pub fn print(&self, filename: &str, source: &str) {
        let color = self.severity.color();
        
        print!("{}", match self.severity {
            Severity::Bug => "bug",
            Severity::Error => "error",
            Severity::Warning => "warning",
            Severity::Info => "info",
            Severity::Help => "help"
        }.color(color).bold());
        
        println!(": {}.", self.message.white().bold());
        self.print_filename(filename);
        self.print_code(source);
    }
    
    fn print_filename(&self, filename: &str) {
        let cdir = std::env::current_dir().unwrap().to_str().unwrap().to_string();
        
        print!("{}{} ", " ".repeat(self.line_nums_width()), "-->".blue().bold());
        
        if filename.starts_with(&cdir) {
            println!("{}:{}:{}", &filename[cdir.len() + 1..], self.span.start.line + 1, self.span.start.col + 1);
        } else {
            println!("{}:{}:{}", filename, self.span.start.line + 1, self.span.start.col + 1);
        }
    }
    
    fn print_code(&self, src: &str) {
        let color = self.severity.color();
        let lines = src.lines().collect::<Vec<_>>();
        let mut indent = std::usize::MAX;
        let lines = lines
            .iter()
            .enumerate()
            .filter(|(i, line)| {
                let mut m = false;
                
                m |= *i == self.span.start.line || *i == self.span.end.line;
                
                for label in &self.labels {
                    if label.span != Span::default() {
                        m |= *i == label.span.start.line || *i == label.span.end.line;
                    }
                }
                
                let ind = {
                    let mut chars = line.chars();
                    let mut j = 0;
                    
                    while let Some(c) = chars.next() {
                        match c {
                            ' ' | '\t' => {
                                j += 1;
                                continue;
                            },
                            _ => break
                        }
                    }
                    
                    j
                };
                
                if ind < indent {
                    indent = ind;
                }
                
                m
            })
            .collect::<Vec<_>>();
        let lines: Vec<(usize, String)> = lines
            .into_iter()
            .map(|(i, l)| {
                let l = if i == self.span.start.line && i == self.span.end.line {
                    format!("{}{}{}", &l[indent..self.span.start.col], &l[self.span.start.col..self.span.end.col].color(color).bold(), &l[self.span.end.col..])
                } else if i == self.span.start.line {
                    format!("{}{}", &l[indent..self.span.start.col], &l[self.span.start.col..].color(color).bold())
                } else if i == self.span.end.line {
                    format!("{}{}", &l[indent..self.span.end.col].color(color).bold(), &l[self.span.end.col..])
                } else {
                    l.to_string()
                };
                
                (i, l)
            })
            .collect();
        
        let mut notes = Vec::new();
        let lines = lines.into_iter().map(|(i, line)| {
            let mut labels = self.labels.iter()
                .filter(|l| l.span.end.line == i)
                .filter(|l| if l.span == Span::default() {
                    if let Some(text) = &l.message {
                        notes.push((l.severity == Severity::Help, text.color(l.severity.color()).bold()));
                    }
                    
                    false
                } else {
                    true
                })
                .map(|l| {
                    let mut l = l.clone();
                    
                    l.span.start.col -= indent;
                    l.span.end.col -= indent;
                    
                    l
                })
                .collect::<Vec<_>>();
            
            labels.sort_by_key(|a| std::cmp::Reverse(a.span.clone()));
            
            LineInfo {
                num: i,
                text: line,
                labels
            }
        }).collect::<Vec<_>>();
        
        self.print_margin_empty(true);
        
        for line in lines {
            self.print_margin(line.num);
            println!("{}", line.text);
            
            let mut lines = Vec::new();
            
            for label in line.labels {
                let indent = " ".repeat(label.span.start.col);
                let symbol = match label.severity {
                    Severity::Bug => "⚠",
                    Severity::Error => "^",
                    Severity::Warning => "~",
                    Severity::Info => "-",
                    Severity::Help => "-"
                };
                let color = label.severity.color();
                let symbols_len = label.span.end.col - label.span.start.col;
                let symbols = symbol.repeat(symbols_len).color(color).bold();
                
                if lines.is_empty() {
                    lines.push(if let Some(text) = &label.message {
                        let text = match label.severity {
                            Severity::Info => format!("info: {}", text),
                            Severity::Help => format!("help: {}", text),
                            _ => text.to_string()
                        };
                        
                        format!("{}{} {}", indent, symbols, text.color(color).bold())
                    } else {
                        format!("{}{}", indent, symbols)
                    });
                } else {
                    string_insert(&mut lines[0], label.span.start.col, symbols_len, symbols.to_string());
                    
                    if let Some(text) = &label.message {
                        let text = match label.severity {
                            Severity::Info => format!("info: {}", text),
                            Severity::Help => format!("help: {}", text),
                            _ => text.to_string()
                        };
                        
                        for i in 1..lines.len() {
                            string_insert(&mut lines[i], label.span.start.col, 1, "|".color(color).bold().to_string());
                        }
                        
                        lines.push(format!("{}{}", indent, "|".color(color).bold()));
                        lines.push(format!("{}{}", indent, text.color(color).bold()));
                    }
                }
            }
            
            for line in lines {
                self.print_margin_empty(false);
                
                println!("{}", line);
            }
        }
        
        self.print_margin_empty(true);
        
        for (is_help, note) in notes {
            let ws = " ".repeat(self.line_nums_width());
            
            println!("{} {} {}: {}", ws, "=".blue().bold(), if is_help { "help" } else { "note" }.white().bold(), note);
        }
        
        println!();
    }
    
    fn print_margin(&self, line: usize) {
        let line = line + 1;
        let ws = " ".repeat(self.line_nums_width() - line.to_string().len());
        
        print!("{}{} ", ws, format!("{} |", line).blue().bold());
    }
    
    fn print_margin_dots(&self) {
        println!("{}", "•••".blue().bold());
    }
    
    fn print_margin_empty(&self, nl: bool) {
        print!("{} {}{}", " ".repeat(self.line_nums_width()), "|".blue().bold(), if nl { "\n" } else { " " });
    }
    
    fn line_nums_width(&self) -> usize {
        let mut max = self.span.start.line;
        
        if self.span.end.line > max {
            max = self.span.end.line;
        }
        
        for label in &self.labels {
            if label.span.start.line > max {
                max = label.span.start.line;
            }
            
            if label.span.end.line > max {
                max = label.span.end.line;
            }
        }
        
        max += 1;
        
        max.to_string().len()
    }
}

impl Default for Severity {
    fn default() -> Severity {
        Severity::Error
    }
}

fn is_empty(line: &str) -> bool {
    line.trim().is_empty()
}

fn abs(n: isize) -> usize {
    (if n < 0 {
        n * -1
    } else {
        n
    }) as usize
}

fn min(a: usize, b: usize) -> usize {
    if a < b {
        a
    } else {
        b
    }
}

fn string_insert(s: &mut String, i: usize, l: usize, new: String) {
    let start = &s[0..i];
    let end = &s[i + l..];
    
    *s = format!("{}{}{}", start, new, end);
}