#![feature(ptr_offset_from)]

pub mod parse;
pub mod token;
pub mod error;
pub mod buffer;
pub mod ident;
pub mod punct;
pub mod literal;
pub mod lexer;

pub use diagnostics;

pub fn parse<T: parse::Parse + Default>(
    source: &str,
    file: diagnostics::FileId,
    reporter: &diagnostics::Reporter,
    start: Option<diagnostics::Span>
) -> T {
    use diagnostics::Spanned;
    
    let mut lexer = lexer::Lexer::new(source, file, reporter);
    let buffer = lexer.run();
    let stream = parse::ParseBuffer::new(buffer.begin(), reporter, (), if let Some(start) = start {
        start
    } else if !buffer.tokens.is_empty() {
        buffer.tokens[0].span()
    } else {
        Default::default()
    });
    
    if let Ok(res) = stream.parse() {
        if !stream.is_empty() {
        }
        
        res
    } else {
        T::default()
    }
}

// pub fn parse_buffer<T: parse::Parse>(buffer: &buffer::TokenBuffer, flags: usize, start: Option<diagnostics::Span>) -> error::Result<T> {
//     let mut stream = parse::ParseBuffer::new(buffer.begin(), if let Some(start) = start {
//         start
//     } else if !buffer.tokens.is_empty() {
//         buffer.tokens[0].span()
//     } else {
//         Default::default()
//     });
    
//     stream.flags = flags;
    
//     stream.parse()
// }
