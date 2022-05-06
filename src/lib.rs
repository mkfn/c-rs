mod lexer;

pub use lexer::{Span, Token, TokenStream};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum CError {
    #[error("lex error")]
    LexError,
    #[error("parse error")]
    ParseError,
    #[error("runtime error")]
    RuntimeError,
}
