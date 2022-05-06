#![feature(stmt_expr_attributes)]

mod lexer;
mod parser;

pub use lexer::{Span, Token, TokenStream};
use thiserror::Error;
use anyhow::Result;

#[derive(Error, Debug)]
pub enum CError {
    #[error("lex error")]
    LexError,
    #[error("parse error {0}")]
    ParseError(Span),
    #[error("runtime error")]
    RuntimeError,
}

pub fn interpret(code: &str) -> Result<()> {
    let tokens = lexer::lex(code)?;
    let _ = parser::parse(&tokens)?;
    todo!();
}
