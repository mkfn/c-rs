#![allow(unused)]
mod seq;
pub use seq::Seq;

use crate::TokenStream;
use macro_helper::*;

pub struct Ast;

pub trait Parse {
    fn parse(seq: &mut Seq) -> Result<Self, crate::Span>
    where
        Self: Sized;
}

pub fn parse(ts: &TokenStream) -> Result<Ast, crate::CError> {
    let mut _seq = Seq::from_tokenstream(ts);
    Ok(Ast)
}
