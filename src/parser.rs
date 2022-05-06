mod seq;
pub use seq::Seq;

use crate::TokenStream;
use anyhow::Result;

pub struct Ast;

pub trait Parse {
    fn parse(tokens: &TokenStream) -> Result<Self>
    where
        Self: Sized;
}

pub fn parse(ts: &TokenStream) -> Result<Ast> {
    let mut _seq = Seq::from_tokenstream(ts);
    Ok(Ast)
}
