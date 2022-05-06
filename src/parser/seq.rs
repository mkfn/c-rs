#![allow(unused)]

use crate::lexer::Group;
use crate::{Span, Token, TokenStream};

#[derive(Clone, Copy)]
pub struct Seq<'a> {
    idx: usize,
    data: &'a [(Token, Span)],
}

impl<'a> Seq<'a> {
    pub fn from_tokenstream(ts: &'a TokenStream) -> Self {
        Seq {
            idx: 0,
            data: ts.slice(),
        }
    }
    pub fn from_group(g: &'a Group) -> Self {
        Seq {
            idx: 0,
            data: g.slice(),
        }
    }
    pub fn forward(&mut self) -> bool {
        if self.idx == self.data.len() {
            false
        } else {
            self.idx += 1;
            true
        }
    }
    pub fn _peek(&self, offset: usize) -> Option<&(Token, Span)> {
        self.data.get(self.idx + offset)
    }
    pub fn _peek_token(&self, offset: usize) -> Option<&Token> {
        self._peek(offset).map(|(t, _)| t)
    }
    pub fn _peek_span(&self, offset: usize) -> Option<&Span> {
        self._peek(offset).map(|(_, s)| s)
    }
    pub fn peek(&self) -> Option<&(Token, Span)> {
        self._peek(0)
    }
    pub fn peek1(&self) -> Option<&(Token, Span)> {
        self._peek(1)
    }
    pub fn peek2(&self) -> Option<&(Token, Span)> {
        self._peek(2)
    }
    pub fn peek3(&self) -> Option<&(Token, Span)> {
        self._peek(3)
    }
    pub fn peek_token(&self) -> Option<&Token> {
        self._peek_token(0)
    }
    pub fn peek_token1(&self) -> Option<&Token> {
        self._peek_token(1)
    }
    pub fn peek_token2(&self) -> Option<&Token> {
        self._peek_token(2)
    }
    pub fn peek_token3(&self) -> Option<&Token> {
        self._peek_token(3)
    }
    pub fn peek_span(&self) -> Option<&Span> {
        self._peek_span(0)
    }
    pub fn peek_span1(&self) -> Option<&Span> {
        self._peek_span(1)
    }
    pub fn peek_span2(&self) -> Option<&Span> {
        self._peek_span(2)
    }
    pub fn peek_span3(&self) -> Option<&Span> {
        self._peek_span(3)
    }
}
