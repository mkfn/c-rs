use crate::CError;
use anyhow::{Context, Result};

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct SpanPos {
    line: usize,
    column: usize,
}

impl SpanPos {
    fn new() -> Self {
        Self { line: 0, column: 0 }
    }

    fn _forward(&mut self, step: usize) {
        self.column += step;
    }

    fn forward(&mut self) {
        self._forward(1);
    }

    fn space(&mut self) {
        self._forward(1);
    }

    fn tab(&mut self) {
        self.column = self.column / 4 * 4;
        self._forward(4);
    }

    fn newline(&mut self) {
        self.line += 1;
        self.column = 0;
    }
}

impl std::fmt::Display for SpanPos {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}

#[derive(Clone, Copy, Debug, PartialEq)]
pub struct Span {
    start: SpanPos, // line, column
    end: SpanPos,   // line, column
}

impl std::fmt::Display for Span {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "{}-{}", self.start, self.end)
    }
}

#[derive(Debug, PartialEq)]
pub enum Delimiter {
    Paren,
    Brace,
    Bracket,
}

#[derive(Debug, PartialEq)]
pub struct Group {
    delimiter: Delimiter,
    tokens: Vec<(Token, Span)>,
}

impl Group {
    fn new(c: char) -> Group {
        Group {
            delimiter: match c {
                '(' => Delimiter::Paren,
                '{' => Delimiter::Brace,
                '[' => Delimiter::Bracket,
                _ => unreachable!(),
            },
            tokens: Vec::new(),
        }
    }

    fn match_delimiter(&self, c: char) -> bool {
        match self.delimiter {
            Delimiter::Paren => c == ')',
            Delimiter::Brace => c == '}',
            Delimiter::Bracket => c == ']',
        }
    }

    fn push(&mut self, token: Token, span: Span) {
        self.tokens.push((token, span));
    }

    pub fn slice(&self) -> &[(Token, Span)] {
        &self.tokens
    }
}

#[derive(Debug, PartialEq)]
pub enum Lit {
    Int(i32),
    Float(f32),
}

#[derive(Debug, PartialEq)]
pub struct Ident {
    name: String,
}

#[derive(Debug, PartialEq)]
pub enum Punct {
    Comma,        // ,
    Dot,          // .
    Semicolon,    // ;
    Plus,         // +
    Minus,        // -
    Star,         // *
    Slash,        // /
    Modulo,       // %
    Caret,        // ^
    BAnd,         // &
    BOr,          // |
    Tilde,        // ~
    Not,          // !
    Question,     // ?
    Arrow,        // ->
    Eq,           // =
    Ne,           // !=
    Lt,           // <
    Gt,           // >
    Le,           // <=
    Ge,           // >=
    And,          // &&
    Or,           // ||
    Assign,       // =
    PlusAssign,   // +=
    MinusAssign,  // -=
    StarAssign,   // *=
    SlashAssign,  // /=
    ModuloAssign, // %=
    CaretAssign,  // ^=
    BAndAssign,   // &=
    BOrAssign,    // |=
}

#[derive(Debug, PartialEq)]
pub enum Token {
    Group(Group),
    Lit(Lit),
    Ident(Ident),
    Punct(Punct),
}

pub struct TokenStream {
    codelines: Vec<String>,
    tokens: Vec<(Token, Span)>,
}

impl TokenStream {
    fn new(code: &str) -> Self {
        let lines: Vec<_> = code.split('\n').map(|l| remove_comments(l)).collect();
        let mut codelines = Vec::new();
        for line in lines {
            codelines.push(line.to_string());
        }
        Self {
            codelines,
            tokens: Vec::new(),
        }
    }

    fn push(&mut self, token: Token, span: Span) {
        self.tokens.push((token, span));
    }

    pub fn display(&self, span: Span) -> String {
        if span.start.line == span.end.line {
            let bytes = self.codelines[span.start.line].as_bytes();
            let bytes = &bytes[span.start.column..span.end.column];
            String::from_utf8(bytes.to_vec()).unwrap()
        } else {
            let mut bs_vec = Vec::new();
            let start_bs = &self.codelines[span.start.line].as_bytes()[span.start.column..];
            bs_vec.push(String::from_utf8(start_bs.to_vec()).unwrap());
            for i in span.start.line + 1..span.end.line {
                bs_vec.push(self.codelines[i].to_string());
            }
            let end_bs = &self.codelines[span.end.line].as_bytes()[..span.end.column];
            bs_vec.push(String::from_utf8(end_bs.to_vec()).unwrap());
            bs_vec.join("\n")
        }
    }

    pub fn slice(&self) -> &[(Token, Span)] {
        &self.tokens
    }
}

#[derive(Copy, Clone, PartialEq)]
enum BufType {
    Punct,
    AlphaNumeric,
    None,
}

/// remove comments: //
fn remove_comments(line: &str) -> String {
    let mut result = String::new();
    let mut last_slash = false;
    for c in line.chars() {
        if c == '/' {
            if last_slash {
                result.pop();
                break;
            }
            last_slash = true;
        }
        result.push(c);
    }
    result
}

/// converts code into a token stream
pub fn lex(code: &str) -> Result<TokenStream> {
    // Group Stack
    let mut stream = TokenStream::new(code);
    let mut groups: Vec<(Group, SpanPos)> = Vec::new();
    let mut cont_buf = String::new();
    let mut buf_reading = BufType::None;
    let mut cur_pos = SpanPos::new();
    let mut last_pos = SpanPos::new();
    let x = groups.last_mut();
    macro_rules! push_token {
        () => {
            if let Some(t) = try_parse_token(&cont_buf)? {
                let span = Span {
                    start: last_pos,
                    end: cur_pos,
                };
                if let Some((g, _)) = groups.last_mut() {
                    g.push(t, span);
                } else {
                    stream.push(t, span);
                }
            }
            cont_buf.clear();
            buf_reading = BufType::None;
        };
    }
    for c in code.chars() {
        if c == ' ' || c == '\t' || c == '\n' {
            push_token!();
            match c {
                ' ' => cur_pos.space(),
                '\t' => cur_pos.tab(),
                '\n' => cur_pos.newline(),
                _ => unreachable!(),
            }
            continue;
        } else if c == '(' || c == '{' || c == '[' {
            push_token!();
            groups.push((Group::new(c), cur_pos));
        } else if c == ')' || c == '}' || c == ']' {
            push_token!();
            if let Some((g, pos)) = groups.pop() {
                if g.match_delimiter(c) {
                    cur_pos.forward();
                    stream.push(
                        Token::Group(g),
                        Span {
                            start: pos,
                            end: cur_pos,
                        },
                    );
                    continue;
                } else {
                    // Not corrcet match
                    return Err(CError::LexError)
                        .with_context(|| format!("not match {:?} {}", g.delimiter, c))?;
                }
            } else {
                // No match (
                return Err(CError::LexError)
                    .with_context(|| format!("no group match {}, pos: {}", c, cur_pos))?;
            }
        } else if c.is_alphanumeric() || c == '_' {
            if buf_reading == BufType::Punct {
                // remove punc
                push_token!();
            }
            cont_buf.push(c);
            // now not reading punct
            if buf_reading != BufType::AlphaNumeric {
                last_pos = cur_pos;
                buf_reading = BufType::AlphaNumeric;
            }
        } else {
            if buf_reading == BufType::AlphaNumeric {
                // remove alphanumeric
                push_token!();
            }
            cont_buf.push(c);
            // now reading punct
            if buf_reading != BufType::Punct {
                last_pos = cur_pos;
                buf_reading = BufType::Punct;
            }
        }
        cur_pos.forward();
    }
    push_token!();
    if groups.len() > 0 {
        // Not all groups closed
        return Err(CError::LexError).with_context(|| format!("not all groups closed"))?;
    }
    Ok(stream)
}

fn try_parse_token(input: &str) -> Result<Option<Token>> {
    if input.len() == 0 {
        Ok(None)
    } else {
        if let Some(t) = try_parse_ident(input) {
            return Ok(Some(Token::Ident(t)));
        }
        if let Some(t) = try_parse_punct(input) {
            return Ok(Some(Token::Punct(t)));
        }
        if let Some(t) = try_parse_lit(input) {
            return Ok(Some(Token::Lit(t)));
        }
        Err(CError::LexError).with_context(|| format!("token: {} parse error", input))?
    }
}

/// ensure input not empty
fn try_parse_punct(input: &str) -> Option<Punct> {
    match input {
        "," => Some(Punct::Comma),
        "." => Some(Punct::Dot),
        ";" => Some(Punct::Semicolon),
        "+" => Some(Punct::Plus),
        "-" => Some(Punct::Minus),
        "*" => Some(Punct::Star),
        "/" => Some(Punct::Slash),
        "%" => Some(Punct::Modulo),
        "^" => Some(Punct::Caret),
        "&" => Some(Punct::BAnd),
        "|" => Some(Punct::BOr),
        "~" => Some(Punct::Tilde),
        "!" => Some(Punct::Not),
        "?" => Some(Punct::Question),
        "->" => Some(Punct::Arrow),
        "=" => Some(Punct::Eq),
        "!=" => Some(Punct::Ne),
        "<" => Some(Punct::Lt),
        ">" => Some(Punct::Gt),
        "<=" => Some(Punct::Le),
        ">=" => Some(Punct::Ge),
        "&&" => Some(Punct::And),
        "||" => Some(Punct::Or),
        "=" => Some(Punct::Assign),
        "+=" => Some(Punct::PlusAssign),
        "-=" => Some(Punct::MinusAssign),
        "*=" => Some(Punct::StarAssign),
        "/=" => Some(Punct::SlashAssign),
        "%=" => Some(Punct::ModuloAssign),
        "^=" => Some(Punct::CaretAssign),
        "&=" => Some(Punct::BAndAssign),
        "|=" => Some(Punct::BOrAssign),
        _ => None,
    }
}

/// ensure input not empty
fn try_parse_ident(input: &str) -> Option<Ident> {
    let mut first = true;
    for c in input.chars() {
        if first {
            if !c.is_alphabetic() && c != '_' {
                return None;
            }
            first = false;
        }
        if !c.is_alphanumeric() && c != '_' {
            return None;
        }
    }
    Some(Ident {
        name: input.to_owned(),
    })
}

/// ensure input not empty
fn try_parse_lit(input: &str) -> Option<Lit> {
    if input.starts_with("0x") {
        i32::from_str_radix(&input[2..], 16)
            .ok()
            .map(|i| Lit::Int(i))
    } else if input.starts_with("0b") {
        i32::from_str_radix(&input[2..], 2)
            .ok()
            .map(|i| Lit::Int(i))
    } else if input.starts_with("0") {
        i32::from_str_radix(&input[1..], 8)
            .ok()
            .map(|i| Lit::Int(i))
    } else {
        if let Some(t) = input.parse::<i32>().ok() {
            return Some(Lit::Int(t));
        }
        if let Some(t) = input.parse::<f32>().ok() {
            return Some(Lit::Float(t));
        }
        None
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn basic() {
        let code = "
            int a = 1 + 1;
            int b=2+2;
            b += a;
        ";
        let ts_res = lex(code);
        assert!(ts_res.is_ok());
        let ts = ts_res.unwrap();
        if let Some((t, s)) = ts.tokens.first() {
            assert_eq!(
                t,
                &Token::Ident(Ident {
                    name: "int".to_owned()
                })
            );
            assert_eq!(
                s,
                &Span {
                    start: SpanPos {
                        line: 1,
                        column: 12
                    },
                    end: SpanPos {
                        line: 1,
                        column: 15
                    },
                }
            );
            assert_eq!("int", ts.display(*s));
        }
        // snip compare all

        // if let Some((t, s)) = ts.tokens.last() {
        //     assert_eq!(t, &Token::Ident(Ident { name: "int".to_owned() }));
        //     assert_eq!(s, "b");
        // }
        assert_eq!(18, ts.tokens.len());
    }

    #[test]
    fn group() {
        let code = "
            int a = 1 + 1;
            int b=2+2;
            {
                b += a;
            }
        ";
        let ts_res = lex(code);
        assert!(ts_res.is_ok());
        let ts = ts_res.unwrap();
        assert_eq!(15, ts.tokens.len());
        if let Some((Token::Group(g), s)) = ts.tokens.last() {
            assert_eq!(4, g.tokens.len());
            assert_eq!(3, s.start.line);
            assert_eq!(12, s.start.column);
            assert_eq!(5, s.end.line);
            assert_eq!(13, s.end.column);
            assert_eq!(
                "{
                b += a;
            }",
                ts.display(*s)
            );
        } else {
            panic!("last token is not group");
        }
        // for (t, span) in ts.tokens.iter() {
        //     println!("{:?} {}", t, span);
        // }
    }

    #[test]
    fn err() {
        assert!(lex("0asb;").is_err());
        assert!(lex("(").is_err());
        assert!(lex("())").is_err());
        assert!(lex("([))").is_err());
    }
}
