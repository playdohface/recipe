use super::{Span, Token};

#[derive(Debug)]
pub enum ParseError<'a> {
    Generic(Span<'a>),
    InvalidToken(Token<'a>),
    InvalidCodeBlock(Span<'a>),
    NomError(nom::error::VerboseError<Span<'a>>),
}
