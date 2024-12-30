use super::{Span, Token, TokenType};

#[derive(Debug)]
pub enum ParseError<'a> {
    Generic(&'a str),
    InvalidToken(Token<'a>),
    InvalidCodeBlock(&'a str),

    // Errors converting to a value
    InvalidValue(&'a str),
    InvalidLiteral((&'a str, json5::Error)),
    InvalidTokenTypeForValue(TokenType<'a>),

    InvalidSelection(nom::Err<nom::error::Error<&'a str>>),

    NomError(nom::error::VerboseError<Span<'a>>),
}
