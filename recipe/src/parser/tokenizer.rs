use std::ops::RangeFrom;
use std::path::Path;

use convert_case::{Case, Casing};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_till, take_while},
    character::complete::{char, line_ending, space0, space1},
    combinator::{fail, success},
    multi::many1_count,
    sequence::{delimited, preceded, terminated},
    IResult, InputLength, Parser, Slice,
};
use nom_locate::position;

use super::{Heading, Keyword, Link, Span, Token, TokenType};

#[derive(Debug)]
pub struct Tokenizer<'a> {
    /// The path being tokenized
    pub origin_path: &'a Path,
    src: Span<'a>,
    scope: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn from_str(src: &'a str, path: &'a Path) -> Self {
        Tokenizer {
            origin_path: path,
            src: src.into(),
            scope: 0,
        }
    }
    /// Only return the Tokens found inside a given heading
    /// The heading-slug is the kebab-case version of the heading text
    pub fn scope_to_heading(&mut self, heading_slug: &str) {
        loop {
            match self.next() {
                Some(Token {
                    inner: TokenType::Heading(Heading { text, level, .. }),
                    ..
                }) if text.to_case(Case::Kebab) == heading_slug => {
                    self.scope = level;
                    break;
                }
                Some(_) => continue,
                None => break,
            }
        }
    }
}
impl<'a> Iterator for Tokenizer<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Ok((rest, found)) = find_next(any_token)(self.src) {
            match found.inner {
                TokenType::Heading(Heading { level, .. }) if level <= self.scope => None,
                _ => {
                    self.src = rest;
                    Some(found)
                }
            }
        } else {
            None
        }
    }
}

/// Eat input until the parser succeeds
fn find_next<I, O, F>(mut f: F) -> impl FnMut(I) -> IResult<I, O>
where
    F: FnMut(I) -> IResult<I, O>,
    I: Clone + InputLength + Slice<RangeFrom<usize>>,
{
    move |mut inp: I| {
        while inp.input_len() > 0 {
            if let Ok(found) = f.parse(inp.clone()) {
                return Ok(found);
            } else {
                inp = inp.slice(1..);
            }
        }
        fail(inp)
    }
}

fn any_token(inp: Span) -> IResult<Span, Token> {
    alt((
        parse_heading,
        parse_block,
        parse_inline,
        parse_link,
        parse_keyword,
        parse_newline,
    ))(inp)
}

// Parse a markdown heading
fn parse_heading(inp: Span) -> IResult<Span, Token> {
    let start = position(inp)?.1.location_offset();
    let (inp, _) = alt((at_start_of_input, line_ending))(inp)?;
    let (inp, _) = take_till(|c: char| !c.is_whitespace())(inp)?;
    let (inp, level) = many1_count(char('#'))(inp)?;
    let (inp, _) = take_till(|c: char| !c.is_whitespace())(inp)?;
    let (inp, text) = is_not("\r\n")(inp)?;
    let (inp, _) = line_ending(inp)?;

    let heading = Heading {
        level,
        text: text.trim_end(),
    };
    Ok((
        inp,
        Token {
            start,
            end: position(inp)?.1.location_offset(),
            inner: TokenType::Heading(heading),
        },
    ))
}

fn at_start_of_input(input: Span) -> IResult<Span, Span> {
    if input.location_line() == 1 && input.get_column() == 1 {
        Ok((input, "".into()))
    } else {
        fail(input)
    }
}

/// Parse a raw codeblock (delimited by trtiple backticks)
fn parse_block(inp: Span) -> IResult<Span, Token> {
    let (rest, res) = delimited(tag("```"), take_while(|c| c != '`'), tag("```"))(inp)?;
    Ok((
        rest,
        Token {
            start: position(res)?.1.location_offset(),
            end: position(rest)?.1.location_offset(),
            inner: TokenType::Block(res.fragment()),
        },
    ))
}

fn parse_inline(inp: Span) -> IResult<Span, Token> {
    let (rest, res) = delimited(
        tag("`"),
        take_while(|c| c != '`' && !is_line_end(c)),
        tag("`"),
    )(inp)?;
    Ok((
        rest,
        Token {
            start: position(res)?.1.location_offset(),
            end: position(rest)?.1.location_offset(),
            inner: TokenType::Inline(res.fragment()),
        },
    ))
}

/// Parse a Markdown-link
fn parse_link(inp: Span) -> IResult<Span, Token> {
    let start = position(inp)?.1.location_offset();
    let (inp, text) = delimited(tag("["), take_while(is_allowed_in_link_text), tag("]"))(inp)?;
    let (inp, path) = delimited(tag("("), take_while(is_allowed_in_link_path), tag(")"))(inp)?;
    let link = TokenType::Link(Link {
        text: &text,
        path: &path,
    });
    Ok((
        inp,
        Token {
            start,
            end: position(inp)?.1.location_offset(),
            inner: link,
        },
    ))
}

fn is_allowed_in_link_text(c: char) -> bool {
    c != ']' && !is_line_end(c)
}

fn is_allowed_in_link_path(c: char) -> bool {
    c != ')' && !is_line_end(c)
}

fn is_line_end(c: char) -> bool {
    c == '\n' || c == '\r'
}

fn parse_keyword(inp: Span) -> IResult<Span, Token> {
    let (rest, res) = terminated(
        alt((
            tag("To"),      //
            tag("Set"),     //
            tag("Execute"), //
        )),
        alt((space1, line_ending)),
    )(inp)?;
    let keyword = match *res.fragment() {
        "To" => Keyword::To,
        "Set" => Keyword::Set,
        "Execute" => Keyword::Execute,
        _ => unreachable!(),
    };
    Ok((
        rest,
        Token {
            start: position(res)?.1.location_offset(),
            end: position(rest)?.1.location_offset(),
            inner: TokenType::Keyword(keyword),
        },
    ))
}

fn parse_newline(inp: Span) -> IResult<Span, Token> {
    let (rest, _newline) = line_ending(inp)?;
    Ok((
        rest,
        Token {
            start: position(inp)?.1.location_offset(),
            end: position(rest)?.1.location_offset(),
            inner: TokenType::Newline,
        },
    ))
}

#[cfg(test)]
mod tests {
    use nom_locate::LocatedSpan;

    use super::*;

    #[test]
    fn test_find_next() {
        let src = "foovrfs barand more";
        let (rest, res) = find_next(tag("bar"))(src).unwrap();
        assert_eq!(res, "bar");
        assert_eq!(rest, "and more");
        assert!(find_next(tag("foo"))("haystack with no match").is_err());
    }

    #[test]
    fn test_parse_heading() {
        let undertest = "\n #### \t How To Bake a Cake\t  \r\nand live to tell the tale";
        let expected = TokenType::Heading(Heading {
            level: 4,
            text: "How To Bake a Cake",
        });
        let (rest, _parsed) = assert_parse_token_with_inner(&undertest, parse_heading, expected);
        assert_eq!(*rest.fragment(), "and live to tell the tale");
    }

    #[test]
    fn test_parse_heading_at_start_of_input() {
        let undertest = " #### \t How To Bake a Cake\t  \r\nand live to tell the tale";
        let expected = TokenType::Heading(Heading {
            level: 4,
            text: "How To Bake a Cake",
        });
        let (rest, _parsed) = assert_parse_token_with_inner(&undertest, parse_heading, expected);
        assert_eq!(*rest.fragment(), "and live to tell the tale");
    }

    #[test]
    fn test_at_beginning_of_file() {
        let undertest = "\n #### \t How To Bake a Cake\t  \r\nand live to tell the tale".into();
        let (rest, parsed) = at_start_of_input(undertest).unwrap();
        assert_eq!(undertest, rest);
        assert_eq!(parsed, "".into());
        let (rest, _) = line_ending::<_, ()>(rest).unwrap();
        assert!(at_start_of_input(rest).is_err());
    }

    #[test]
    fn test_parse_link() {
        let undertest = "[Foo](bar) and more";
        let expected = TokenType::Link(Link {
            text: "Foo",
            path: "bar",
        });
        let (rest, _parsed) = assert_parse_token_with_inner(undertest, parse_link, expected);
        assert_eq!(*rest.fragment(), " and more");
    }

    #[test]
    fn test_parse_block() {
        let undertest = "```\nfoo\nbar\n``` and more";
        let expected = TokenType::Block("\nfoo\nbar\n".into());
        let (rest, _parsed) = assert_parse_token_with_inner(undertest, parse_block, expected);
        assert_eq!(*rest.fragment(), " and more");
    }

    #[test]
    fn test_parse_inline() {
        let undertest = "`foo` and more";
        let expected = TokenType::Inline("foo".into());
        let (rest, _parsed) = assert_parse_token_with_inner(undertest, parse_inline, expected);
        assert_eq!(*rest.fragment(), " and more");
    }

    #[test]
    fn test_parse_keyword() {
        let undertest = "To \n";
        let expected = TokenType::Keyword(Keyword::To);
        let (rest, _parsed) = assert_parse_token_with_inner(undertest, parse_keyword, expected);
        assert_eq!(*rest.fragment(), "\n");
    }

    #[test]
    fn test_parse_newline() {
        let undertest = "\n";
        let expected = TokenType::Newline;
        let (rest, _parsed) = assert_parse_token_with_inner(undertest, parse_newline, expected);
        assert_eq!(*rest.fragment(), "");
    }

    pub(crate) fn assert_parse_token_with_inner<'a>(
        src: &'a str,
        parser: impl Fn(Span) -> IResult<Span, Token>,
        expected_inner: TokenType,
    ) -> (Span<'a>, Token<'a>) {
        let undertest = LocatedSpan::new(src);
        let (rest, parsed) = parser(undertest).unwrap();
        let _f: Result<
            (LocatedSpan<&str>, Token<'_>),
            nom::Err<nom::error::Error<LocatedSpan<&str>>>,
        > = parser(undertest);
        assert_eq!(parsed.inner, expected_inner);
        (rest, parsed)
    }
}
