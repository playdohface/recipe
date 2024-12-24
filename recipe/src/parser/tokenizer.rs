use std::ops::RangeFrom;

use anyhow::anyhow;
use convert_case::{Case, Casing};
use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take_till, take_while},
    character::complete::{char, line_ending, space0, space1},
    combinator::fail,
    multi::many1_count,
    sequence::{delimited, preceded, terminated},
    IResult, InputLength, Parser, Slice,
};
use nom_locate::position;

use crate::loader;

use super::{Heading, Keyword, Link, Span, Token, TokenType};

#[derive(Debug)]
pub struct Tokenizer<'a> {
    /// The path being tokenized
    pub origin_path: &'a str,
    src: Span<'a>,
    scope: usize,
}

impl<'a> Tokenizer<'a> {
    pub fn from_str(src: &'a str, path: &'a str) -> Self {
        Tokenizer {
            origin_path: path,
            src: src.into(),
            scope: 0,
        }
    }
    /// Only return the Tokens found inside a given heading
    /// The heading-slug is the kebab-case version of the heading text
    fn scope_to_heading(&mut self, heading_slug: &str) {
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
                TokenType::Heading(Heading { level, .. }) if level <= self.scope => {
                    return None;
                }
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
    let (inp, start) = position(inp)?;
    let (inp, level) = preceded(terminated(line_ending, space0), many1_count(char('#')))(inp)?;
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
            end: position(inp)?.1,
            inner: TokenType::Heading(heading),
        },
    ))
}

/// Parse a raw codeblock (delimited by trtiple backticks)
fn parse_block(inp: Span) -> IResult<Span, Token> {
    let (rest, res) = delimited(tag("```"), take_while(|c| c != '`'), tag("```"))(inp)?;
    Ok((
        rest,
        Token {
            start: position(res)?.1,
            end: position(rest)?.1,
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
            start: position(res)?.1,
            end: position(rest)?.1,
            inner: TokenType::Inline(res.fragment()),
        },
    ))
}

/// Parse a Markdown-link
fn parse_link(inp: Span) -> IResult<Span, Token> {
    let (inp, start) = position(inp)?;
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
            end: position(inp)?.1,
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
            start: position(res)?.1,
            end: position(rest)?.1,
            inner: TokenType::Keyword(keyword),
        },
    ))
}

fn parse_newline(inp: Span) -> IResult<Span, Token> {
    let (rest, _newline) = line_ending(inp)?;
    Ok((
        rest,
        Token {
            start: position(inp)?.1,
            end: position(rest)?.1,
            inner: TokenType::Newline,
        },
    ))
}

#[cfg(test)]
mod tests {
    use nom::Offset;
    use nom_locate::LocatedSpan;

    use super::*;

    #[test]
    fn test_scope_to_heading() {
        let src = r#"
# Heading 1
`inside heading 1`
## Heading 2 please ScopeMe-to_ThisHeading
`inside heading 2`
### Heading 3
`inside heading 3`
## Another heading
`inside another heading`
"#;
        let mut tokenizer = Tokenizer::from_str(src, "Mock.md");
        tokenizer.scope_to_heading("heading-2-please-scope-me-to-this-heading");
        let tokens: Vec<TokenType> = tokenizer.map(|t| t.inner).collect();
        assert_eq!(
            tokens,
            vec![
                TokenType::Inline("inside heading 2"),
                TokenType::Heading(Heading {
                    level: 3,
                    text: "Heading 3"
                }),
                TokenType::Inline("inside heading 3"),
            ]
        );
    }

    #[test]
    fn test_tokenizer() {
        let src = r#"
#Heading 1
Some text
```rust
### Not a heading
fn main() {
    println!("Hello, World!");
}
```
Some more text
[Link](https://example.com)
Set `foo` to `bar`
"#;
        let tokenizer = Tokenizer::from_str(src, "Mock.md");
        let tokens: Vec<TokenType> = tokenizer.map(|t| t.inner).collect();
        assert_eq!(
            tokens,
            vec![
                TokenType::Heading(Heading {
                    level: 1,
                    text: "Heading 1"
                }),
                TokenType::Newline,
                TokenType::Block(
                    "rust\n### Not a heading\nfn main() {\n    println!(\"Hello, World!\");\n}\n"
                ),
                TokenType::Newline,
                TokenType::Newline,
                TokenType::Link(Link {
                    text: "Link",
                    path: "https://example.com"
                }),
                TokenType::Newline,
                TokenType::Keyword(Keyword::Set),
                TokenType::Inline("foo".into()),
                TokenType::Inline("bar".into()),
                TokenType::Newline,
            ]
        );
    }

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
        let (rest, parsed) = assert_parse_token_with_inner(&undertest, parse_heading, expected);
        assert_eq!(
            parsed.start.offset(&rest),
            undertest.len() - rest.fragment().len()
        );
        assert_eq!(*rest.fragment(), "and live to tell the tale");
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
