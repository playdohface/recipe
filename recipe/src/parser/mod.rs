use std::ops::RangeFrom;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take, take_till, take_until, take_while, take_while1},
    character::{
        complete::{char, line_ending, space0, space1},
        is_newline,
    },
    combinator::{fail, verify},
    error::{Error, ParseError},
    multi::many1_count,
    sequence::{delimited, preceded, terminated},
    IResult, InputIter, InputLength, InputTake, Parser, Slice,
};
use nom_locate::{position, LocatedSpan};

type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq, Eq)]
pub struct Heading<'a> {
    level: usize,
    text: &'a str,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CodeBlock<'a> {
    executor: Option<&'a str>,
    name: Option<&'a str>,
    type_hint: Option<&'a str>,
    annotations: Vec<&'a str>,
    code: &'a str,
}

#[derive(Debug, PartialEq, Eq)]
pub struct Link<'a> {
    text: &'a str,
    path: &'a str,
}

#[derive(Debug, PartialEq, Eq)]
pub struct ToDirective<'a> {
    callpath: Vec<&'a str>,
    args: Vec<&'a str>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType<'a> {
    Heading(Heading<'a>),
    Ident(&'a str),
    CodeBlock(CodeBlock<'a>),
    Link(Link<'a>),
    ToDirective(ToDirective<'a>),
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token<'a> {
    position: Span<'a>,
    inner: TokenType<'a>,
}

impl<'a> Token<'a> {
    pub fn inner(&self) -> &TokenType<'a> {
        &self.inner
    }
}

#[derive(Debug)]
pub struct RecipeParser<'a> {
    src: Span<'a>,
}

impl<'a> RecipeParser<'a> {
    pub fn from_str(src: &'a str) -> Self {
        RecipeParser { src: src.into() }
    }
}
impl<'a> Iterator for RecipeParser<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        if let Ok((rest, found)) = find_next(any_token)(self.src) {
            self.src = rest;
            Some(found)
        } else {
            None
        }
    }
}

fn any_token(inp: Span) -> IResult<Span, Token> {
    alt((
        parse_heading,
        parse_link,
        parse_code_block,
        parse_to_directive,
        parse_single_ident,
    ))(inp)
}

fn is_allowed_in_ident(c: char) -> bool {
    !c.is_whitespace() && c != '`'
}

fn is_allowed_in_name(c: char) -> bool {
    is_allowed_in_ident(c) && !['(', ')', '.', '[', ']', '{', '}'].contains(&c)
}

fn is_allowed_in_link_text(c: char) -> bool {
    !is_newline(c.try_into().unwrap_or_default()) && c != ']'
}

fn is_allowed_in_link_path(c: char) -> bool {
    !is_newline(c.try_into().unwrap_or_default()) && c != ')'
}

/// To-Directive: Starts with keyword "To" followed by at least one space.
/// finds valid names in backticks (\`name\`) until end of line (list of names is the callpath).
/// Optionally specifies required arguments as valid names preceded by 2 dashes(\`--name\`).
/// Callpath must be complete before the first argument, and must have at least one name in callpath before newline.
fn parse_to_directive(inp: Span) -> IResult<Span, Token, Error<LocatedSpan<&str>>> {
    let (inp, position) = position(inp)?;
    let (inp, _) = terminated(tag("To"), space1)(inp)?;
    let (mut inp, (callpath, end)) = find_many_till(
        delimited(tag("`"), valid_name, tag("`")),
        alt((tag("`--"), line_ending)),
        inp,
    )?;
    if callpath.is_empty() {
        return fail(position); // no elements in callpath means invalid directive
    }
    let mut args = Vec::new();
    if *end.fragment() == "`--" {
        let (rest, arg1) = terminated(valid_name, tag("`"))(inp)?;
        let (rest, (otherargs, _)) = find_many_till(
            delimited(tag("`--"), valid_name, tag("`")),
            line_ending,
            rest,
        )?;
        args.push(arg1);
        args.extend(otherargs);
        inp = rest;
    }

    let to_directive = ToDirective {
        callpath: callpath.into_iter().map(|e| *e.fragment()).collect(),
        args: args.into_iter().map(|e| *e.fragment()).collect(),
    };

    Ok((
        inp,
        Token {
            position,
            inner: TokenType::ToDirective(to_directive),
        },
    ))
}

fn parse_link(inp: Span) -> IResult<Span, Token> {
    let (inp, position) = position(inp)?;
    let (inp, text) = delimited(tag("["), take_while(is_allowed_in_link_text), tag("]"))(inp)?;
    let (inp, path) = delimited(tag("("), take_while(is_allowed_in_link_path), tag(")"))(inp)?;
    let link = TokenType::Link(Link {
        text: &text,
        path: &path,
    });
    Ok((
        inp,
        Token {
            position,
            inner: link,
        },
    ))
}

fn parse_code_block(inp: Span) -> IResult<Span, Token> {
    let (inp, position) = position(inp)?;
    let (inp, _) = tag("```")(inp)?;
    let (inp, executor) = valid_name(inp)?;
    let (inp, name) = preceded(space1, valid_name)(inp)?;
    let (mut inp, type_hint) = preceded(space0, delimited(tag("("), valid_name, tag(")")))(inp)?;
    let mut annotations: Vec<&str> = Vec::new();
    while let Ok((inp_ok, annotation)) = preceded(space1, valid_name)(inp) {
        annotations.push(&annotation);
        inp = inp_ok;
    }
    let (inp, _) = preceded(space0, line_ending)(inp)?;
    let (inp, code) = take_until("```")(inp)?;
    let (inp, _) = tag("```")(inp)?;
    let executor: Option<&str> = if executor.len() > 0 {
        Some(&executor)
    } else {
        None
    };
    let name: Option<&str> = if name.len() > 0 { Some(&name) } else { None };
    let type_hint: Option<&str> = if type_hint.len() > 0 {
        Some(&type_hint)
    } else {
        None
    };
    let codeblock = TokenType::CodeBlock(CodeBlock {
        executor,
        name,
        annotations,
        type_hint,
        code: &code,
    });
    Ok((
        inp,
        Token {
            position,
            inner: codeblock,
        },
    ))
}

fn parse_single_ident(inp: Span) -> IResult<Span, Token> {
    let (inp, position) = position(inp)?;
    let (inp, name) = delimited(tag("`"), take_while(is_allowed_in_ident), tag("`"))(inp)?;
    Ok((
        inp,
        Token {
            position,
            inner: TokenType::Ident(&name),
        },
    ))
}

fn parse_heading(inp: Span) -> IResult<Span, Token> {
    let (inp, position) = position(inp)?;
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
            position,
            inner: TokenType::Heading(heading),
        },
    ))
}

fn is_allowed_at_name_start(c: char) -> bool {
    is_allowed_in_name(c) && !['-'].contains(&c)
}

fn is_valid_name<'a>(name: &Span) -> bool {
    name.starts_with(is_allowed_at_name_start)
}

fn valid_name<'a>(inp: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    verify(take_while1(is_allowed_in_name), is_valid_name)(inp)
}

fn find_next<I, O, G>(mut g: G) -> impl FnMut(I) -> IResult<I, O>
where
    G: FnMut(I) -> IResult<I, O>,
    I: Clone + InputTake + InputLength + InputIter + Slice<RangeFrom<usize>>,
{
    move |mut inp: I| {
        while inp.input_len() > 0 {
            if let Ok(t) = g.parse(inp.clone()) {
                return Ok(t);
            } else {
                inp = inp.slice(1..);
            }
        }
        fail(inp)
    }
}

/// Greedily collect matches for the first parser, discarding anything that does not match until the second parser succeeds
/// Returns Err if the second parser does not succeed before EOF
fn find_many_till<I, O, P>(
    mut f: impl FnMut(I) -> IResult<I, O>,
    mut g: impl FnMut(I) -> IResult<I, P>,
    inp: I,
) -> IResult<I, (Vec<O>, P)>
where
    I: Clone + InputTake + InputLength + InputIter,
{
    let mut all_found = Vec::new();
    let mut inp = inp;
    loop {
        match g(inp.clone()) {
            Err(_) => {
                if let Ok((rest, found)) = f(inp.clone()) {
                    all_found.push(found);
                    inp = rest;
                } else {
                    let (rest, _) = take(1usize)(inp)?;
                    inp = rest;
                }
            }
            Ok((rest, stop)) => return Ok((rest, (all_found, stop))),
        }
    }
}

/// finds all non-overlapping matches for a given parser until the input is exhausted
fn _find_all<I, O, E, F>(mut pat: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
    I: InputIter + InputTake + Clone + InputLength + Slice<RangeFrom<usize>>,
    F: Parser<I, O, E>,
    E: ParseError<I>,
{
    move |mut i: I| {
        let mut all_found = Vec::new();
        loop {
            match pat.parse(i.clone()) {
                Ok((rest, found)) => {
                    i = rest;
                    all_found.push(found);
                }
                Err(_) => {
                    if i.input_len() > 0 {
                        i = i.slice(1..);
                    } else {
                        return Ok((i, all_found));
                    }
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use nom::combinator::not;
    use nom::multi::many0_count;
    use nom::Offset;

    #[test]
    fn test_find_all() {
        let src = "needle fskjnfonwofinwfeneedlekfsdond;nsdf;ondsfneedlefsmfok";
        let (rest, found) = _find_all(tag::<&str, &str, ()>("needle"))(src).unwrap();
        assert_eq!(found, vec!["needle", "needle", "needle"]);
        assert_eq!(rest, "");
    }

    #[test]
    fn test_find_many_till() {
        let src = "abcdefsba bababaaaaabbbbbabababab";
        let (rest, (found, end)) =
            find_many_till(alt((tag("a"), tag("b"))), tag("baba"), src).unwrap();
        assert_eq!(found, vec!["a", "b", "b", "a"]);
        assert_eq!(end, "baba");
        assert_eq!(rest, "baaaaabbbbbabababab");
        assert!(find_many_till(tag("a"), tag("b"), "aaaaaaaacccccc").is_err());
    }

    #[test]
    fn test_find_next() {
        let src = "foovrfs barand more";
        let needle = tag::<_, _, ()>("bar");
        let mut f_next = many0_count(not(&needle).and(take(1usize))).and(&needle);
        let (rest, res) = find_next(tag("bar"))(src).unwrap();
        let (rest2, res2) = f_next.parse(src).unwrap();
        assert_eq!(res, "bar");
        assert_eq!(rest, "and more");
        assert_eq!(res2, (8, "bar"));
        assert_eq!(rest2, "and more");
        assert!(find_next(tag("foo"))("haystack with no match").is_err());
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
    fn test_parse_to_directive() {
        let undertest =
            "To really `bake` a `cake` with `--flour`, `ignoreme` `--butter` and `--milk`: \r\nand more";
        let expected = TokenType::ToDirective(ToDirective {
            callpath: vec!["bake", "cake"],
            args: vec!["flour", "butter", "milk"],
        });
        let (rest, _) = assert_parse_token_with_inner(undertest, parse_to_directive, expected);
        assert_eq!(*rest.fragment(), "and more");
        assert!(parse_to_directive("To do nothing is no fun...\n".into()).is_err());
        assert!(parse_to_directive(
            "To have a `valid` to-directive without trailing newline is meaningless".into()
        )
        .is_err());
        let undertest = "To `have` `cake` without args is okay\n";
        let expected = TokenType::ToDirective(ToDirective {
            callpath: vec!["have", "cake"],
            args: vec![],
        });
        assert_parse_token_with_inner(undertest, parse_to_directive, expected);
    }

    #[test]
    fn test_parse_code_block() {
        let undertest = r#"```sh foo_bar (object) annot1 annot2
echo "I am a {{ banana }}"
```
and more"#;
        let expected = TokenType::CodeBlock(CodeBlock {
            executor: Some("sh"),
            name: Some("foo_bar"),
            type_hint: Some("object"),
            annotations: vec!["annot1", "annot2"],
            code: "echo \"I am a {{ banana }}\"\n",
        });
        let (rest, _) = assert_parse_token_with_inner(undertest, parse_code_block, expected);
        assert_eq!(*rest.fragment(), "\nand more");
    }

    #[test]
    fn test_parse_ident() {
        let undertest = "`_foo_bar.?bafroo\\` and this is whats left";
        let expected = TokenType::Ident("_foo_bar.?bafroo\\");
        let (rest, _) = assert_parse_token_with_inner(&undertest, parse_single_ident, expected);
        assert_eq!(*rest.fragment(), " and this is whats left");
    }

    #[test]
    fn test_parse_heading() {
        let undertest = "#### \t How To Bake a Cake\t  \r\nand live to tell the tale";
        let expected = TokenType::Heading(Heading {
            level: 4,
            text: "How To Bake a Cake",
        });
        let (rest, parsed) = assert_parse_token_with_inner(&undertest, parse_heading, expected);
        assert_eq!(
            parsed.position.offset(&rest),
            undertest.len() - rest.fragment().len()
        );
        assert_eq!(*rest.fragment(), "and live to tell the tale");
    }

    fn assert_parse_token_with_inner<'a>(
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
