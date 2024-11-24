use std::ops::RangeFrom;

use nom::{
    branch::alt,
    bytes::complete::{is_not, tag, take, take_till, take_until, take_while, take_while1},
    character::complete::{char, digit1, line_ending, space0, space1},
    combinator::{fail, verify},
    error::{Error, VerboseError},
    multi::many1_count,
    sequence::{delimited, preceded, terminated},
    IResult, InputIter, InputLength, InputTake, Parser, Slice,
};
use nom_locate::{position, LocatedSpan};
mod error;
use error::ParseError;
pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, PartialEq, Eq)]
pub struct Heading<'a> {
    level: usize,
    text: &'a str,
}

#[derive(Debug, PartialEq, Eq)]
pub struct CodeBlock<'a> {
    pub executor: Option<Span<'a>>,
    pub name: Option<Span<'a>>,
    pub type_hint: Option<Span<'a>>,
    pub annotations: Vec<Span<'a>>,
    pub code: Span<'a>,
}

impl<'a> TryFrom<Token<'a>> for CodeBlock<'a> {
    type Error = ParseError<'a>;

    fn try_from(value: Token<'a>) -> Result<Self, Self::Error> {
        match value.inner {
            TokenType::Block(src) => parse_code_block(src)
                .map(|(_, res)| res)
                .map_err(|_| ParseError::InvalidCodeBlock(src)),
            _ => Err(ParseError::InvalidToken(value)),
        }
    }
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
pub enum Selection<'a> {
    Index(u64),
    Key(&'a str),
}

#[derive(Debug, PartialEq, Eq)]
pub enum Value<'a> {
    ContextRef(Vec<Selection<'a>>),
    Literal(serde_json::Value),
}

#[derive(Debug, PartialEq, Eq)]
pub struct SetDirective<'a> {
    variable: Vec<Selection<'a>>,
    value: Value<'a>,
}

#[derive(Debug, PartialEq, Eq)]
pub enum Keyword {
    To,
    Set,
    Execute,
}
impl Keyword {
    fn try_from_span(inp: Span) -> IResult<(), Keyword> {
        let Ok(keyword) = Keyword::try_from_str(*inp.fragment()) else {
            return fail(());
        };
        Ok(((), keyword))
    }

    fn try_from_str(inp: &str) -> Result<Self, String> {
        match inp {
            "To" => Ok(Keyword::To),
            "Set" => Ok(Keyword::Set),
            "Execute" => Ok(Keyword::Execute),
            _ => Err(format!("Not a keyword: {inp}")),
        }
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct Token<'a> {
    start: Span<'a>,
    end: Span<'a>,
    inner: TokenType<'a>,
}

impl<'a> Token<'a> {
    pub fn inner(&self) -> &TokenType<'a> {
        &self.inner
    }
}

#[derive(Debug)]
pub struct Tokenizer<'a> {
    src: Span<'a>,
}

impl<'a> Tokenizer<'a> {
    pub fn from_str(src: &'a str) -> Self {
        Tokenizer { src: src.into() }
    }
}
impl<'a> Iterator for Tokenizer<'a> {
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

#[derive(Debug, PartialEq, Eq)]
pub enum TokenType<'a> {
    Heading(Heading<'a>),
    Block(Span<'a>),
    Inline(Span<'a>),
    Link(Link<'a>),
    Keyword(Span<'a>),
    Newline,
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

fn parse_newline(inp: Span) -> IResult<Span, Token> {
    let (rest, newline) = line_ending(inp)?;
    Ok((
        rest,
        Token {
            start: position(inp)?.1,
            end: position(rest)?.1,
            inner: TokenType::Newline,
        },
    ))
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
    Ok((
        rest,
        Token {
            start: position(res)?.1,
            end: position(rest)?.1,
            inner: TokenType::Keyword(res),
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
            inner: TokenType::Inline(res),
        },
    ))
}

fn parse_block(inp: Span) -> IResult<Span, Token> {
    let (rest, res) = delimited(tag("```"), take_while(|c| c != '`'), tag("```"))(inp)?;
    Ok((
        rest,
        Token {
            start: position(res)?.1,
            end: position(rest)?.1,
            inner: TokenType::Block(res),
        },
    ))
}

fn is_allowed_in_ident(c: char) -> bool {
    !c.is_whitespace() && c != '`'
}

fn is_allowed_in_name(c: char) -> bool {
    is_allowed_in_ident(c) && !['(', ')', '.', '[', ']', '{', '}'].contains(&c)
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

fn ends_literal(c: char) -> bool {
    c == '`'
}

fn parse_set_directive(inp: Span) -> IResult<Span, Token> {
    let (inp, start) = position(inp)?;
    let (inp, _) = terminated(tag("Set"), space1)(inp)?;
    let (_, first) = find_next(delimited(tag("`"), take_while(|c| c != '`'), tag("`")))(inp)?;
    let (_, variable) = parse_selection_path(first)?;
    let set_directive = SetDirective {
        variable,
        value: todo!(),
    };
    todo!();
}

/// Parses a value per the [JSON5-spec](https://spec.json5.org/) (i.e. the json5 crate for now)
fn parse_multiline_literal_inner(inp: Span) -> IResult<Span, serde_json::Value> {
    let (rest, src) = take_till(ends_literal)(inp)?;
    if let Ok(val) = json5::from_str::<serde_json::Value>(*src) {
        return Ok((rest, val));
    } else {
        fail(inp)
    }
}

fn parse_inline_literal_inner(inp: Span) -> IResult<Span, serde_json::Value> {
    let (rest, src) = take_while(|c| !ends_literal(c) && !is_line_end(c))(inp)?;
    if let Ok(val) = json5::from_str::<serde_json::Value>(*src) {
        return Ok((rest, val));
    } else {
        fail(inp)
    }
}

fn parse_multiline_literal(inp: Span) -> IResult<Span, serde_json::Value> {
    delimited(
        parse_multiline_literal_start,
        parse_multiline_literal_inner,
        tag("```"),
    )(inp)
}

fn parse_inline_literal(inp: Span) -> IResult<Span, serde_json::Value> {
    delimited(tag("`"), parse_inline_literal_inner, tag("`"))(inp)
}

fn parse_multiline_literal_start(inp: Span) -> IResult<Span, Span> {
    terminated(
        alt((tag("```"), tag("```json"), tag("```json5"))),
        alt((space1, line_ending)),
    )(inp)
}

/// By value we mean the right hand side of an assignment
fn parse_value(inp: Span) -> IResult<Span, Value> {
    if let Ok((inp, select)) = delimited(tag("`"), parse_selection_path, tag("`"))(inp) {
        return Ok((inp, Value::ContextRef(select)));
    }
    if let Ok((inp, value)) = delimited(
        tag::<_, _, VerboseError<_>>("`"),
        take_while1(|c| !is_line_end(c)),
        tag("`"),
    )(inp)
    {
        if let Ok((_, value)) = parse_multiline_literal_inner(inp) {
            return Ok((inp, Value::Literal(value)));
        } else {
            return fail(inp);
        }
    }
    let valid_multiline_literal_start = alt((
        terminated(tag("```"), alt((space1, line_ending))),
        tag("```json"),
        tag("```json5"),
    ));
    if let Ok((inp, value)) = delimited(
        valid_multiline_literal_start,
        parse_multiline_literal_inner,
        tag("```"),
    )(inp)
    {
        return Ok((inp, Value::Literal(value)));
    } else {
        return fail(inp);
    }
}

fn parse_selection_path(inp: Span) -> IResult<Span, Vec<Selection>> {
    let (mut inp, first) = valid_name(inp)?;
    let mut sel_path = vec![Selection::Key(*first)];
    while let Ok((rest, sel)) = alt((preceded(tag("."), parse_selection), parse_selection))(inp) {
        inp = rest;
        sel_path.push(sel);
    }
    Ok((inp, sel_path))
}

fn parse_selection(inp: Span) -> IResult<Span, Selection> {
    let mut array_index = delimited(tag("["), digit1::<_, VerboseError<Span>>, tag("]"));
    if let Ok((rest, idx)) = array_index(inp) {
        if let Ok(index) = idx.fragment().parse::<u64>() {
            return Ok((rest, Selection::Index(index)));
        } else {
            // arrays longer than u64::MAX will never be a thing in Recipe
            return fail(inp);
        }
    } else if let Ok((rest, name)) = valid_name(inp) {
        return Ok((rest, Selection::Key(&name)));
    }
    fail(inp)
}

/// To-Directive: Starts with keyword "To" followed by at least one space.
/// finds valid names in backticks (\`name\`) until end of line (list of names is the callpath).
/// Optionally specifies required arguments as valid names preceded by 2 dashes(\`--name\`).
/// Callpath must be complete before the first argument, and must have at least one name in callpath before newline.
fn parse_to_directive(inp: Span) -> IResult<Span, ToDirective, Error<LocatedSpan<&str>>> {
    let (inp, start) = position(inp)?;
    let (inp, _) = terminated(tag("To"), space1)(inp)?;
    let (mut inp, (callpath, end)) = find_many_till(
        delimited(tag("`"), valid_name, tag("`")),
        alt((tag("`--"), line_ending)),
        inp,
    )?;
    if callpath.is_empty() {
        return fail(start); // no elements in callpath means invalid directive
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

    Ok((
        inp,
        ToDirective {
            callpath: callpath.into_iter().map(|e| *e.fragment()).collect(),
            args: args.into_iter().map(|e| *e.fragment()).collect(),
        },
    ))
}

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

/// Parse the inner portion of a code-block
/// always consumes the entire input
/// The first word, not preceded by whitespace, is the executor
/// the next word, or first word preceded by whitespace is the name
/// the next word, if it is in parenthesis, is the type-hint
/// all following words until newline are annotations
/// everything after (but not including) the first newline is the code
fn parse_code_block(inp: Span) -> IResult<Span, CodeBlock> {
    let (inp, executor) = valid_name(inp)?;
    let (inp, name) = preceded(space1, valid_name)(inp)?;
    let (mut inp, type_hint) = preceded(space0, delimited(tag("("), valid_name, tag(")")))(inp)?;
    let mut annotations = Vec::new();
    while let Ok((inp_ok, annotation)) = preceded(space1, valid_name)(inp) {
        annotations.push(annotation);
        inp = inp_ok;
    }
    let (code, _) = preceded(space0, line_ending)(inp)?;
    let codeblock = CodeBlock {
        executor: none_if_empty(executor),
        name: none_if_empty(name),
        type_hint: none_if_empty(type_hint),
        annotations,
        code: code,
    };
    let (rest, _) = take_while(|_| true)(code)?;
    Ok((rest, codeblock))
}

/// Parse a markdown heading
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

fn is_allowed_at_name_start(c: char) -> bool {
    is_allowed_in_name(c) && !['-'].contains(&c)
}

fn is_valid_name<'a>(name: &Span) -> bool {
    name.starts_with(is_allowed_at_name_start)
}

fn valid_name<'a>(inp: Span<'a>) -> IResult<Span<'a>, Span<'a>> {
    verify(take_while1(is_allowed_in_name), is_valid_name)(inp)
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
            Ok((rest, second_parser_match)) => return Ok((rest, (all_found, second_parser_match))),
        }
    }
}

/// finds all non-overlapping matches for a given parser until the input is exhausted
fn _find_all<I, O, E, F>(mut pat: F) -> impl FnMut(I) -> IResult<I, Vec<O>, E>
where
    I: InputIter + InputTake + Clone + InputLength + Slice<RangeFrom<usize>>,
    F: Parser<I, O, E>,
    E: nom::error::ParseError<I>,
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

fn none_if_empty(s: Span) -> Option<Span> {
    if s.len() > 0 {
        Some(s)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {

    use super::*;
    use nom::combinator::not;
    use nom::multi::many0_count;
    use nom::Offset;

    impl<'a> TokenType<'a> {
        fn mock_token(self) -> Token<'a> {
            Token {
                start: "".into(),
                end: "".into(),
                inner: self,
            }
        }
    }

    fn same_fragment(a: Span, b: Span) -> bool {
        *a.fragment() == *b.fragment()
    }

    fn same_opt_fragment(a: Option<Span>, b: Option<Span>) -> bool {
        a.map(|s| *s.fragment()) == b.map(|s| *s.fragment())
    }

    impl<'a> CodeBlock<'a> {
        fn eq_by_value(&self, other: &CodeBlock) -> bool {
            if self.annotations.len() != other.annotations.len() {
                return false;
            }
            for i in 0..self.annotations.len() {
                if !same_fragment(self.annotations[i], other.annotations[i]) {
                    return false;
                }
            }
            same_fragment(self.code, other.code)
                && same_opt_fragment(self.name, other.name)
                && same_opt_fragment(self.executor, other.executor)
                && same_opt_fragment(self.type_hint, other.type_hint)
        }
    }

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
    fn test_parse_selection_path() {
        let src = "foo[2].bar and something else";
        let (rest, parsed) = parse_selection_path(src.into()).unwrap();
        assert_eq!(
            parsed,
            vec![
                Selection::Key("foo"),
                Selection::Index(2),
                Selection::Key("bar")
            ]
        );
        assert_eq!(*rest.fragment(), " and something else");
    }

    #[test]
    fn test_parse_selection() {
        let src = "[0123].foo"; //if str.parse() accepts leading zeros, so do we
        let (rest, parsed) = parse_selection(src.into()).unwrap();
        assert_eq!(parsed, Selection::Index(123));
        assert_eq!(*rest.fragment(), ".foo");
        let src = "A_Valid_Key123[0]";
        let (rest, parsed) = parse_selection(src.into()).unwrap();
        assert_eq!(parsed, Selection::Key("A_Valid_Key123"));
        assert_eq!(*rest.fragment(), "[0]");
        assert!(parse_selection(".foo".into()).is_err());
        assert!(parse_selection("[3.14]".into()).is_err());
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

    // #[test]
    // fn test_parse_to_directive() {
    //     let undertest =
    //         "To really `bake` a `cake` with `--flour`, `ignoreme` `--butter` and `--milk`: \r\nand more";
    //     let expected = TokenType::ToDirective(ToDirective {
    //         callpath: vec!["bake", "cake"],
    //         args: vec!["flour", "butter", "milk"],
    //     });
    //     let (rest, _) = assert_parse_token_with_inner(undertest, parse_to_directive, expected);
    //     assert_eq!(*rest.fragment(), "and more");
    //     assert!(parse_to_directive("To do nothing is no fun...\n".into()).is_err());
    //     assert!(parse_to_directive(
    //         "To have a `valid` to-directive without trailing newline is meaningless".into()
    //     )
    //     .is_err());
    //     let undertest = "To `have` `cake` without args is okay\n";
    //     let expected = TokenType::ToDirective(ToDirective {
    //         callpath: vec!["have", "cake"],
    //         args: vec![],
    //     });
    //     assert_parse_token_with_inner(undertest, parse_to_directive, expected);
    // }

    #[test]
    fn test_parse_code_block() {
        let undertest = r#"sh foo_bar (object) annot1 annot2
echo "I am a {{ banana }}"
"#;
        let final_block = CodeBlock {
            executor: Some("sh".into()),
            name: Some("foo_bar".into()),
            type_hint: Some("object".into()),
            annotations: vec!["annot1".into(), "annot2".into()],
            code: "echo \"I am a {{ banana }}\"\n".into(),
        };

        let from_token: CodeBlock = TokenType::Block(undertest.into())
            .mock_token()
            .try_into()
            .unwrap();

        let (_, parsed) = parse_code_block(undertest.into()).unwrap();
        assert!(parsed.eq_by_value(&final_block));
        assert!(from_token.eq_by_value(&final_block));
    }

    #[test]
    fn test_parse_heading() {
        let undertest = "\n#### \t How To Bake a Cake\t  \r\nand live to tell the tale";
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
