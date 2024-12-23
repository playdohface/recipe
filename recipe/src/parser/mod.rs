use std::{
    collections::{HashMap, HashSet},
    ops::RangeFrom,
};

use anyhow::anyhow;
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
pub mod tokenizer;
use error::ParseError;
use tokenizer::Tokenizer;

use crate::loader;
pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Heading<'a> {
    level: usize,
    text: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CodeBlock<'a> {
    pub executor: Option<&'a str>,
    pub name: Option<&'a str>,
    pub type_hint: Option<&'a str>,
    pub annotations: Vec<&'a str>,
    pub code: &'a str,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Link<'a> {
    text: &'a str,
    pub path: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToDirective<'a> {
    callpath: Vec<&'a str>,
    args: Vec<&'a str>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Selection<'a> {
    Index(u64),
    Key(&'a str),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value<'a> {
    ContextRef(Vec<Selection<'a>>),
    Literal(serde_json::Value),
}
impl<'a> TryFrom<&TokenType<'a>> for Value<'a> {
    type Error = ParseError<'a>;

    fn try_from(value: &TokenType<'a>) -> Result<Self, ParseError<'a>> {
        match *value {
            TokenType::Inline(src) => {
                if let Ok((_, val)) = parse_selection_path(src) {
                    Ok(Value::ContextRef(val))
                } else if let Ok(val) = json5::from_str(src) {
                    Ok(Value::Literal(val))
                } else {
                    Err(ParseError::InvalidValue(src))
                }
            }
            TokenType::Block(src) => match json5::from_str(src) {
                Ok(val) => Ok(Value::Literal(val)),
                Err(e) => Err(ParseError::InvalidLiteral((src, e))),
            },
            _ => Err(ParseError::InvalidTokenTypeForValue(value.clone())),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SetDirective<'a> {
    variable: Vec<Selection<'a>>,
    value: Value<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Keyword {
    To,
    Set,
    Execute,
}

#[derive(Debug, Clone, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum TokenType<'a> {
    Heading(Heading<'a>),
    Block(&'a str),
    Inline(&'a str),
    Link(Link<'a>),
    Keyword(Keyword),
    Newline,
}

pub struct Tokens<'a> {
    tokens: Vec<Tokenizer<'a>>,
}
impl<'a> Tokens<'a> {
    pub fn new() -> Self {
        Self { tokens: Vec::new() }
    }
    pub fn push(&mut self, tokenizer: Tokenizer<'a>) {
        self.tokens.push(tokenizer);
    }
}
impl<'a> Iterator for Tokens<'a> {
    type Item = Token<'a>;

    fn next(&mut self) -> Option<Self::Item> {
        while let Some(tokenizer) = self.tokens.last_mut() {
            if let Some(token) = tokenizer.next() {
                return Some(token);
            } else {
                self.tokens.pop();
            }
        }
        None
    }
}

fn parse_set_directive<'a>(inp: &'a [Token]) -> IResult<&'a [Token<'a>], SetDirective<'a>> {
    let mut iter = inp.iter().map(|t| &t.inner).enumerate();
    match (iter.next(), iter.next()) {
        (Some((_, TokenType::Keyword(Keyword::Set))), Some((_, TokenType::Inline(variable)))) => {
            let variable = if let Ok((_, variable)) = parse_selection_path(*variable) {
                variable
            } else {
                return fail(inp);
            };
            let mut next = iter.next();
            while let Some((i, TokenType::Newline)) = next {
                next = iter.next();
            }
            let (i, value) = if let Some((i, value)) = next {
                (i, value.try_into())
            } else {
                return fail(inp);
            };
            if let Ok(value) = value {
                let rest = if inp.len() >= i { &inp[i + 1..] } else { &[] };
                Ok((rest, SetDirective { variable, value }))
            } else {
                fail(inp)
            }
        }
        _ => fail(inp),
    }
}

fn is_allowed_in_ident(c: char) -> bool {
    !c.is_whitespace() && c != '`'
}

fn is_allowed_in_name(c: char) -> bool {
    is_allowed_in_ident(c) && !['(', ')', '.', '[', ']', '{', '}', '\'', '"'].contains(&c)
}

fn ends_literal(c: char) -> bool {
    c == '`'
}

fn parse_multiline_literal_start(inp: Span) -> IResult<Span, Span> {
    terminated(
        alt((tag("```"), tag("```json"), tag("```json5"))),
        alt((space1, line_ending)),
    )(inp)
}

fn parse_selection_path(inp: &str) -> IResult<&str, Vec<Selection>> {
    let (mut inp, first) = valid_name(inp)?;
    let mut sel_path = vec![Selection::Key(first)];
    while let Ok((rest, sel)) = alt((preceded(tag("."), parse_selection), parse_selection))(inp) {
        inp = rest;
        sel_path.push(sel);
    }
    Ok((inp, sel_path))
}

fn parse_selection(inp: &str) -> IResult<&str, Selection> {
    let mut array_index = delimited(tag("["), digit1::<_, VerboseError<&str>>, tag("]"));
    if let Ok((rest, idx)) = array_index(inp) {
        if let Ok(index) = idx.parse::<u64>() {
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

// /// To-Directive: Starts with keyword "To" followed by at least one space.
// /// finds valid names in backticks (\`name\`) until end of line (list of names is the callpath).
// /// Optionally specifies required arguments as valid names preceded by 2 dashes(\`--name\`).
// /// Callpath must be complete before the first argument, and must have at least one name in callpath before newline.
// fn parse_to_directive(inp: Span) -> IResult<Span, ToDirective, Error<LocatedSpan<&str>>> {
//     let (inp, start) = position(inp)?;
//     let (inp, _) = terminated(tag("To"), space1)(inp)?;
//     let (mut inp, (callpath, end)) = find_many_till(
//         delimited(tag("`"), valid_name, tag("`")),
//         alt((tag("`--"), line_ending)),
//         inp,
//     )?;
//     if callpath.is_empty() {
//         return fail(start); // no elements in callpath means invalid directive
//     }
//     let mut args = Vec::new();
//     if *end.fragment() == "`--" {
//         let (rest, arg1) = terminated(valid_name, tag("`"))(inp)?;
//         let (rest, (otherargs, _)) = find_many_till(
//             delimited(tag("`--"), valid_name, tag("`")),
//             line_ending,
//             rest,
//         )?;
//         args.push(arg1);
//         args.extend(otherargs);
//         inp = rest;
//     }

//     Ok((
//         inp,
//         ToDirective {
//             callpath: callpath.into_iter().map(|e| *e.fragment()).collect(),
//             args: args.into_iter().map(|e| *e.fragment()).collect(),
//         },
//     ))
// }

/// Parse the inner portion of a code-block
/// always consumes the entire input
/// The first word, not preceded by whitespace, is the executor
/// the next word, or first word preceded by whitespace is the name
/// the next word, if it is in parenthesis, is the type-hint
/// all following words until newline are annotations
/// everything after (but not including) the first newline is the code
fn parse_code_block(inp: &str) -> IResult<&str, CodeBlock> {
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

fn is_allowed_at_name_start(c: char) -> bool {
    is_allowed_in_name(c) && !['-'].contains(&c)
}

fn is_valid_name<'a>(name: &str) -> bool {
    name.starts_with(is_allowed_at_name_start)
}

fn valid_name<'a>(inp: &'a str) -> IResult<&'a str, &'a str> {
    verify(take_while1(is_allowed_in_name), is_valid_name)(inp)
}

fn none_if_empty(s: &str) -> Option<&str> {
    if s.len() > 0 {
        Some(s)
    } else {
        None
    }
}

#[cfg(test)]
mod tests {

    use super::*;

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
                if !(self.annotations[i] == other.annotations[i]) {
                    return false;
                }
            }
            self.code == other.code
                && self.name == other.name
                && self.executor == other.executor
                && self.type_hint == other.type_hint
        }
    }

    #[test]
    fn test_parse_set_directive() {
        let src = vec![
            TokenType::Keyword(Keyword::Set).mock_token(),
            TokenType::Inline("foo.bar").mock_token(),
            TokenType::Inline("'baz'").mock_token(),
        ];
        let (rest, parsed) = parse_set_directive(&src).unwrap();
        assert_eq!(
            parsed.variable,
            vec![Selection::Key("foo"), Selection::Key("bar")]
        );
        assert_eq!(
            parsed.value,
            Value::Literal(serde_json::Value::String("baz".into()))
        );
        assert_eq!(rest.len(), 0);
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
        assert_eq!(rest, " and something else");
    }

    #[test]
    fn test_parse_selection() {
        let src = "[0123].foo"; //if str.parse() accepts leading zeros, so do we
        let (rest, parsed) = parse_selection(src.into()).unwrap();
        assert_eq!(parsed, Selection::Index(123));
        assert_eq!(rest, ".foo");
        let src = "A_Valid_Key123[0]";
        let (rest, parsed) = parse_selection(src.into()).unwrap();
        assert_eq!(parsed, Selection::Key("A_Valid_Key123"));
        assert_eq!(rest, "[0]");
        assert!(parse_selection(".foo".into()).is_err());
        assert!(parse_selection("[3.14]".into()).is_err());
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

    //     #[test]
    //     fn test_parse_code_block() {
    //         let undertest = r#"sh foo_bar (object) annot1 annot2
    // echo "I am a {{ banana }}"
    // "#;
    //         let final_block = CodeBlock {
    //             executor: Some("sh".into()),
    //             name: Some("foo_bar".into()),
    //             type_hint: Some("object".into()),
    //             annotations: vec!["annot1".into(), "annot2".into()],
    //             code: "echo \"I am a {{ banana }}\"\n".into(),
    //         };

    //         let from_token: CodeBlock = TokenType::Block(undertest.into())
    //             .mock_token()
    //             .try_into()
    //             .unwrap();

    //         let (_, parsed) = parse_code_block(undertest.into()).unwrap();
    //         assert!(parsed.eq_by_value(&final_block));
    //         assert!(from_token.eq_by_value(&final_block));
    //     }
}
