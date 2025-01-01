use std::collections::HashSet;

use nom::combinator::opt;
use nom::multi::many0;
use nom::{
    branch::alt,
    bytes::complete::{tag, take_while, take_while1},
    character::complete::{digit1, line_ending, space0, space1},
    combinator::{fail, verify},
    error::VerboseError,
    sequence::{delimited, preceded},
    IResult,
};
use nom_locate::LocatedSpan;
use serde::{Deserialize, Serialize};

use error::ParseError;
use tokenizer::Tokenizer;

use crate::context::Command;

mod error;
pub mod tokenizer;

pub type Span<'a> = LocatedSpan<&'a str>;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Heading<'a> {
    pub level: usize,
    pub text: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
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

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Link<'a> {
    pub text: &'a str,
    pub path: &'a str,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Selection<'a> {
    Index(usize),
    Key(&'a str),
}
impl<'a> Selection<'a> {
    fn as_string(&self) -> String {
        match self {
            Selection::Index(i) => i.to_string(),
            Selection::Key(k) => k.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SelectionPath<'a>(#[serde(borrow)] pub Vec<Selection<'a>>);
impl<'a> SelectionPath<'a> {
    /// the selection as a JSON-Pointer per [RFC6901](https://datatracker.ietf.org/doc/html/rfc6901)
    pub fn json_pointer(&self) -> String {
        let mut pointer = String::new();
        for s in self.0.iter() {
            // both Tilde ~ and Slash / are disallowed in names so we can skip escaping
            pointer = format!("{pointer}/{}", s.as_string())
        }
        pointer
    }

    /// The last element
    pub fn last(&self) -> Option<&Selection> {
        self.0.last()
    }
    /// The selection up until the second-to-last element
    pub fn parent(&self) -> Option<SelectionPath> {
        if self.0.len() > 1 {
            let mut parent = self.0.clone();
            parent.pop();
            Some(SelectionPath(parent))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Val<'a> {
    ContextRef(#[serde(borrow)] SelectionPath<'a>),
    Literal(serde_json::Value),
}
impl<'a> TryFrom<&TokenType<'a>> for Val<'a> {
    type Error = ParseError<'a>;

    fn try_from(value: &TokenType<'a>) -> Result<Self, ParseError<'a>> {
        match *value {
            TokenType::Inline(src) => {
                if let Ok((_, val)) = parse_selection_path(src) {
                    Ok(Val::ContextRef(val))
                } else if let Ok(val) = json5::from_str(src) {
                    Ok(Val::Literal(val))
                } else {
                    Err(ParseError::InvalidValue(src))
                }
            }
            TokenType::Block(src) => match json5::from_str(src) {
                Ok(val) => Ok(Val::Literal(val)),
                Err(e) => Err(ParseError::InvalidLiteral((src, e))),
            },
            _ => Err(ParseError::InvalidTokenTypeForValue(value.clone())),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SetDirective<'a> {
    #[serde(borrow)]
    pub variable: SelectionPath<'a>,
    pub value: Val<'a>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Keyword {
    To,
    Set,
    Execute,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct Token<'a> {
    start: usize,
    end: usize,
    inner: TokenType<'a>,
}
impl<'a> Token<'a> {
    pub fn inner(&self) -> &TokenType<'a> {
        &self.inner
    }
    pub fn is_inline(&self) -> bool {
        matches!(self.inner, TokenType::Inline(_))
    }
    pub fn is_newline(&self) -> bool {
        self.inner == TokenType::Newline
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
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
    loaded_paths: HashSet<String>,
}
impl<'a> Tokens<'a> {
    pub fn new() -> Self {
        Self {
            tokens: Vec::new(),
            loaded_paths: HashSet::new(),
        }
    }
    pub fn load(&mut self, src: String, file_path: &'a str, heading_slug: Option<&'a str>) {
        if self.loaded_paths.insert(format!(
            "{}#{}",
            file_path,
            heading_slug.unwrap_or_default()
        )) {
            //TODO: we may want to tie the lifetime of the loaded files to the lifetime of the Tokens struct or Tokenizer instead of just .leak()ing
            //TODO: we are also prepending a newline here to avoid missing headings at the start of the file, but this messes with our offsets in Token
            let mut tokenizer = Tokenizer::from_str(format!("\n{src}").leak(), file_path);
            if let Some(heading_slug) = heading_slug {
                tokenizer.scope_to_heading(heading_slug);
            }
            self.tokens.push(tokenizer);
        }
    }
    pub fn current_path(&self) -> Option<&'a str> {
        self.tokens.last().map(|t| t.origin_path)
    }
}
impl<'a> Default for Tokens<'a> {
    fn default() -> Self {
        Self::new()
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

/// Parse a series of Commands from Tokens, i.e. the body of a To-Directive
pub fn parse_to_directive_inner<'a>(tokens: &mut Tokens<'a>) -> Vec<Command<'a>> {
    let mut commands = Vec::new();
    while let Some(token) = tokens.next() {
        match token.inner {
            TokenType::Block(block) => {
                if let Ok((_, codeblock)) = parse_code_block(block) {
                    commands.push(Command::CodeBlock(codeblock));
                } else {
                    todo!("Invalid Code Block");
                }
            }
            TokenType::Keyword(Keyword::Set) => {
                if let Ok(set_directive) = parse_set_directive_inner(tokens) {
                    commands.push(Command::SetDirective(set_directive));
                } else {
                    todo!("Invalid Set Directive");
                }
            }
            TokenType::Heading(_) => break,
            _ => continue, //TODO warnings/errors
        }
    }
    commands
}

fn parse_set_directive_inner<'a>(
    tokens: &mut Tokens<'a>,
) -> Result<SetDirective<'a>, ParseError<'a>> {
    if let (
        Some(Token {
            inner: TokenType::Inline(variable),
            ..
        }),
        Some(Token {
            inner: TokenType::Inline(value),
            ..
        }),
        Some(Token {
            inner: TokenType::Newline,
            ..
        }),
    ) = (tokens.next(), tokens.next(), tokens.next())
    {
        let (_, variable) = parse_selection_path(variable).map_err(ParseError::InvalidSelection)?;
        if let Ok((_, value)) = parse_selection_path(value) {
            Ok(SetDirective {
                variable,
                value: Val::ContextRef(value),
            })
        } else if let Ok(literal) = json5::from_str(value) {
            Ok(SetDirective {
                variable,
                value: Val::Literal(literal),
            })
        } else {
            Err(ParseError::InvalidValue(value))
        }
    } else {
        Err(ParseError::Generic("Wrong Tokentypes for Set-directive"))
    }
}
fn is_allowed_in_ident(c: char) -> bool {
    !c.is_whitespace() && c != '`'
}

fn is_allowed_in_name(c: char) -> bool {
    is_allowed_in_ident(c) && !['(', ')', '.', '[', ']', '{', '}', '\'', '"', '/', '~'].contains(&c)
}

fn parse_selection_path(inp: &str) -> IResult<&str, SelectionPath> {
    let (mut inp, first) = valid_name(inp)?;
    let mut sel_path = vec![Selection::Key(first)];
    while let Ok((rest, sel)) = alt((preceded(tag("."), parse_selection), parse_selection))(inp) {
        inp = rest;
        sel_path.push(sel);
    }
    Ok((inp, SelectionPath(sel_path)))
}

fn parse_selection(inp: &str) -> IResult<&str, Selection> {
    let mut array_index = delimited(tag("["), digit1::<_, VerboseError<&str>>, tag("]"));
    if let Ok((rest, idx)) = array_index(inp) {
        if let Ok(index) = idx.parse::<usize>() {
            return Ok((rest, Selection::Index(index)));
        } else {
            // arrays longer than u64::MAX will never be a thing in Recipe
            return fail(inp);
        }
    } else if let Ok((rest, name)) = valid_name(inp) {
        return Ok((rest, Selection::Key(name)));
    }
    fail(inp)
}

/// Parse the inner portion of a code-block
/// always consumes the entire input
/// The first word, not preceded by whitespace, is the executor
/// the next word, or first word preceded by whitespace is the name
/// the next word, if it is in parenthesis, is the type-hint
/// all following words until newline are annotations
/// everything after (but not including) the first newline is the code
fn parse_code_block(inp: &str) -> IResult<&str, CodeBlock> {
    let (inp, executor) = opt(valid_name)(inp)?;
    let (inp, name) = opt(preceded(space1, valid_name))(inp)?;
    let (inp, type_hint) = opt(preceded(space0, delimited(tag("("), valid_name, tag(")"))))(inp)?;
    let (inp, annotations) = many0(preceded(space1, valid_name))(inp)?;

    let (code, _) = preceded(space0, line_ending)(inp)?;
    let codeblock = CodeBlock {
        executor,
        name,
        type_hint,
        annotations,
        code,
    };
    let (rest, _) = take_while(|_| true)(code)?;
    Ok((rest, codeblock))
}

fn is_allowed_at_name_start(c: char) -> bool {
    is_allowed_in_name(c) && !['-'].contains(&c)
}

fn is_valid_name(name: &str) -> bool {
    name.starts_with(is_allowed_at_name_start)
}

fn valid_name(inp: &str) -> IResult<&str, &str> {
    verify(take_while1(is_allowed_in_name), is_valid_name)(inp)
}

#[cfg(test)]
mod tests {
    use super::*;

    impl<'a> TokenType<'a> {
        fn mock_token(self) -> Token<'a> {
            Token {
                start: 0,
                end: 0,
                inner: self,
            }
        }
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
    fn test_parse_code_block() {
        let undertest = r#"sh foo_bar (object) annot1 annot2
echo "I am a {{ banana }}"
"#;
        let expected = CodeBlock {
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
        assert!(parsed.eq_by_value(&expected));
        assert!(from_token.eq_by_value(&expected));

        let undertest = r#"sh
echo "I am a {{ banana }}"
"#;
        let expected = CodeBlock {
            executor: Some("sh".into()),
            name: None,
            type_hint: None,
            annotations: vec![],
            code: "echo \"I am a {{ banana }}\"\n".into(),
        };

        let from_token: CodeBlock = TokenType::Block(undertest.into())
            .mock_token()
            .try_into()
            .unwrap();

        let (_, parsed) = parse_code_block(undertest.into()).unwrap();
        assert!(parsed.eq_by_value(&expected));
        assert!(from_token.eq_by_value(&expected));
    }

    #[test]
    fn test_parse_selection_path() {
        let src = "foo[2].bar and something else";
        let (rest, parsed) = parse_selection_path(src.into()).unwrap();
        assert_eq!(
            parsed,
            SelectionPath(vec![
                Selection::Key("foo"),
                Selection::Index(2),
                Selection::Key("bar")
            ])
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
}
