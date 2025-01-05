use std::path::Path;

use convert_case::{Case, Casing};
use itertools::Itertools;

use crate::context::{Command, Commands};
use crate::parser::{Tokens, TokenType};
use crate::parser::parse_to_directive_inner;

/// Load the Context
pub fn load(root_path: &Path) -> anyhow::Result<Commands> {
    let recipe = load_file(root_path)?;
    let mut tokens = Tokens::new();
    tokens.load(recipe, root_path, None);
    let mut commands = Commands::new();
    while let Some(token) = tokens.next() {
        use crate::parser::Keyword::*;
        use TokenType::*;
        match token.inner() {
            Link(link) => load_link(&mut tokens, link.path),
            Block(code) => load_block(&mut tokens, &mut commands, code),
            Keyword(To) => load_to_directive(&mut tokens, &mut commands),
            Keyword(_keyword) => todo!(),
            Inline(_) | Heading(_) | Newline => continue,
        }
    }
    Ok(commands)
}

fn load_link<'a>(tokens: &mut Tokens<'a>, origin_path: &'a str) {
    if origin_path.is_empty() {
        return;
    }
    let (mut file_path, heading_slug) = match origin_path.split('#').collect::<Vec<_>>().as_slice()
    {
        [file_path] => (*file_path, None),
        [file_path, heading_slug] => (*file_path, Some(*heading_slug)),
        _ => return,
    };
    if file_path.is_empty() {
        file_path = tokens.current_path().and_then(|p|p.to_str()).unwrap_or_default();
    }
    if let Ok(contents) = load_file(file_path) {
        tokens.load(contents, file_path.as_ref(), heading_slug);
    }
}

/// Resolve a path and return the contents of the file
/// TODO - There will be relative paths and default locations
pub fn load_file(path: impl AsRef<Path>) -> Result<String, std::io::Error> {
    std::fs::read_to_string(path)
}

fn load_block(_tokens: &mut Tokens, _commands: &mut Commands, _code: &str) {
    todo!()
}

fn load_to_directive<'a>(tokens: &mut Tokens<'a>, commands: &mut Commands<'a>) {
    let mut names = Vec::new();
    for token in tokens.by_ref() {
        match token.inner() {
            TokenType::Inline(inline) => {
                names.push(*inline);
            }
            TokenType::Newline => break,
            _ => {
                //TODO Output a warning. Do we break or continue here?
                continue;
            }
        }
    }
    let name = names.iter().map(|n| n.to_case(Case::Kebab)).join("-");
    if name.is_empty() {

        //return; //TODO errors/warnings
    }
    let parsed_commands = parse_to_directive_inner(tokens);
    commands.register_command(name, Command::Composite(parsed_commands));
}

#[cfg(test)]
mod tests {}
