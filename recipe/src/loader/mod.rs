mod error;

use crate::context::Command;
use crate::parser::{parse_to_directive_inner, Token};
use crate::{
    context::AppContext,
    parser::{TokenType, Tokens},
};
use itertools::Itertools;

/// Load the entire AppContext
pub fn load(root_path: &str) -> anyhow::Result<AppContext> {
    let recipe = load_file(root_path)?;
    let mut tokens = Tokens::new();
    tokens.load(recipe, root_path, None);
    let mut app_context = AppContext::new();
    while let Some(token) = tokens.next() {
        use crate::parser::Keyword::*;
        use TokenType::*;
        match token.inner() {
            Link(link) => load_link(&mut tokens, link.path),
            Block(block) => load_block(&mut tokens, &mut app_context),
            Keyword(To) => load_to_directive(&mut tokens, &mut app_context),
            Keyword(keyword) => todo!(),
            Inline(_) | Heading(_) | Newline => continue,
        }
    }
    Ok(app_context)
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
        file_path = tokens.current_path().unwrap_or_default();
    }
    if let Ok(contents) = load_file(file_path) {
        tokens.load(contents, file_path, heading_slug);
    }
}

/// Resolve a path and return the contents of the file
/// TODO - There will be relative paths and default locations
pub fn load_file(path: &str) -> Result<String, std::io::Error> {
    std::fs::read_to_string(path)
}

fn load_block(tokens: &mut Tokens, app_context: &mut AppContext) {
    todo!()
}

fn load_to_directive<'a>(tokens: &mut Tokens<'a>, app_context: &mut AppContext<'a>) {
    let mut names = Vec::new();
    for token in tokens.by_ref() {
        match token.inner() {
            TokenType::Inline(inline) => {
                names.push(*inline);
            },
            TokenType::Newline => break,
            _ => {
                //TODO Output a warning. Do we break or continue here?
                continue;
            }

        }
    }
    let name = names.iter().join(" ");
    if name.is_empty()  {

        //return; //TODO errors/warnings
    }
    let commands = parse_to_directive_inner(tokens);
    app_context.register_command(name, Command::Composite(commands));
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::Write;

    use tempfile::{tempdir, TempDir};

    use super::*;
}
