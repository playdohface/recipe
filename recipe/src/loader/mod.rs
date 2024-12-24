use std::collections::HashSet;

use crate::{
    context::AppContext,
    parser::{tokenizer::Tokenizer, TokenType, Tokens},
};

/// Resolve a path and return the contents of the file
/// TODO - There will be relative paths and default locations
pub fn read_file(path: &str) -> Result<String, std::io::Error> {
    std::fs::read_to_string(path)
}

/// Load the entire AppContext
pub fn load(root_path: &str) -> anyhow::Result<AppContext> {
    let recipe = read_file(root_path)?;
    let mut tokens = Tokens::new();
    tokens.load(recipe, root_path);
    let mut app_context = AppContext::new();
    while let Some(token) = tokens.next() {
        use crate::parser::Keyword::*;
        use TokenType::*;
        match token.inner() {
            Link(link) => load_link(&mut tokens, link.path),
            Block(block) => load_block(&mut tokens, &mut app_context),
            Keyword(To) => load_command(&mut tokens, &mut app_context),
            Keyword(keyword) => todo!(),
            Inline(_) | Heading(_) | Newline => continue,
        }
    }
    Ok(app_context)
}

fn load_link(tokens: &mut Tokens, path: &str) {
    todo!()
}

fn load_block(tokens: &mut Tokens, app_context: &mut AppContext) {
    todo!()
}

fn load_command(tokens: &mut Tokens, app_context: &mut AppContext) {
    todo!()
}
