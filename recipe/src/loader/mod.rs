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
    let mut loaded = HashSet::new();
    // to avoid infinite loops on circular references
    loaded.insert(root_path.to_owned());
    let mut tokens = Tokens::new();
    tokens.push(Tokenizer::from_str(recipe.leak(), root_path));
    let mut app_context = AppContext::new();
    loop {
        match tokens.next() {
            Some(token) => {
                use crate::parser::Keyword::*;
                use TokenType::*;
                match token.inner() {
                    Link(link) => {
                        if loaded.insert(link.path.to_owned()) {
                            if let Ok(src) = read_file(link.path) {
                                //TODO: .leak() is very lazy memory management but might just be good enough for now
                                tokens.push(Tokenizer::from_str(src.leak(), link.path));
                            }
                        }
                    }
                    Block(block) => todo!(),
                    Keyword(To) => load_command(&mut tokens, &mut app_context),
                    Keyword(keyword) => todo!(),
                    Inline(_) | Heading(_) | Newline => continue,
                }
            }
            None => break,
        }
    }

    todo!();
}

fn load_command(tokens: &mut Tokens, app_context: &mut AppContext) {
    todo!();
}
