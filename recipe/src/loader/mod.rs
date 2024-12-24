use std::collections::HashSet;

use crate::{
    context::AppContext,
    parser::{tokenizer::Tokenizer, TokenType, Tokens},
};

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
            Keyword(To) => load_command(&mut tokens, &mut app_context),
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

fn load_command(tokens: &mut Tokens, app_context: &mut AppContext) {
    todo!()
}

#[cfg(test)]
mod tests {
    use std::fs::File;
    use std::io::Write;

    use tempfile::{tempdir, TempDir};

    use super::*;

    #[test]
    fn load_internal_link() {
        let contents = r#"
# Heading 1
This is a note
"#;
        let (base_path, dir, file) = write_temp_file("Recipe.md", contents);
        let loaded = load(&base_path).unwrap();
    }

    fn write_temp_file(filename: &str, contents: &str) -> (String, TempDir, File) {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join(filename);
        let mut file = File::create(&file_path).unwrap();
        write!(file, "{}", contents).unwrap();
        (file_path.as_os_str().to_str().unwrap().to_owned(), dir, file)
    }
}
