use std::fs::File;
use std::io::Write;
use std::path::{Path, PathBuf};

use insta::assert_yaml_snapshot;
use tempfile::{tempdir, TempDir};

use recipe::context::{Command, Commands};
use recipe::loader::load;
use recipe::parser::{
    CodeBlock, Heading, Keyword, Selection, SelectionPath, SetDirective, TokenType, Tokens, Val,
};

#[test]
fn load_simple_recipe() {
    let contents = r#"
# Heading 1
To `bake` a `cake`
Set the `topping` to `"cherry"`
All you need to do is run the following command:
```sh
echo {{ topping }}
```
"#;
    let (base_path, _dir, _file) = write_temp_file("Recipe.md", contents);
    let loaded = load(&base_path).unwrap();
    assert_yaml_snapshot!(loaded);
}

#[test]
fn load_tokens() {
    let contents = r#"# Heading 1
To `bake` a `cake`
Set `foo` to `"bar"`
```sh
echo {{ topping }}
```
"#;
    let mut tokens = Tokens::new();
    tokens.load(contents.to_string(), Path::new("foo"), None);
    assert_yaml_snapshot!(tokens.collect::<Vec<_>>());
}

fn write_temp_file(filename: &str, contents: &str) -> (PathBuf, TempDir, File) {
    let dir = tempdir().unwrap();
    let file_path = dir.path().join(filename);
    let mut file = File::create(&file_path).unwrap();
    write!(file, "{}", contents).unwrap();
    (file_path, dir, file)
}
