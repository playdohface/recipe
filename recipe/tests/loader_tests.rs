#[cfg(test)]
mod loader_tests {
    use std::fs::File;
    use std::io::Write;
    use itertools::Itertools;
    use serde_json::json;
    use tempfile::{TempDir, tempdir};
    use recipe::context::{AppContext, Command};
    use recipe::loader::load;
    use recipe::parser::{CodeBlock, Heading, Keyword, Selection, SelectionPath, SetDirective, TokenType, Tokens, Val};

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

        let set_cherry = Command::SetDirective(SetDirective { variable: SelectionPath (vec![Selection::Key("topping")]), value: Val::Literal(json!("cherry")) });
        let echo_topping = Command::CodeBlock(CodeBlock { executor: Some("sh"), name: None, type_hint: None, annotations: vec![], code: "echo {{ topping }}\n" });
        let bake_cake = Command::Composite(vec![set_cherry, echo_topping]);
        let mut expected = AppContext::new();
        expected.register_command("bake cake".to_string(), bake_cake);
        assert_eq!(loaded, expected);

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
        tokens.load(contents.to_string(), "foo", None);
        assert_eq!(tokens.map(|t|t.inner().clone()).collect_vec(), vec![
            TokenType::Heading(Heading{ level: 1, text: "Heading 1" }),
            TokenType::Keyword(Keyword::To),
            TokenType::Inline("bake"),
            TokenType::Inline("cake"),
            TokenType::Newline,
            TokenType::Keyword(Keyword::Set),
            TokenType::Inline("foo"),
            TokenType::Inline("\"bar\""),
            TokenType::Newline,
            TokenType::Block("sh\necho {{ topping }}\n"),
            TokenType::Newline
        ]);
    }

    fn write_temp_file(filename: &str, contents: &str) -> (String, TempDir, File) {
        let dir = tempdir().unwrap();
        let file_path = dir.path().join(filename);
        let mut file = File::create(&file_path).unwrap();
        write!(file, "{}", contents).unwrap();
        (file_path.as_os_str().to_str().unwrap().to_owned(), dir, file)
    }
}