use std::path::Path;

use insta::assert_yaml_snapshot;

use recipe::parser::{Heading, Keyword, Link, TokenType};
use recipe::parser::tokenizer::Tokenizer;

#[test]
fn test_scope_to_heading() {
    let src = r#"
# Heading 1
`inside heading 1`
## Heading 2 please ScopeMe-to_ThisHeading
`inside heading 2`
### Heading 3
`inside heading 3`
## Another heading
`inside another heading`
"#;
    let mut tokenizer = Tokenizer::from_str(src, Path::new("Mock.md"));
    tokenizer.scope_to_heading("heading-2-please-scope-me-to-this-heading");
    let tokens: Vec<_> = tokenizer.collect();
    assert_yaml_snapshot!(tokens);
}

#[test]
fn test_tokenizer() {
    let src = r#"
#Heading 1
Some text
```rust
### Not a heading
fn main() {
    println!("Hello, World!");
}
```
Some more text
[Link](https://example.com)
Set `foo` to `bar`
"#;
    let tokenizer = Tokenizer::from_str(src, Path::new("Mock.md"));
    let tokens: Vec<_> = tokenizer.collect();
    assert_yaml_snapshot!(tokens);
}
