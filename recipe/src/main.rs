fn main() {
    let recipe = std::fs::read_to_string("recipes/module.md").unwrap();
    println!("{:?}", std::env::current_dir());
    println!("{:?}", recipe);
    let parser = recipe::parser::Tokenizer::from_str(&recipe);
    for token in parser {
        println!("{:?}", token.inner());
    }

    // let foo: serde_json::Value = json5::from_str("{foo : 4, bar : [1,2]}").unwrap();
    // dbg!(foo);
}
