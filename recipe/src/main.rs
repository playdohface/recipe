fn main() {
    let recipe = std::fs::read_to_string("recipes/module.md").unwrap();
    println!("{:?}", std::env::current_dir());
    println!("{:?}", recipe);
    let parser = recipe::parser::RecipeParser::from_str(&recipe);
    for token in parser {
        println!("{:?}", token.inner());
    }
}
