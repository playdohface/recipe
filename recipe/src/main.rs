use convert_case::{Case, Casing};

use recipe::context::Context;
use recipe::loader::load;

fn main() {
    let mut args = std::env::args().skip(1).collect::<Vec<_>>();
    let mut args_to_register = Vec::new();
    let commands = load("Readme.md").unwrap();
    while !args.is_empty() {
        if let Some(command) = commands.get(&args.join("-").to_case(Case::Kebab)) {
            let mut ctx = Context::default();
            args_to_register.reverse();
            ctx.map_mut()
                .insert("args".to_string(), args_to_register.into());
            let _res = command.execute(&mut ctx);
            print!(
                "{}",
                ctx.map()
                    .get("output")
                    .and_then(|c| c.as_str())
                    .unwrap_or_default()
            );
            break;
        } else {
            args_to_register.push(args.pop().expect("by loop invariant"));
        }
    }
}
