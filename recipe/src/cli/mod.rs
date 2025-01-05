use std::fs;
use std::path::{Path, PathBuf};

use convert_case::{Case, Casing};

use crate::cli::restore_dir::RestoreDir;
use crate::context::{Command, Context};
use crate::loader::load;

mod restore_dir;

/// The main entry point
pub fn run() {
    let mut args = std::env::args().skip(1).collect::<Vec<_>>();

    let root_file_name = "Readme.md"; //TODO config

    let _cwd_guard = RestoreDir::new();
    let origin_working_directory = std::env::current_dir().expect("By previous call of RestoreDir::new");

    let root_path = find_closest_parent_containing(root_file_name, origin_working_directory).expect("Could not find Recipe.md in current directory or any of its parents.");
    std::env::set_current_dir(&root_path).expect("Could not change working directory.");
    let recipe_path = root_path.join(root_file_name);

    let commands = load(&recipe_path).unwrap();
    
    let mut args_to_register = Vec::new();
    while !args.is_empty() {
        if let Some(command) = commands.get(&args.join("-").to_case(Case::Kebab)) {
            args_to_register.reverse();
            execute_with_new_context(command, args_to_register);
            break;
        } else {
            args_to_register.push(args.pop().expect("by loop invariant"));
        }
    }
}

fn execute_with_new_context(command: &Command, args: Vec<String>) {
    let mut ctx = Context::default();
    ctx.map_mut()
        .insert("args".to_string(), args.into());
    let _res = command.execute(&mut ctx);
    print!(
        "{}",
        ctx.map()
            .get("output")
            .and_then(|c| c.as_str())
            .unwrap_or_default()
    );
}

fn find_closest_parent_containing(file_name: &str, start_dir: impl AsRef<Path>) -> Option<PathBuf> {
    let start_dir = start_dir.as_ref();
    loop {
        let file_path = start_dir.join(file_name);
        if fs::metadata(&file_path).is_ok() {
            return Some(start_dir.into());
        }
        return match start_dir.parent() {
            Some(parent) => find_closest_parent_containing(file_name, parent),
            None => None
        }
    }
}
