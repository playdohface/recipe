use std::fs;
use std::path::{Path, PathBuf};

use anyhow::anyhow;
use convert_case::{Case, Casing};

use crate::cli::restore_dir::RestoreDir;
use crate::context::{Command, Commands, Context};
use crate::loader::load;

mod restore_dir;

/// The main entry point
pub fn run() -> anyhow::Result<()> {
    //TODO read config, set up logging
    let root_file_name = "Readme.md";

    // find a Recipe
    let root_path = find_closest_parent_containing(root_file_name, std::env::current_dir()?)
        .ok_or(anyhow!(
            "Could not find Recipe.md in current directory or any of its parents"
        ))?;
    let recipe_path = root_path.join(root_file_name);

    // set up working directory to the Path ot the recipe
    let _cwd_guard = RestoreDir::new(); // dropping this guard will restore the directory
    std::env::set_current_dir(&root_path)?;

    // Parse and load the recipe
    let commands = load(&recipe_path)?;

    // find the corresponding Command from the loaded recipe
    let (command, remaining_args) = find_command_from_args(&commands)?;

    // Execute the command
    execute_with_new_context(command, remaining_args)?;

    Ok(())
}

fn find_command_from_args<'a>(
    commands: &'a Commands,
) -> anyhow::Result<(&'a Command<'a>, Vec<String>)> {
    let mut args = std::env::args().skip(1).collect::<Vec<_>>();
    let mut remaining_args = Vec::new();
    while !args.is_empty() {
        if let Some(command) = commands.get(&args.join("-").to_case(Case::Kebab)) {
            remaining_args.reverse();
            return Ok((command, remaining_args));
        } else {
            remaining_args.push(args.pop().expect("by loop invariant"));
        }
    }
    Err(anyhow!(
        "Could not find matching Recipe for {}",
        args.join(" ")
    ))
}

fn execute_with_new_context(command: &Command, args: Vec<String>) -> anyhow::Result<()> {
    let mut ctx = Context::default();
    ctx.map_mut().insert("args".to_string(), args.into());
    command.execute(&mut ctx)?;
    print!(
        "{}",
        ctx.map()
            .get("output")
            .and_then(|c| c.as_str())
            .unwrap_or_default()
    );
    Ok(())
}

fn find_closest_parent_containing(file_name: &str, start_dir: impl AsRef<Path>) -> Option<PathBuf> {
    let start_dir = start_dir.as_ref();

    let file_path = start_dir.join(file_name);
    if fs::metadata(&file_path).is_ok() {
        return Some(start_dir.into());
    }
    return match start_dir.parent() {
        Some(parent) => find_closest_parent_containing(file_name, parent),
        None => None,
    };
}
