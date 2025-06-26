use std::{collections::HashMap, process::Output};

use handlebars::Handlebars;
use selection::SelectionPath;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};

use error::ExecutionError;

use command::Command;

pub mod command;
pub mod error;
pub mod selection;

/// Wraps all the loaded Commands
#[derive(Debug, Default, PartialEq, Serialize, Deserialize)]
pub struct Commands<'a> {
    #[serde(borrow)]
    inner: HashMap<String, Command<'a>>,
}
impl<'a> Commands<'a> {
    pub fn new() -> Self {
        Commands {
            inner: HashMap::new(),
        }
    }
    pub fn register_command(&mut self, name: String, command: Command<'a>) {
        self.inner.insert(name, command);
    }
    pub fn get(&self, name: &str) -> Option<&Command> {
        self.inner.get(name)
    }
}

#[derive(Debug, PartialEq, Serialize)]
pub struct Executors {
    inner: HashMap<String, Executor>,
}
impl Default for Executors {
    fn default() -> Self {
        let mut inner = HashMap::new();
        inner.insert("sh".to_owned(), Executor::shell());
        inner.insert("node".to_string(), Executor::node());
        Executors { inner }
    }
}
impl Executors {
    fn run(&self, name: &str, code: String) -> Result<Output, ExecutionError> {
        if let Some(exec) = self.inner.get(name) {
            exec.run(code)
        } else {
            Executor::generic(name).run(code)
        }
    }
}

/// Something that can execute a script of a certain type
/// May need tweaking to accommodate for different types of scripts
#[derive(Debug, Clone, PartialEq, Serialize)]
pub struct Executor {
    program: String,
    args: Vec<String>,
}
impl Executor {
    fn run(&self, code: String) -> Result<Output, ExecutionError> {
        std::process::Command::new(&self.program)
            .args(&self.args)
            .arg(&code)
            .output()
            .map_err(|e| ExecutionError::ExecutorFailed(self.clone(), e))
    }
    pub fn shell() -> Self {
        Executor {
            program: "/bin/sh".to_owned(),
            args: vec!["-c".to_owned()],
        }
    }
    pub fn node() -> Self {
        Executor {
            program: "node".to_string(),
            args: vec!["--eval".to_string()],
        }
    }
    fn generic(executable: &str) -> Self {
        Executor {
            program: executable.to_owned(),
            args: vec!["-".to_owned()],
        }
    }
}

/// Wraps the context passed to each command-invocation
#[derive(Debug)]
pub struct Context<'a> {
    /// the root of the template context
    root: serde_json::Value,
    /// the loaded Executors
    executors: Executors,
    /// Handlebars template registry instance
    handlebars: Handlebars<'a>,
}

impl Default for Context<'_> {
    fn default() -> Self {
        Context {
            root: json!({}),
            executors: Executors::default(),
            handlebars: Handlebars::new(),
        }
    }
}
impl Context<'_> {
    /// get a mutable reference to the base context-object
    pub fn map_mut(&mut self) -> &mut Map<String, Value> {
        self.root.as_object_mut().expect("root is always an object")
    }

    /// get a shared reference to the base context-object
    pub fn map(&self) -> &Map<String, Value> {
        self.root.as_object().expect("root is always an object")
    }

    /// get a shared reference to a value corresponding to a selection-path, or None if it is not found.
    /// Note that this value might be Some(serde_json::Value::Null)
    pub fn get(&self, selection_path: &SelectionPath) -> Option<&Value> {
        self.root.pointer(&selection_path.json_pointer())
    }

    /// get a mutable reference to a value corresponding to a selection-path, or None if it is not found.
    /// Note that this value might be Some(serde_json::Value::Null)
    pub fn get_mut(&mut self, selection_path: &SelectionPath) -> Option<&mut Value> {
        self.root.pointer_mut(&selection_path.json_pointer())
    }

    /// The actions to be taken after a command has been executed against this context-instance
    pub fn register_execution(&mut self, output: Output, _type_hint: Option<&str>) {
        //TODO: implement type converters
        let stdout =
            json!(String::from_utf8(output.stdout).unwrap_or("TODO: Not UTF8!".to_owned()));
        let stderr =
            json!(String::from_utf8(output.stderr).unwrap_or("TODO: Not UTF8!".to_owned()));
        let statuscode = json!(output.status.code());
        self.map_mut().insert("output".to_owned(), stdout);
        self.map_mut().insert("error".to_owned(), stderr);
        self.map_mut().insert("status".to_owned(), statuscode);
    }
}
