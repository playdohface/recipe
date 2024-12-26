use std::{
    collections::{BTreeMap, HashMap},
    process::Output,
};

mod error;

use error::ExecutionError;
use handlebars::Handlebars;
use serde_json::{json, Value};

use crate::parser::CodeBlock;

trait Template {
    fn template(&self) -> &str;
}

impl<'a> Template for CodeBlock<'a> {
    fn template(&self) -> &str {
        self.code
    }
}

/// AppContext serves as the anchor for an invocation of recipe
#[derive(Debug, Default)]
pub struct AppContext<'a> {
    commands: Commands<'a>,
    ctx: Context,
}
impl<'a> AppContext<'a> {
    pub fn new() -> Self {
        AppContext {
            commands: Commands {
                inner: BTreeMap::new(),
            },
            ctx: Context {
                root: HashMap::new(),
                executors: Executors::default(),
            },
        }
    }
    pub fn register_command(&mut self, name: &'a str, command: Command<'a>) {
        self.commands.inner.insert(name.to_owned(), command);
    }
    pub fn register_variable(&mut self, name: &str, value: Value) {
        self.ctx.root.insert(name.to_owned(), value);
    }
    pub fn register_executor(&mut self, name: &str, executor: Executor) {
        self.ctx.executors.inner.insert(name.to_owned(), executor);
    }
}

/// Wraps all the loaded Commands
#[derive(Debug, Default)]
pub struct Commands<'a> {
    inner: BTreeMap<String, Command<'a>>,
}

#[derive(Debug)]
pub struct Executors {
    inner: HashMap<String, Executor>,
}
impl Default for Executors {
    fn default() -> Self {
        let mut inner = HashMap::new();
        inner.insert("sh".to_owned(), Executor::shell());
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
/// May need tweaking to accomodate for different types of scripts
#[derive(Debug, Clone)]
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
            .map_err(|e| ExecutionError::ExecutorFailed((self.clone(), e)))
    }
    pub fn shell() -> Self {
        Executor {
            program: "/bin/sh".to_owned(),
            args: vec!["-c".to_owned()],
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
#[derive(Debug, Default)]
pub struct Context {
    /// the root of the template context
    root: HashMap<String, serde_json::Value>,
    /// the loaded Executors
    executors: Executors,
}
impl Context {
    fn register_execution(&mut self, output: Output, type_hint: Option<&str>) {
        //TODO: implement type converters
        let stdout =
            json!(String::from_utf8(output.stdout).unwrap_or("TODO: Not UTF8!".to_owned()));
        let stderr =
            json!(String::from_utf8(output.stderr).unwrap_or("TODO: Not UTF8!".to_owned()));
        let statuscode = json!(output.status.code());
        self.root.insert("output".to_owned(), stdout);
        self.root.insert("error".to_owned(), stderr);
        self.root.insert("status".to_owned(), statuscode);
    }
}

#[derive(Debug)]
/// Represents a single loaded command
pub enum Command<'a> {
    Composite(Vec<Command<'a>>),
    CodeBlock(CodeBlock<'a>),
}
impl<'a> Command<'a> {
    fn execute(&self, ctx: &mut Context) -> Result<(), error::ExecutionError> {
        match self {
            Self::Composite(commands) => {
                for cmd in commands {
                    cmd.execute(ctx)?
                }
                Ok(())
            }
            Self::CodeBlock(codeblock) => {
                let executor = codeblock.executor.ok_or(ExecutionError::MissingExecutor)?;
                let template = codeblock.code;
                //TODO we may not want to construct this every time...
                let handlebars = Handlebars::new();
                let code = handlebars
                    .render_template(template, &ctx.root)
                    .map_err(|e| ExecutionError::TemplateError)?;
                let output = ctx.executors.run(executor, code)?;
                ctx.register_execution(output, codeblock.type_hint);
                Ok(())
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn shell_code_block(code: &str) -> CodeBlock {
        CodeBlock {
            executor: Some("sh".into()),
            name: None,
            type_hint: None,
            annotations: vec![],
            code: code.into(),
        }
    }

    #[test]
    fn can_execute_and_interpolate_codeblock() {
        let codeblock = shell_code_block("echo \"Hello, World!\"\n");
        let command = Command::CodeBlock(codeblock);
        let mut context = Context {
            root: HashMap::new(),
            executors: Executors::default(),
        };
        let res = command.execute(&mut context);
        assert!(res.is_ok());
        assert_eq!(
            context.root.get("output").unwrap(),
            &json!("Hello, World!\n")
        );
        let codeblock = shell_code_block("echo \"Output was: {{ output }}\"\n");
        let command = Command::CodeBlock(codeblock);
        let res = command.execute(&mut context);
        assert!(res.is_ok());
        assert_eq!(
            context.root.get("output").unwrap(),
            &json!("Output was: Hello, World!\n\n")
        );
    }

    #[test]
    fn can_run_generic_script() {
        let command = Command::CodeBlock(CodeBlock {
            executor: Some("node".into()),
            name: None,
            type_hint: None,
            annotations: vec![],
            code: "console.log(\"Hello from node\");".into(),
        });
        let mut context = Context {
            root: HashMap::new(),
            executors: Executors::default(),
        };
        let res = command.execute(&mut context);
        dbg!(context);
    }
}
