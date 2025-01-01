use std::{collections::HashMap, process::Output};

use handlebars::Handlebars;
use serde::{Deserialize, Serialize};
use serde_json::{json, Map, Value};

use error::ExecutionError;

use crate::parser::{CodeBlock, Selection, SelectionPath, SetDirective, Val};

mod error;

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
#[derive(Debug, PartialEq, Serialize)]
pub struct Context {
    /// the root of the template context
    root: serde_json::Value,
    /// the loaded Executors
    executors: Executors,
}

impl Default for Context {
    fn default() -> Self {
        Context {
            root: json!({}),
            executors: Executors::default(),
        }
    }
}
impl Context {
    /// get a mutable reference to the base context-object
    pub fn map_mut(&mut self) -> &mut Map<String, Value> {
        self.root.as_object_mut().expect("root is always an object")
    }

    /// get a shared reference to the based context-object
    pub fn map(&self) -> &Map<String, Value> {
        self.root.as_object().expect("root is always an object")
    }

    /// get a shared reference to a value corresponding to a selection-path, or None if it is not found.
    /// Note that this value is still permitted to be serde_json::Value::Null
    pub fn get(&self, selection_path: &SelectionPath) -> Option<&Value> {
        self.root.pointer(&selection_path.json_pointer())
    }

    /// get a mutable reference to a value corresponding to a selection-path, or None if it is not found.
    /// Note that this value is still permitted to be serde_json::Value::Null
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

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
/// Represents a single loaded command
pub enum Command<'a> {
    Composite(Vec<Command<'a>>),
    #[serde(borrow)]
    CodeBlock(CodeBlock<'a>),
    SetDirective(SetDirective<'a>),
}
impl<'a> Command<'a> {
    pub fn execute(&self, ctx: &mut Context) -> Result<(), error::ExecutionError> {
        match self {
            Self::Composite(commands) => {
                for cmd in commands {
                    cmd.execute(ctx)?
                }
                Ok(())
            }
            Self::CodeBlock(code_block) => execute_code_block(code_block, ctx),
            Self::SetDirective(set_directive) => execute_set_directive(set_directive.clone(), ctx),
        }
    }
}

fn execute_code_block(code_block: &CodeBlock, ctx: &mut Context) -> Result<(), ExecutionError> {
    let executor = code_block.executor.ok_or(ExecutionError::MissingExecutor)?;
    let template = code_block.code;
    //TODO we may not want to construct this every time...
    let handlebars = Handlebars::new();
    let code = handlebars
        .render_template(template, &ctx.root)
        .map_err(|_e| ExecutionError::TemplateError)?;
    let output = ctx.executors.run(executor, code)?;
    ctx.register_execution(output, code_block.type_hint);
    Ok(())
}

/// Execute a Set-Directive (i.e. mutate the context)
/// TODO this should be broken up for better readability
fn execute_set_directive(
    set_directive: SetDirective,
    ctx: &mut Context,
) -> Result<(), ExecutionError> {
    let value = match set_directive.value {
        Val::ContextRef(s) => ctx.get(&s).ok_or(ExecutionError::ValueNotFound)?.clone(),
        Val::Literal(v) => v,
    };
    if let Some(existing) = ctx.get_mut(&set_directive.variable) {
        *existing = value;
    } else if set_directive.variable.0.len() == 1 {
        if let Some(Selection::Key(key)) = set_directive.variable.last() {
            ctx.map_mut().insert(key.to_string(), value);
        } else {
            return Err(ExecutionError::InvalidPath);
        }
    } else {
        let parent = ctx
            .get_mut(
                &set_directive
                    .variable
                    .parent()
                    .ok_or(ExecutionError::InvalidPath)?,
            )
            .ok_or(ExecutionError::InvalidPath)?;

        match set_directive.variable.last() {
            Some(Selection::Index(i)) => {
                if let Some(arr) = parent.as_array_mut() {
                    if arr.len() <= *i {
                        arr.extend(std::iter::repeat_n(Value::Null, i - arr.len()));
                        arr.push(value);
                    } else {
                        arr[*i] = value;
                    }
                } else {
                    return Err(ExecutionError::InvalidPath);
                }
            }
            Some(Selection::Key(key)) => {
                if let Some(obj) = parent.as_object_mut() {
                    obj.insert(key.to_string(), value);
                } else {
                    return Err(ExecutionError::InvalidPath);
                }
            }
            None => unreachable!("if it has a parent it must have a last element"),
        }
    }
    Ok(())
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
    fn can_execute_set_directive() {
        let mut ctx = Context {
            root: json!({}),
            executors: Executors::default(),
        };
        let literal = json!({
            "foo" : 1,
            "bar" : ["a", "b", "c"]
        });
        let set_directive = SetDirective {
            variable: SelectionPath(vec![Selection::Key("hello")]),
            value: Val::Literal(literal.clone()),
        };
        let res = execute_set_directive(set_directive, &mut ctx);
        assert_eq!(ctx.map(), json!({"hello" : literal}).as_object().unwrap());

        let literal = json!({
            "foo" : 2,
            "bar" : ["a"]
        });
        let set_directive = SetDirective {
            variable: SelectionPath(vec![Selection::Key("hello")]),
            value: Val::Literal(literal.clone()),
        };
        let res = execute_set_directive(set_directive, &mut ctx);
        assert_eq!(ctx.map(), json!({"hello" : literal}).as_object().unwrap());

        let set_directive = SetDirective {
            variable: SelectionPath(vec![
                Selection::Key("hello"),
                Selection::Key("bar"),
                Selection::Index(4),
            ]),
            value: Val::Literal(json!("here")),
        };
        let res = execute_set_directive(set_directive, &mut ctx);
        assert_eq!(
            ctx.map(),
            json! ({
                "hello" : {
                    "foo" : 2,
                    "bar" : ["a", null, null, null, "here"]
                }
            })
            .as_object()
            .unwrap()
        );

        let set_directive = SetDirective {
            variable: SelectionPath(vec![Selection::Key("hello"), Selection::Key("baz")]),
            value: Val::ContextRef(SelectionPath(vec![
                Selection::Key("hello"),
                Selection::Key("bar"),
                Selection::Index(4),
            ])),
        };
        let res = execute_set_directive(set_directive, &mut ctx);
        assert_eq!(
            ctx.map(),
            json! ({
                "hello" : {
                    "foo" : 2,
                    "bar" : ["a", null, null, null, "here"],
                    "baz" : "here"
                }
            })
            .as_object()
            .unwrap()
        );
    }

    #[test]
    fn can_execute_and_interpolate_codeblock() {
        let codeblock = shell_code_block("echo \"Hello, World!\"\n");
        let command = Command::CodeBlock(codeblock);
        let mut context = Context {
            root: json!({}),
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
            root: json!({}),
            executors: Executors::default(),
        };
        let res = command.execute(&mut context);
        dbg!(context);
    }
}
