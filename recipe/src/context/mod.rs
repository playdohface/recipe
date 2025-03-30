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
    let code = ctx
        .handlebars
        .render_template(template, &ctx.root)
        .map_err(|_e| ExecutionError::TemplateError)?;
    let output = ctx.executors.run(executor, code)?;
    ctx.register_execution(output, code_block.type_hint);
    Ok(())
}

/// Execute a Set-Directive (i.e. mutate the context)
fn execute_set_directive(
    set_directive: SetDirective,
    ctx: &mut Context,
) -> Result<(), ExecutionError> {
    let value = match set_directive.value {
        Val::ContextRef(s) => ctx.get(&s).ok_or(ExecutionError::ValueNotFound)?.clone(),
        Val::Literal(v) => v,
    };

    set_value_at_path(ctx, &set_directive.variable, value)
}

/// Sets a value at the given selection path in the context.
/// This is the main entry point that delegates to more specific functions.
pub fn set_value_at_path(
    ctx: &mut Context,
    path: &SelectionPath,
    value: serde_json::Value,
) -> Result<(), ExecutionError> {
    // If the path is empty, we can't set anything
    if path.0.is_empty() {
        return Err(ExecutionError::InvalidPath);
    }

    // If the path is direct (just one key), set it directly in the root
    if path.is_direct() {
        return set_direct_path(ctx, path, value);
    }

    // For compound paths, navigate and create as needed
    set_compound_path(ctx, path, value)
}

/// Sets a direct path (single key) in the context
fn set_direct_path(
    ctx: &mut Context,
    path: &SelectionPath,
    value: serde_json::Value,
) -> Result<(), ExecutionError> {
    if let Some(Selection::Key(key)) = path.last() {
        ctx.map_mut().insert(key.to_string(), value);
        Ok(())
    } else {
        // Direct path should be a key at root level
        Err(ExecutionError::InvalidPath)
    }
}

/// Sets a value at a compound path (more than one level deep)
fn set_compound_path(
    ctx: &mut Context,
    path: &SelectionPath,
    value: serde_json::Value,
) -> Result<(), ExecutionError> {
    let mut current = &mut ctx.root;

    // Process all segments except the last one
    for (idx, segment) in path.0.iter().enumerate() {
        // Skip the last segment since we'll handle it separately
        if idx == path.0.len() - 1 {
            break;
        }

        current = navigate_or_create_segment(current, segment)?;
    }

    // Set value at the last segment
    set_value_at_segment(current, path.0.last().unwrap(), value)
}

/// Navigates to a specific segment, creating intermediate structures if needed
fn navigate_or_create_segment<'a>(
    current: &'a mut Value,
    segment: &Selection,
) -> Result<&'a mut Value, ExecutionError> {
    match segment {
        Selection::Key(key) => navigate_or_create_key(current, key),
        Selection::Index(idx) => navigate_or_create_index(current, *idx),
    }
}

/// Navigates to a specific key, creating it if needed
fn navigate_or_create_key<'a>(
    current: &'a mut Value,
    key: &str,
) -> Result<&'a mut Value, ExecutionError> {
    // Convert to object if null
    if current.is_null() {
        *current = json!({});
    }

    // Check if we have an object
    if !current.is_object() {
        return Err(ExecutionError::InvalidPath);
    }

    let obj = current.as_object_mut().unwrap();

    // Create key if needed
    if !obj.contains_key(key) {
        obj.insert(key.to_string(), json!(null));
    }

    // Return reference to the key's value
    Ok(&mut obj[key])
}

/// Navigates to a specific index, creating it if needed
fn navigate_or_create_index<'a>(
    current: &'a mut Value,
    idx: usize,
) -> Result<&'a mut Value, ExecutionError> {
    // Convert to array if null
    if current.is_null() {
        *current = json!([]);
    }

    // Check if we have an array
    if !current.is_array() {
        return Err(ExecutionError::InvalidPath);
    }

    let arr = current.as_array_mut().unwrap();

    // Extend array if needed
    if arr.len() <= idx {
        arr.extend(std::iter::repeat_n(json!(null), idx - arr.len() + 1));
    }

    // Return reference to the index's value
    Ok(&mut arr[idx])
}

/// Sets a value at a specific segment (the final part of the path)
fn set_value_at_segment(
    current: &mut Value,
    segment: &Selection,
    value: Value,
) -> Result<(), ExecutionError> {
    match segment {
        Selection::Key(key) => {
            // Convert to object if null
            if current.is_null() {
                *current = json!({});
            }

            // Check if we have an object
            if !current.is_object() {
                return Err(ExecutionError::InvalidPath);
            }

            // Set the value
            current
                .as_object_mut()
                .unwrap()
                .insert(key.to_string(), value);
            Ok(())
        }
        Selection::Index(idx) => {
            // Convert to array if null
            if current.is_null() {
                *current = json!([]);
            }

            // Check if we have an array
            if !current.is_array() {
                return Err(ExecutionError::InvalidPath);
            }

            // Set the value
            let arr = current.as_array_mut().unwrap();
            if arr.len() <= *idx {
                arr.extend(std::iter::repeat_n(json!(null), *idx - arr.len() + 1));
            }
            arr[*idx] = value;
            Ok(())
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parser::Selection;
    use crate::parser::SelectionPath;
    use serde_json::json;

    #[test]
    fn test_set_directive_creates_nested_structures() {
        let mut ctx = Context::default();

        // Create a deeply nested structure
        let set_directive = SetDirective {
            variable: SelectionPath(vec![
                Selection::Key("users"),
                Selection::Index(0),
                Selection::Key("profile"),
                Selection::Key("contact"),
                Selection::Key("email"),
            ]),
            value: Val::Literal(json!("user@example.com")),
        };

        let res = execute_set_directive(set_directive, &mut ctx);
        assert!(res.is_ok());

        // Verify the nested structure was created
        assert_eq!(
            ctx.root,
            json!({
                "users": [{
                    "profile": {
                        "contact": {
                            "email": "user@example.com"
                        }
                    }
                }]
            })
        );
    }

    #[test]
    fn test_set_directive_with_complex_reference() {
        let mut ctx = Context {
            root: json!({
                "data": {
                    "items": [
                        {"id": 1, "value": "first"},
                        {"id": 2, "value": "second"},
                    ]
                }
            }),
            executors: Executors::default(),
            handlebars: Handlebars::new(),
        };

        // Copy a nested value using context reference
        let set_directive = SetDirective {
            variable: SelectionPath(vec![Selection::Key("selected")]),
            value: Val::ContextRef(SelectionPath(vec![
                Selection::Key("data"),
                Selection::Key("items"),
                Selection::Index(1),
                Selection::Key("value"),
            ])),
        };

        let res = execute_set_directive(set_directive, &mut ctx);
        assert!(res.is_ok());

        // Verify the reference was copied correctly
        assert_eq!(
            ctx.root,
            json!({
                "data": {
                    "items": [
                        {"id": 1, "value": "first"},
                        {"id": 2, "value": "second"},
                    ]
                },
                "selected": "second"
            })
        );
    }

    #[test]
    fn test_set_directive_with_nonexistent_reference() {
        let mut ctx = Context::default();

        // Try to reference a path that doesn't exist
        let set_directive = SetDirective {
            variable: SelectionPath(vec![Selection::Key("result")]),
            value: Val::ContextRef(SelectionPath(vec![
                Selection::Key("nonexistent"),
                Selection::Key("path"),
            ])),
        };

        let res = execute_set_directive(set_directive, &mut ctx);

        // Should fail with ValueNotFound error
        assert!(res.is_err());
        assert!(matches!(res, Err(ExecutionError::ValueNotFound)));

        // Context should remain unchanged
        assert_eq!(ctx.root, json!({}));
    }

    #[test]
    fn test_set_directive_overwrite_existing_value() {
        let mut ctx = Context {
            root: json!({
                "config": {
                    "timeout": 30,
                    "retries": 3
                }
            }),
            executors: Executors::default(),
            handlebars: Handlebars::new(),
        };

        // Overwrite an existing value
        let set_directive = SetDirective {
            variable: SelectionPath(vec![Selection::Key("config"), Selection::Key("timeout")]),
            value: Val::Literal(json!(60)),
        };

        let res = execute_set_directive(set_directive, &mut ctx);
        assert!(res.is_ok());

        // Verify the value was overwritten
        assert_eq!(
            ctx.root,
            json!({
                "config": {
                    "timeout": 60,
                    "retries": 3
                }
            })
        );
    }

    #[test]
    fn test_set_directive_mixed_creation_patterns() {
        let mut ctx = Context::default();

        // First create an array with a specific index
        let set_directive1 = SetDirective {
            variable: SelectionPath(vec![Selection::Key("data"), Selection::Index(2)]),
            value: Val::Literal(json!("value")),
        };

        let res = execute_set_directive(set_directive1, &mut ctx);
        assert!(res.is_ok());

        // Verify array was created with nulls for missing indices
        assert_eq!(
            ctx.root,
            json!({
                "data": [null, null, "value"]
            })
        );

        // Now set something in the first element of that array
        let set_directive2 = SetDirective {
            variable: SelectionPath(vec![
                Selection::Key("data"),
                Selection::Index(0),
                Selection::Key("nested"),
            ]),
            value: Val::Literal(json!({"status": "ok"})),
        };

        let res = execute_set_directive(set_directive2, &mut ctx);
        assert!(res.is_ok());

        // Verify the null was converted to an object with the nested field
        assert_eq!(
            ctx.root,
            json!({
                "data": [
                    {"nested": {"status": "ok"}},
                    null,
                    "value"
                ]
            })
        );
    }

    #[test]
    fn test_set_directive_type_conversion_errors() {
        let mut ctx = Context {
            root: json!({
                "number": 123,
                "array": [1, 2, 3]
            }),
            executors: Executors::default(),
            handlebars: Handlebars::new(),
        };

        // Try to set a key in a number (should fail)
        let set_directive1 = SetDirective {
            variable: SelectionPath(vec![Selection::Key("number"), Selection::Key("key")]),
            value: Val::Literal(json!("value")),
        };

        let res = execute_set_directive(set_directive1, &mut ctx);
        assert!(res.is_err());
        assert!(matches!(res, Err(ExecutionError::InvalidPath)));

        // Try to set an index in an object (should fail)
        let set_directive2 = SetDirective {
            variable: SelectionPath(vec![Selection::Key("number"), Selection::Index(0)]),
            value: Val::Literal(json!("value")),
        };

        let res = execute_set_directive(set_directive2, &mut ctx);
        assert!(res.is_err());
        assert!(matches!(res, Err(ExecutionError::InvalidPath)));
    }

    #[test]
    fn test_direct_path() {
        let mut ctx = Context::default();

        // Set a simple root key
        let path = SelectionPath(vec![Selection::Key("hello")]);
        let result = set_value_at_path(&mut ctx, &path, json!("world"));

        assert!(result.is_ok());
        assert_eq!(ctx.root.get("hello").unwrap(), &json!("world"));
    }

    #[test]
    fn test_direct_path_with_index() {
        let mut ctx = Context::default();

        // Direct path with index is invalid
        let path = SelectionPath(vec![Selection::Index(0)]);
        let result = set_direct_path(&mut ctx, &path, json!("value"));

        assert!(result.is_err());
        assert!(matches!(result, Err(ExecutionError::InvalidPath)));
    }

    #[test]
    fn test_navigate_or_create_key() {
        // Test with existing object
        let mut value = json!({"existing": "value"});
        let result = navigate_or_create_key(&mut value, "new");

        assert!(result.is_ok());
        *result.unwrap() = json!("created");
        assert_eq!(value, json!({"existing": "value", "new": "created"}));

        // Test with null
        let mut value = json!(null);
        let result = navigate_or_create_key(&mut value, "key");

        assert!(result.is_ok());
        *result.unwrap() = json!("value");
        assert_eq!(value, json!({"key": "value"}));

        // Test with incompatible type
        let mut value = json!(123);
        let result = navigate_or_create_key(&mut value, "key");

        assert!(result.is_err());
        assert!(matches!(result, Err(ExecutionError::InvalidPath)));
    }

    #[test]
    fn test_navigate_or_create_index() {
        // Test with existing array
        let mut value = json!([1, 2, 3]);
        let result = navigate_or_create_index(&mut value, 1);

        assert!(result.is_ok());
        *result.unwrap() = json!(99);
        assert_eq!(value, json!([1, 99, 3]));

        // Test extending array
        let result = navigate_or_create_index(&mut value, 5);

        assert!(result.is_ok());
        *result.unwrap() = json!("end");
        assert_eq!(value, json!([1, 99, 3, null, null, "end"]));

        // Test with null
        let mut value = json!(null);
        let result = navigate_or_create_index(&mut value, 2);

        assert!(result.is_ok());
        *result.unwrap() = json!("value");
        assert_eq!(value, json!([null, null, "value"]));

        // Test with incompatible type
        let mut value = json!({"not": "array"});
        let result = navigate_or_create_index(&mut value, 0);

        assert!(result.is_err());
        assert!(matches!(result, Err(ExecutionError::InvalidPath)));
    }

    #[test]
    fn test_set_value_at_segment() {
        // Test setting key in object
        let mut value = json!({"existing": "value"});
        let result = set_value_at_segment(&mut value, &Selection::Key("new"), json!("created"));

        assert!(result.is_ok());
        assert_eq!(value, json!({"existing": "value", "new": "created"}));

        // Test setting index in array
        let mut value = json!([1, 2, 3]);
        let result = set_value_at_segment(&mut value, &Selection::Index(4), json!("value"));

        assert!(result.is_ok());
        assert_eq!(value, json!([1, 2, 3, null, "value"]));

        // Test setting key in null (converts to object)
        let mut value = json!(null);
        let result = set_value_at_segment(&mut value, &Selection::Key("key"), json!("value"));

        assert!(result.is_ok());
        assert_eq!(value, json!({"key": "value"}));

        // Test setting index in null (converts to array)
        let mut value = json!(null);
        let result = set_value_at_segment(&mut value, &Selection::Index(2), json!("value"));

        assert!(result.is_ok());
        assert_eq!(value, json!([null, null, "value"]));
    }

    #[test]
    fn test_set_compound_path() {
        let mut ctx = Context::default();

        // Set nested object path
        let path = SelectionPath(vec![
            Selection::Key("user"),
            Selection::Key("profile"),
            Selection::Key("name"),
        ]);

        let result = set_value_at_path(&mut ctx, &path, json!("John Doe"));

        assert!(result.is_ok());
        assert_eq!(ctx.root, json!({"user": {"profile": {"name": "John Doe"}}}));

        // Set array index in nested path
        let path = SelectionPath(vec![
            Selection::Key("user"),
            Selection::Key("hobbies"),
            Selection::Index(2),
        ]);

        let result = set_value_at_path(&mut ctx, &path, json!("hiking"));

        assert!(result.is_ok());
        assert_eq!(
            ctx.root,
            json!({
                "user": {
                    "profile": {"name": "John Doe"},
                    "hobbies": [null, null, "hiking"]
                }
            })
        );
    }

    #[test]
    fn test_mixed_path_types() {
        let mut ctx = Context::default();

        // Create a mixed path with arrays and objects
        let path = SelectionPath(vec![
            Selection::Key("data"),
            Selection::Index(0),
            Selection::Key("values"),
            Selection::Index(1),
        ]);

        let result = set_value_at_path(&mut ctx, &path, json!("target"));

        assert!(result.is_ok());
        assert_eq!(ctx.root, json!({"data": [{"values": [null, "target"]}]}));
    }

    #[test]
    fn test_path_traversal_errors() {
        let mut ctx = Context::default();

        // Set initial state
        ctx.root = json!({
            "scalar": 123,
            "array": [1, 2, 3]
        });

        // Try to treat scalar as object
        let path = SelectionPath(vec![Selection::Key("scalar"), Selection::Key("invalid")]);

        let result = set_value_at_path(&mut ctx, &path, json!("value"));
        assert!(result.is_err());
        assert!(matches!(result, Err(ExecutionError::InvalidPath)));

        // Try to treat array as object
        let path = SelectionPath(vec![Selection::Key("array"), Selection::Key("invalid")]);

        let result = set_value_at_path(&mut ctx, &path, json!("value"));
        assert!(result.is_err());
        assert!(matches!(result, Err(ExecutionError::InvalidPath)));

        // Try to treat scalar as array
        let path = SelectionPath(vec![Selection::Key("scalar"), Selection::Index(0)]);

        let result = set_value_at_path(&mut ctx, &path, json!("value"));
        assert!(result.is_err());
        assert!(matches!(result, Err(ExecutionError::InvalidPath)));
    }

    #[test]
    fn test_empty_path() {
        let mut ctx = Context::default();
        let path = SelectionPath(vec![]);

        let result = set_value_at_path(&mut ctx, &path, json!("value"));
        assert!(result.is_err());
        assert!(matches!(result, Err(ExecutionError::InvalidPath)));
    }

    #[test]
    fn can_deep_set_array_element() {
        let mut ctx = Context {
            root: json!({}),
            executors: Executors::default(),
            handlebars: Handlebars::new(),
        };
        let set_directive = SetDirective {
            variable: SelectionPath(vec![
                Selection::Key("foo"),
                Selection::Index(0),
                Selection::Key("bar"),
            ]),
            value: Val::Literal(json!("baz")),
        };
        let _res = execute_set_directive(set_directive, &mut ctx);
        assert_eq!(
            ctx.map(),
            json!({
                "foo": [{"bar": "baz"}]
            })
            .as_object()
            .unwrap()
        );
    }

    #[test]
    fn can_execute_set_directive() {
        let mut ctx = Context {
            root: json!({}),
            executors: Executors::default(),
            handlebars: Handlebars::new(),
        };
        let literal = json!({
            "foo" : 1,
            "bar" : ["a", "b", "c"]
        });
        let set_directive = SetDirective {
            variable: SelectionPath(vec![Selection::Key("hello")]),
            value: Val::Literal(literal.clone()),
        };
        let _res = execute_set_directive(set_directive, &mut ctx);
        assert_eq!(ctx.map(), json!({"hello" : literal}).as_object().unwrap());

        let literal = json!({
            "foo" : 2,
            "bar" : ["a"]
        });
        let set_directive = SetDirective {
            variable: SelectionPath(vec![Selection::Key("hello")]),
            value: Val::Literal(literal.clone()),
        };
        let _res = execute_set_directive(set_directive, &mut ctx);
        assert_eq!(ctx.map(), json!({"hello" : literal}).as_object().unwrap());

        let set_directive = SetDirective {
            variable: SelectionPath(vec![
                Selection::Key("hello"),
                Selection::Key("bar"),
                Selection::Index(4),
            ]),
            value: Val::Literal(json!("here")),
        };
        let _res = execute_set_directive(set_directive, &mut ctx);
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
        let _res = execute_set_directive(set_directive, &mut ctx);
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

    pub fn shell_code_block(code: &str) -> CodeBlock {
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
            root: json!({}),
            executors: Executors::default(),
            handlebars: Handlebars::new(),
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
            handlebars: Handlebars::new(),
        };
        let _res = command.execute(&mut context);
        dbg!(context);
    }
}
