use serde::{Deserialize, Serialize};

use crate::context::{error::ExecutionError, Context};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct CodeBlock<'a> {
    pub executor: Option<&'a str>,
    pub name: Option<&'a str>,
    pub type_hint: Option<&'a str>,
    pub annotations: Vec<&'a str>,
    pub code: &'a str,
}

impl<'a> CodeBlock<'a> {
    pub fn execute(&self, ctx: &mut Context) -> Result<(), ExecutionError> {
        let executor = self.executor.ok_or(ExecutionError::MissingExecutor)?;
        let template = self.code;
        let code = ctx
            .handlebars
            .render_template(template, &ctx.root)
            .map_err(|_e| ExecutionError::TemplateError)?;
        let output = ctx.executors.run(executor, code)?;
        ctx.register_execution(output, self.type_hint);
        Ok(())
    }
}

#[cfg(test)]
mod tests {

    use crate::context::{command::Command, Executors};

    use super::*;
    use handlebars::Handlebars;
    use serde_json::json;

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
