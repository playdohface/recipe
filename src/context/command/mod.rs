use code_block::CodeBlock;
use serde::{Deserialize, Serialize};
use set_directive::SetDirective;

use super::{error::ExecutionError, Context};

pub mod code_block;
pub mod set_directive;

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
/// Represents a single loaded command
pub enum Command<'a> {
    Composite(Vec<Command<'a>>),
    #[serde(borrow)]
    CodeBlock(CodeBlock<'a>),
    SetDirective(SetDirective<'a>),
}
impl<'a> Command<'a> {
    pub fn execute(&self, ctx: &mut Context) -> Result<(), ExecutionError> {
        match self {
            Self::Composite(commands) => {
                for cmd in commands {
                    cmd.execute(ctx)?
                }
                Ok(())
            }
            Self::CodeBlock(code_block) => code_block.execute(ctx),
            Self::SetDirective(set_directive) => set_directive.execute(ctx),
        }
    }
}

#[cfg(test)]
mod tests {}
