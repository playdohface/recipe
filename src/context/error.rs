use thiserror::Error;

use super::Executor;

#[derive(Error, Debug)]
pub enum ExecutionError {
    #[error("An error occurred during execution.")]
    Generic,
    #[error("An error occurred while processing the template.")]
    TemplateError,
    #[error("Executor {0:?} failed: {1}")]
    ExecutorFailed(Executor, std::io::Error),
    #[error("No executor found.")]
    MissingExecutor,
    #[error("Value not found.")]
    ValueNotFound,
    #[error("Invalid path provided.")]
    InvalidPath,
}
