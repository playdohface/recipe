use super::Executor;
use crate::parser::SelectionPath;
use thiserror::Error;

#[derive(Debug)]
pub enum ExecutionError {
    Generic,
    TemplateError,
    ExecutorFailed((Executor, std::io::Error)),
    MissingExecutor,
    ValueNotFound,
    InvalidPath,
}
