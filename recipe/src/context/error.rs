use thiserror::Error;
use super::Executor;

#[derive(Debug)]
pub enum ExecutionError {
    Generic,
    TemplateError,
    ExecutorFailed((Executor, std::io::Error)),
    MissingExecutor,
}
