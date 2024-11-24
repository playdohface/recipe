use super::Executor;

pub enum ExecutionError {
    Generic,
    TemplateError,
    ExecutorFailed((Executor, std::io::Error)),
    MissingExecutor,
}
