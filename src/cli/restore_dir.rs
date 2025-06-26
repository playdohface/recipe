use std::path::PathBuf;

#[derive(Debug)]
pub struct RestoreDir {
    original_dir: PathBuf,
}

impl RestoreDir {
    /// Returns a RAII guard that will restore the current working directory to what it was when dropped
    pub fn new() -> Self {
        let original_dir = std::env::current_dir().expect("Could not read current directory.");
        RestoreDir { original_dir }
    }
}
impl Drop for RestoreDir {
    fn drop(&mut self) {
        std::env::set_current_dir(&self.original_dir).expect("Failed to restore working directory")
    }
}
