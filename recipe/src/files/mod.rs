/// Resolve a path and return the contents of the file
/// TODO - There will be relative paths and default locations
pub fn read(path: &str) -> Result<String, std::io::Error> {
    std::fs::read_to_string(path)
}
