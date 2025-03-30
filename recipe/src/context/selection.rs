use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

use super::{error::ExecutionError, Context};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Selection<'a> {
    Index(usize),
    Key(&'a str),
}
impl<'a> Selection<'a> {
    fn as_string(&self) -> String {
        match self {
            Selection::Index(i) => i.to_string(),
            Selection::Key(k) => k.to_string(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SelectionPath<'a>(#[serde(borrow)] pub Vec<Selection<'a>>);
impl<'a> SelectionPath<'a> {
    /// the selection as a JSON-Pointer per [RFC6901](https://datatracker.ietf.org/doc/html/rfc6901)
    pub fn json_pointer(&self) -> String {
        let mut pointer = String::new();
        for s in self.0.iter() {
            // both Tilde ~ and Slash / are disallowed in names so we can skip escaping
            pointer = format!("{pointer}/{}", s.as_string())
        }
        pointer
    }

    /// Whether or not this path has segments
    pub fn is_direct(&self) -> bool {
        self.0.len() == 1
    }

    /// The last element
    pub fn last(&self) -> Option<&Selection> {
        self.0.last()
    }
    /// The selection up until the second-to-last element
    pub fn parent(&self) -> Option<SelectionPath> {
        if self.0.len() > 1 {
            let mut parent = self.0.clone();
            parent.pop();
            Some(SelectionPath(parent))
        } else {
            None
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum Val<'a> {
    ContextRef(#[serde(borrow)] SelectionPath<'a>),
    Literal(serde_json::Value),
}

// Sets a value at the given selection path in the context.
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
}
