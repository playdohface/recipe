use serde::{Deserialize, Serialize};

use crate::context::{
    error::ExecutionError,
    selection::{set_value_at_path, SelectionPath, Val},
    Context,
};

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SetDirective<'a> {
    #[serde(borrow)]
    pub variable: SelectionPath<'a>,
    pub value: Val<'a>,
}

impl<'a> SetDirective<'a> {
    pub fn execute(&self, ctx: &mut Context) -> Result<(), ExecutionError> {
        //TODO avoid this clone?
        let value = match self.value.clone() {
            Val::ContextRef(s) => ctx.get(&s).ok_or(ExecutionError::ValueNotFound)?.clone(),
            Val::Literal(v) => v,
        };

        set_value_at_path(ctx, &self.variable, value)
    }
}

#[cfg(test)]
mod tests {

    use handlebars::Handlebars;
    use serde_json::json;

    use crate::context::{selection::Selection, Executors};

    use super::*;

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

        let res = set_directive.execute(&mut ctx);
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

        let res = set_directive.execute(&mut ctx);
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

        let res = set_directive.execute(&mut ctx);

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

        let res = set_directive.execute(&mut ctx);
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

        let res = set_directive1.execute(&mut ctx);
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

        let res = set_directive2.execute(&mut ctx);
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

        let res = set_directive1.execute(&mut ctx);
        assert!(res.is_err());
        assert!(matches!(res, Err(ExecutionError::InvalidPath)));

        // Try to set an index in an object (should fail)
        let set_directive2 = SetDirective {
            variable: SelectionPath(vec![Selection::Key("number"), Selection::Index(0)]),
            value: Val::Literal(json!("value")),
        };

        let res = set_directive2.execute(&mut ctx);
        assert!(res.is_err());
        assert!(matches!(res, Err(ExecutionError::InvalidPath)));
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
        let _res = set_directive.execute(&mut ctx);
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
        let _res = set_directive.execute(&mut ctx);
        assert_eq!(ctx.map(), json!({"hello" : literal}).as_object().unwrap());

        let literal = json!({
            "foo" : 2,
            "bar" : ["a"]
        });
        let set_directive = SetDirective {
            variable: SelectionPath(vec![Selection::Key("hello")]),
            value: Val::Literal(literal.clone()),
        };
        let _res = set_directive.execute(&mut ctx);
        assert_eq!(ctx.map(), json!({"hello" : literal}).as_object().unwrap());

        let set_directive = SetDirective {
            variable: SelectionPath(vec![
                Selection::Key("hello"),
                Selection::Key("bar"),
                Selection::Index(4),
            ]),
            value: Val::Literal(json!("here")),
        };
        let _res = set_directive.execute(&mut ctx);
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
        let _res = set_directive.execute(&mut ctx);
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
}
