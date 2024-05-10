use std::collections::HashMap;

use crate::interpreter::{InterpreterError, InterpreterResult, InterpreterValue};

pub struct Environment {
    variables: HashMap<String, InterpreterValue>,
}

impl Environment {
    pub fn define(&mut self, name: String, value: InterpreterValue) {
        self.variables.insert(name, value);
    }

    pub fn get(&self, name: &str) -> InterpreterResult<InterpreterValue> {
        let Some(value) = self.variables.get(name) else {
            return Err(InterpreterError::Generic(format!(
                "Undefined variable '{name}'"
            )));
        };

        Ok(value.clone())
    }

    pub fn new() -> Self {
        Self {
            variables: HashMap::new(),
        }
    }
}
