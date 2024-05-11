use std::{collections::HashMap, rc::Rc};

use crate::interpreter::{InterpreterError, InterpreterResult, InterpreterValue};

pub struct Environment {
    parent_env: Option<Rc<Environment>>,
    variables: HashMap<String, InterpreterValue>,
}

impl Environment {
    pub fn assign(&mut self, name: String, value: InterpreterValue) {
        self.variables.insert(name, value);
    }

    pub fn retrieve(&self, name: &str) -> InterpreterResult<InterpreterValue> {
        let value = if let Some(value) = self.variables.get(name) {
            value.clone()
        } else if let Some(parent_env) = self.parent_env() {
            parent_env.retrieve(name)?
        } else {
            return Err(InterpreterError::Generic(format!(
                "Undefined variable '{name}'"
            )));
        };

        Ok(value)
    }

    pub fn parent_env(&self) -> Option<&Rc<Environment>> {
        self.parent_env.as_ref()
    }

    pub fn new(parent_env: Option<Rc<Environment>>) -> Self {
        Self {
            parent_env,
            variables: HashMap::new(),
        }
    }
}
