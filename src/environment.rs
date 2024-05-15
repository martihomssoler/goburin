use std::{borrow::BorrowMut, cell::RefCell, collections::HashMap, rc::Rc};

use crate::interpreter::{InterpreterError, InterpreterResult, InterpreterValue};

pub struct Environment {
    parent_env: Option<Rc<RefCell<Environment>>>,
    variables: HashMap<String, InterpreterValue>,
}

impl Environment {
    pub fn define(&mut self, name: String, value: InterpreterValue) {
        self.variables.insert(name, value);
    }

    pub fn set(&mut self, name: String, value: InterpreterValue) {
        if let Some(environment) = self.parent_env() {
            environment.as_ref().borrow_mut().define(name, value);
        } else {
            self.variables.insert(name, value);
        }
    }

    pub fn get(&self, name: &str) -> InterpreterResult<InterpreterValue> {
        let value = if let Some(value) = self.variables.get(name) {
            value.clone()
        } else if let Some(parent_env) = self.parent_env() {
            parent_env.as_ref().borrow().get(name)?
        } else {
            return Err(InterpreterError::Generic(format!(
                "Undefined variable '{name}'"
            )));
        };

        Ok(value)
    }

    pub fn parent_env(&self) -> Option<Rc<RefCell<Environment>>> {
        self.parent_env.clone()
    }

    pub fn new(parent_env: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            parent_env: parent_env.clone(),
            variables: HashMap::new(),
        }
    }
}
