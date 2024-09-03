use std::{cell::RefCell, collections::HashMap, rc::Rc};

use super::Object;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Environment {
    pub store: HashMap<String, Object>,
    pub outer: Option<Rc<RefCell<Environment>>>,
}

impl Environment {
    pub fn new(outer: Option<Rc<RefCell<Environment>>>) -> Self {
        Self {
            store: HashMap::new(),
            outer,
        }
    }

    pub fn get(&self, name: &String) -> Option<Object> {
        let got = self.store.get(name);

        match got {
            Some(object) => Some(object.clone()),
            None => {
                if let Some(outer) = &self.outer {
                    outer.borrow().get(name)
                } else {
                    None
                }
            }
        }
    }

    pub fn set(&mut self, name: String, value: Object) -> Object {
        self.store.insert(name.clone(), value);

        self.store.get(&name).unwrap().clone()
    }
}

#[cfg(test)]
mod test {
    use std::{cell::RefCell, rc::Rc};

    use super::{ Environment, super::{ TRUE, FALSE } };

    #[test]
    fn test_new_scope() {
        let env = Rc::new(RefCell::new(Environment::new(None)));

        env.borrow_mut().set(String::from("x"), TRUE);

        let env_with_outer = Environment::new(Some(env.clone()));

        env.borrow_mut().set(String::from("x"), FALSE);

        assert_eq!(env.borrow().get(&String::from("x")), env_with_outer.get(&String::from("x")));

        println!("{:?}", env);
    }
}